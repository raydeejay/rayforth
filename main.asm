;; this is a forth written in assembly... or at least it tries to be
;; (C) 2022 Sergi Reyner
;; MIT License

;; Register Allocation

;; The System V ABI has just enough registers that we can avoid using
;; rax-rdx, which are the means to pass parameters to linux syscalls

;; All registers are equally capable (for the most part...)

;; Since this is an STC Forth, the IP register is also the IP register
;; of the machine, namely rip

;; The parameter stack pointer will reside on rbp, which is the stack
;; frame pointer and not really relevant as long as we stay on the
;; Forth and assembly side of code

;; rsi and rdi are used as source and destination pointers for a few
;; instructions

;; r8 r9 r10 r11 are scratch registers
;; r12 r13 r14 r15 are preserved between calls

;; There's not really a need to respect the System V ABI, but in the
;; future we may want to interface with C code. Since there's not
;; really a cost to this future-proofing, because we have enough
;; preserved registers, I ended up with the following allocation,
;; assigned following the recommendations from Moving Forth:

;; %define RSP rsp
%define PSP rbp
%define IP rip
%define TOS r15
%define W r12
%define X r13
%define Y r11                   ; it's going to be a scratch register anyway
%define UP r14

;; Possibly useful for compiling literals?
;; x64 provides a new rip-relative addressing mode. Instructions that
;; refer to a single constant address are encoded as offsets from
;; rip. For example, the mov rax, [addr] instruction moves 8 bytes
;; beginning at addr + rip to rax.

bits 64

;; Constants
        CELLSIZE equ 8
        STACKSIZE equ 64

;; static data stuff
SECTION .data
align 8
        helloStr db "RayForth v0", 10
        helloLen equ $ -helloStr

        keytestStr db "Press a key and it will be printed back", 10
        keytestStrLen equ $ -keytestStr

        notFoundMsgStr db " not found"
        notFoundMsgLen equ $ -notFoundMsgStr


;; here's where these things go, apparently
SECTION .bss
align 8

;; Parameter stack
        DATASTACK resb CELLSIZE*64
        DATASTACKBOTTOM equ $ - CELLSIZE

;; Parameter Stack Macros
%macro DPUSH 1
        sub rbp, CELLSIZE
        mov qword [rbp], qword %1
%endmacro

%macro DPOP 1
        mov qword %1, qword [rbp]
        add rbp, CELLSIZE
%endmacro


;; Other memory zones
PADDATA:
        resb 4096
TIBDATA:
        resb 4096
WORDBUFFER:
        resb 4096

;; dictionary here?
;; colon and code definitions have the same structure
;; LINK   FLAGS|COUNT   NAME   code here...

;; inspired by Itsy Forth
%define link 0
%define IMMEDIATE 0x80

%macro head 3
%{2}_entry:
        %%link dq link
%define link %%link
%strlen %%count %1
        db %3 + %%count,%1
%endmacro

%macro .colon 2-3 0
        head %1,%2,%3
%{2}:
%endmacro

%macro .constant 3
        head %1,%2,0
        val_ %+ %2 dq %3
%{2}:
        mov r8, val_ %+ %2
        mov r9, [r8]
        DPUSH r9
        ret
%endmacro

%macro .variable 3
        head %1,%2,0
        val_ %+ %2 dq %3
%{2}:
        DPUSH val_ %+ %2
        ret
%endmacro


SECTION mysection,EWR
DICTIONARY:
;; primitives
.colon "@",fetch
        DPOP r8
        mov qword r9, qword [r8]
        DPUSH r9
        ret

.colon "!",store
        DPOP r8
        DPOP r9
        mov qword [r8], qword r9
        ret

.colon "SP@", spFetch
        mov r8, rbp
        DPUSH r8
        ret

.colon "RP@", rpFetch
        mov r8, rsp
        DPUSH r8
        ret

.colon "0=", zeroEqual
        DPOP r8
        test r8, r8
        pushf
        pop r8
        shr r8, 6+8
        and r8, 1
        mov r9, 0
        sub r9, r8
        DPUSH r9
        ret

.colon "+", plus
        DPOP r8
        DPOP r9
        add r8, r9
        DPUSH r8
        ret

.colon "NAND", nand
        DPOP r8
        DPOP r9
        and r8, r9
        not r8
        DPUSH r8
        ret

.colon "EXIT", exit
        pop r8
        pop r9
        push r8
        ret

;; ideally we should set the terminal to raw or something first
.colon "KEY", key
        sub rbp, 4
        mov rax, 3
        mov rbx, 1
        mov rcx, rbp
        mov rdx, 1
        int 0x80
        ret

.colon "EMIT", emit
        mov r8, rbp
        DPUSH r8
        DPUSH 1
        call type
        add rbp, CELLSIZE
        ret

;; end of SectorForth primitives, start of mine

;; TYPE
.colon "TYPE", type
        mov rax, 4
        mov rbx, 1
        DPOP rdx
        DPOP rcx
        int 0x80
        ret

.colon "SQUARE", square
        call dup
        call multiply
        ret

.colon "DUP", dup
        mov r8, [rbp]
        DPUSH r8
        ret

.colon "SWAP", swap
        DPOP r8
        DPOP r9
        DPUSH r8
        DPUSH r9
        ret

.colon "DROP", drop
        add rbp, CELLSIZE
        ret

.colon "OVER", over
        mov r8, [ebp+CELLSIZE]
        DPUSH r8
        ret

.colon "*", multiply
        DPOP r8
        DPOP r9
        imul r8, r9
        DPUSH r8
        ret

.colon "CR", cr
        DPUSH 10
        call emit
        ret

.colon "C@", cfetch
        DPOP r8
        xor rbx, rbx
        mov bl, [r8]            ; hmmm... we do want to read a single char here
        DPUSH rbx
        ret

.colon "C!", cstore
        DPOP r8
        DPOP rbx                ; same here
        mov [r8], bl
        ret

.colon "BYE", bye
        mov rax, 1
        mov rbx, 0
        int 0x80

.variable "STATE", state, 76
.variable "HERE", here, 77
.variable "LATEST", latest, 78
.variable "TIB", TIB, TIBDATA
.variable ">IN", TIBIN, 0

.colon "REFILL", refill
        mov rax, 3
        mov rbx, 1
        mov rcx, TIBDATA
        mov rdx, 4096
        int 0x80

        ; rax holds size or -errno
        test rax, rax
        js refill_error

        ; no error, reset >IN and return true
        DPUSH 0
        call TIBIN
        call store
        DPUSH -1
        ret

refill_error:
        DPUSH 0                 ; return false
        ret

;; Outer interpreter stuff
.constant "BL", bl_, ' '

.colon "COUNT", count
        call dup
        call cfetch
        call swap
        DPUSH 1
        call plus
        call swap
        ret

.colon "WORD", word_
        call TIB
        call fetch
        call TIBIN
        call fetch
        call plus

        ; compare char with delimiter
        ; the address of the potential word is on TOS
        DPOP rsi
        ; the delimiter is on TOS now, we'll just point rdi to it
        mov rdi, rbp

word_skip_delimiters:
        cmpsb

        ; if not equal we have a word
        jne skipped_delimiters

        ; if we went over the end of TIBDATA, we're done
        cmp rsi, TIBDATA+4096
        je tibdata_was_empty

        ; otherwise move to the next char and repeat
        dec rdi                 ; RDI should always point to the same char
        jmp word_skip_delimiters

tibdata_was_empty:
        mov rsi, WORDBUFFER
        mov qword [rsi], qword 0

        ; clean up
        call drop
        DPUSH WORDBUFFER

        ret

skipped_delimiters:
        ; readjust the pointers back 1 char
        dec rsi
        dec rdi
        DPUSH rsi               ; save the beginning of the word

find_closing_delimiter:
        ; we have a word, find the end
        ; the word address is on rsi (and on the stack)
        ; the delimiter is on rdi (and on the stack under TOS)

        ; compare char with delimiter
        cmpsb

        ; if we find delimiter, we are past the word
        je found_closing_delimiter

        ; if we went over the end of TIBDATA, return right away
        cmp rsi, TIBDATA+4096
        je found_closing_delimiter

        ; otherwise move along to the next char
        dec rdi                 ; RDI should always point to the same char
        jmp find_closing_delimiter

found_closing_delimiter:
        ; update >IN
        ; with the difference between end of parsing and TIBDATA
        mov rcx, rsi
        sub rcx, TIBDATA
        DPUSH rcx
        call TIBIN
        call store

        ; we have the end, calculate the length now
        ; rsi holds the end
        ; the word address is on the stack
        mov rcx, rsi
        DPOP rax
        sub rcx, rax            ; load the number of chars on RCX (end-start)
        dec rcx                 ; correct the off by one

        mov rsi, rax
        mov rdi, WORDBUFFER
        mov [rdi], cl           ; put the count

        ; put the string
        inc rdi
        rep movsb

        ; clear the delimiter off the stack
        call drop

        ; return the address of the parsed word
        DPUSH WORDBUFFER
        ret

.colon "FIND", find             ; ( c-addr -- c-addr 0 | xt 1 | xt -1 )
        ; store the address of the source string on r10
        mov r10, [rbp]
        ; store the address of the link on r11
        call latest
        call fetch
        DPOP r11

find_setup:
        ; load rsi and rdi
        mov rsi, r10
        mov rdi, r11

        ; first move over the link, to the count+name
        add rdi, CELLSIZE

        ; compare the string lengths
find_check_lengths:
        xor rbx, rbx
        xor rcx, rcx
        mov bl, [rsi]
        mov cl, [rdi]
        cmp bl, cl

        je find_check_names

        ; if not the same, next word
        ; follow the link
        mov r11, [r11]
        mov rdi, r11

        ; if the link is 0, not found
        test rdi, rdi
        jz find_not_found

        ; otherwise repeat the process
        jmp find_setup

find_check_names:
        ; count is still loaded on bl and cl
        ; advance the pointers over the counts
        inc rsi
        inc rdi

        ; compare strings, count is already loaded in cl/rcx
        repe cmpsb

        ; if they're equal we found a word
        je find_word_found

        ; if they're different move to the next link
        mov r11, [r11]
        mov rdi, r11

        ; if the link is 0, not found
        test rdi, rdi
        jz find_not_found

        ; else check the next entry
        jmp find_setup

find_not_found:
        ; return c-addr and 0
        DPUSH 0
        ret

find_word_found:
        ; drop c-addr
        call drop
        ; push xt (code address)
        DPUSH rdi
        ; push either 1 (imm) or -1 (non-imm)
        DPUSH -1
        ret


.colon "INTERPRET", interpret
        call find
        DPOP r10
        test r10, r10

        ; if found, execute it
        jnz interpret_execute

        ; if not found, complain
        call count
        call type
        DPUSH notFoundMsgStr
        DPUSH notFoundMsgLen
        call type
        call cr
        ret

interpret_execute:
        DPOP r10
        call r10
        ret

;; temporary test word
.colon "NUM",num
        DPUSH 789
        ret

.colon ".", period              ; ( n -- )
        DPOP rax
        test rax, rax
        jz period_zero

        xor r10, r10
period_process_digit:
        xor rdx, rdx
        mov rbx, 10
        div rbx
        add rdx, '0'            ; make a letter
        DPUSH rdx
        inc r10
        test rax, rax
        jnz period_process_digit

period_emit_digit:
        ; no more digits, print them back from the stack
        call emit
        dec r10
        jnz period_emit_digit

        jmp period_done

period_zero:
        DPUSH '0'
        call emit

period_done:
        DPUSH ' '
        call emit
        ret


.colon "QUIT", quit
        ; interpret some words from TIB separated by spaces(!)
        call refill
quit_again:
        call bl_
        call word_

        ; exit if there are no more words left (WORD returns "")
        call dup
        call cfetch
        DPOP rax
        test rax, rax
        jz endquit

        call interpret
        jmp quit_again

endquit:
        call drop
        ret

end_of_builtins:
;; should I add a blob of uninitialised (or initialised) space here?

;; the program code here
SECTION .text
align 8

global _start

;; perform various initialization stuff
init:
        mov rdi, PADDATA
        mov rcx, 4096
        mov al, ' '
        rep stosb

        mov rdi, TIBDATA
        mov rcx, 4096
        mov al, ' '
        rep stosb

        mov rdi, DATASTACK
        mov rcx, CELLSIZE*STACKSIZE
        mov al, 0
        rep stosb

        mov rbp, DATASTACKBOTTOM

        mov rsi, val_here
        mov qword [rsi], end_of_builtins
        mov rsi, val_latest
        mov qword [rsi], quit_entry

        ret


;; --- more code ---
;; function things
display:
        DPUSH PADDATA
        DPUSH 4096
        call type
        call cr
        ret

hello:
        DPUSH helloStr
        DPUSH helloLen
        call type
        ret

;; Inner interpreter stuff
;; do we even have one? :-/
;; answer is nope, with the magic of STC

;;;; THIS IS THE ENTRY POINT
_start:
        call init
        call hello
        ;call testword
        ;call testword2
        call quit
        call display
        jmp coda


;;;; THIS IS THE EXIT POINT
coda:
        call bye

testexitinner:
        DPUSH 'A'
        call emit
        call exit               ; exits from outer
        ret

testexitouter:
        call testexitinner
        DPUSH 'B'
        call emit
        ret

testword:
        ; test stack and emit, emit EDC
        DPUSH 'C'
        DPUSH 'D'
        DPUSH 'E'
        call emit
        call emit
        call emit
        call cr

        ; test fetch and store, copy 8 bytes of hello string (offset 4)
        ; to the pad ("orth V0", 10)
        DPUSH helloStr+4
        call fetch
        DPUSH PADDATA
        call store

        ; test 0=, emit 0 and 1
        DPUSH 0
        call zeroEqual
        DPOP rax
        add rax, '1'
        DPUSH rax
        call emit

        DPUSH 1
        call zeroEqual
        DPOP rax
        add rax, '1'
        DPUSH rax
        call emit

        ; test +, emit A
        DPUSH 60
        DPUSH 5
        call plus
        call emit

        ; test NAND, emit O
        DPUSH 0xffffffffffffffff
        DPUSH 0xffffffffffffffb0
        call nand
        call emit

        ; test exit, emit A but not B
        call testexitouter

        ; test for KEY
        DPUSH keytestStr
        DPUSH keytestStrLen
        call type
        call key
        call emit

        ; test SQUARE colon definition (but not the whole entry)
        ; also tests * and DUP code definitions
        ; emit C
        DPUSH 8
        call square
        DPUSH 3
        call plus
        call emit

        ; test HERE as a variable, emit E
        call here
        call fetch              ; save the value of here on the stack

        DPUSH '*'
        call emit

        DPUSH 69
        call here
        call store
        call here
        call fetch
        call emit

        call here
        call store              ; and restore the value of here
        call cr

        ; test dictionary structure
        DPUSH here_entry
        DPUSH CELLSIZE
        call plus
        call cfetch
        DPUSH '0'
        call plus
        call emit

        DPUSH ' '
        call emit

        DPUSH here_entry
        DPUSH CELLSIZE
        call plus
        DPUSH 1
        call plus
        DPUSH 4
        call type
        call cr

        ; more test for dictionary structure
        ; print the name of the word preceeding HERE
        DPUSH here_entry
        call fetch              ; address of the linked entry

        DPUSH CELLSIZE
        call plus               ; address of the count

        call dup
        call cfetch             ; get the count and move it out of the way
        call swap

        DPUSH 1                 ; increment the address
        call plus

        call swap

        call type               ; print the name
        call cr


        ; test C@, emit F from Forth
        DPUSH helloStr
        DPUSH 3
        call plus
        call cfetch
        call emit

        ; test C!, emit X and modifies PADDATA too
        DPUSH 88
        DPUSH PADDATA
        call cstore
        DPUSH PADDATA
        call cfetch
        call emit

        call cr
        ret

testword2:
        ; test REFILL, BL, WORD, and COUNT
        call refill
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr

        ; end of tests
        call cr
        ret
