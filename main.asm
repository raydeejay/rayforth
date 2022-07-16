;; this is a forth written in assembly... or at least it tries to be
;; (C) 2022 Sergi Reyner
;; MIT License

bits 64

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

;; %define IP rip
;; %define RSP rsp
%define PSP rbp
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

;; Constants
        CELLSIZE equ 8
        STACKSIZE equ 64
        BUFFERSIZE equ 4096

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
        DATASTACK resb CELLSIZE*STACKSIZE
        DATASTACKBOTTOM equ $ - CELLSIZE

;; Parameter Stack Macros
%macro DPUSH 1
        sub PSP, CELLSIZE
        mov qword [PSP], qword %1
%endmacro

%macro DPOP 1
        mov qword %1, qword [PSP]
        add PSP, CELLSIZE
%endmacro


;; Other memory zones
PADDATA:
        resb BUFFERSIZE
TIBDATA:
        resb BUFFERSIZE
WORDBUFFER:
        resb BUFFERSIZE

;; dictionary here?
;; colon and code definitions have the same structure
;; LINK   FLAGS|COUNT   NAME   code here...

;; Here we create some macros for easy creation of dictionary entries,
;; along with labels than can be used later to call code or address
;; data directly from assembly
        
;; inspired by Itsy Forth, modified for STC and additional
;; code. .CONSTANT and .VARIABLE compile their own code straight
;; away. .CODE is gone, since all definitions are the same under STC,
;; but may come back later if I find some utility to having two
;; different words.
        
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
        mov W, val_ %+ %2
        mov X, [W]
        DPUSH X
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
        DPOP W
        mov qword X, qword [W]
        DPUSH X
        ret

.colon "!",store
        DPOP W
        DPOP X
        mov qword [W], qword X
        ret

.colon "SP@", spFetch
        mov W, PSP
        DPUSH W
        ret

.colon "RP@", rpFetch
        mov W, rsp
        DPUSH W
        ret

.colon "0=", zeroEqual
        DPOP W
        test W, W
        pushf
        pop W
        shr W, 6+8
        and W, 1
        mov X, 0
        sub X, W
        DPUSH X
        ret

.colon "+", plus
        DPOP W
        DPOP X
        add W, X
        DPUSH W
        ret

.colon "NAND", nand
        DPOP W
        DPOP X
        and W, X
        not W
        DPUSH W
        ret

.colon "EXIT", exit
        pop W
        pop X
        push W
        ret

;; ideally we should set the terminal to raw or something first
.colon "KEY", key
        sub PSP, 4
        mov rax, 3
        mov rbx, 1
        mov rcx, PSP
        mov rdx, 1
        int 0x80
        ret

.colon "EMIT", emit
        mov Y, PSP
        DPUSH Y
        DPUSH 1
        call type
        add PSP, CELLSIZE
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
        mov W, [PSP]
        DPUSH W
        ret

.colon "SWAP", swap
        DPOP W
        DPOP X
        DPUSH W
        DPUSH X
        ret

.colon "DROP", drop
        add PSP, CELLSIZE
        ret

.colon "OVER", over
        mov W, [PSP+CELLSIZE]
        DPUSH W
        ret

.colon "*", multiply
        DPOP W
        DPOP X
        imul W, X
        DPUSH W
        ret

.colon "CR", cr
        DPUSH 10
        call emit
        ret

.colon "C@", cfetch
        DPOP W
        xor rbx, rbx
        mov bl, [W]            ; hmmm... we do want to read a single char here
        DPUSH rbx
        ret

.colon "C!", cstore
        DPOP W
        DPOP rbx                ; same here
        mov [W], bl
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
        mov rdx, BUFFERSIZE
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
        mov rdi, PSP

word_skip_delimiters:
        cmpsb

        ; if not equal we have a word
        jne skipped_delimiters

        ; if we went over the end of TIBDATA, we're done
        cmp rsi, TIBDATA+BUFFERSIZE
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
        cmp rsi, TIBDATA+BUFFERSIZE
        je found_closing_delimiter

        ; otherwise move along to the next char
        dec rdi                 ; RDI should always point to the same char
        jmp find_closing_delimiter

found_closing_delimiter:
        ; update >IN
        ; with the difference between end of parsing and TIBDATA
        mov W, rsi
        sub W, TIBDATA
        DPUSH W
        call TIBIN
        call store

        ; we have the end, calculate the length now
        ; rsi holds the end
        ; the word address is on the stack
        mov rcx, rsi
        DPOP W
        sub rcx, W            ; load the number of chars on RCX (end-start)
        dec rcx                 ; correct the off by one

        mov rsi, W
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
        mov r10, [PSP]
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
        DPOP W
        test W, W

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
        DPOP W
        call W
        ret

;; temporary test word
.colon "NUM",num
        DPUSH 789
        ret

.colon ".", period              ; ( n -- )
        DPOP rax
        test rax, rax
        jz period_zero

        xor W, W
period_process_digit:
        xor rdx, rdx
        mov rbx, 10
        div rbx
        add rdx, '0'            ; make a letter
        DPUSH rdx
        inc W
        test rax, rax
        jnz period_process_digit

period_emit_digit:
        ; no more digits, print them back from the stack
        call emit
        dec W
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
        DPOP W
        test W, W
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
        mov rcx, BUFFERSIZE
        mov al, ' '
        rep stosb

        mov rdi, TIBDATA
        mov rcx, BUFFERSIZE
        mov al, ' '
        rep stosb

        mov rdi, DATASTACK
        mov rcx, CELLSIZE*STACKSIZE
        mov al, 0
        rep stosb

        mov PSP, DATASTACKBOTTOM

        mov rsi, val_here
        mov qword [rsi], end_of_builtins
        mov rsi, val_latest
        mov qword [rsi], quit_entry

        ret


;; --- more code ---
;; function things
display:
        DPUSH PADDATA
        DPUSH BUFFERSIZE
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

;; %include "testcode.asm"
