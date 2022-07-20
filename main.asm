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
        DIGITS db "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

;; static data stuff
SECTION .data
align 8
        helloStr db "RayForth v0", 10
        helloLen equ $-helloStr

        keytestStr db "Press a key and it will be printed back", 10
        keytestStrLen equ $-keytestStr

        notFoundMsgStr db " not found"
        notFoundMsgLen equ $-notFoundMsgStr

        promptStr db " ok", 10
        promptLen equ $-promptStr

        ; pop r8
        ; sub rbp, 8
        ; mov [rbp], r8
        ; 41 58 48 83 ed 08 4c 89 45 00
        doesPrelude db 0x41, 0x58, 0x48, 0x83, 0xED, 0x08, 0x4C, 0x89, 0x45, 0x0
        doesPreludeLen equ $-doesPrelude

        ; mov rax, 0x1122334455667788
        ; sub rbp, 8
        ; mov [rbp], rax
        literalCode db 0x48, 0xb8
        literalCodeAddressOffset equ $-literalCode
        db 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11
        db 0x48, 0x83,  0xed, 0x08, 0x48, 0x89, 0x45, 0x00
        literalCodeLen equ $-literalCode

;; here's where these things go, apparently
SECTION .bss
align 8

;; Parameter stack
        DATASTACK resb CELLSIZE*STACKSIZE
        DATASTACKBOTTOM equ $

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
%define IMM 0x80

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
%{2}:
        mov W, val_ %+ %2
        mov X, [W]
        DPUSH X
        ret
        val_ %+ %2 dq %3        ; value stored here
%endmacro

%macro .variable 3
        head %1,%2,0
%{2}:
        DPUSH val_ %+ %2
        ret
        val_ %+ %2 dq %3        ; value stored here
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
        mov r8, PSP
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

.colon "-", minus
        DPOP r8
        DPOP r9
        sub r9, r8
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
        mov r8, [PSP]
        DPUSH r8
        ret

.colon "SWAP", swap
        DPOP r8
        DPOP r9
        DPUSH r8
        DPUSH r9
        ret

.colon "DROP", drop
        add PSP, CELLSIZE
        ret

.colon "OVER", over
        mov r8, [PSP+CELLSIZE]
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
        DPOP W
        xor rbx, rbx
        mov bl, [W]            ; hmmm... we do want to read a single char here
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

.variable "BASE", base, 10      ; base is 10 by default
.variable "STATE", state, 0     ; 0 interpret, 1 compile
.variable "LATEST", latest, 0
.variable "TIB", TIB, TIBDATA
.variable ">IN", TIBIN, 0

.variable "DP", dp, 0
.colon "HERE", here
        mov r8, [val_dp]
        DPUSH r8
        ret

.colon "REFILL", refill
        mov rdi, TIBDATA
        mov rcx, BUFFERSIZE
        mov al, ' '
        rep stosb

        mov rax, 3
        mov rbx, 1
        mov rcx, TIBDATA
        mov rdx, BUFFERSIZE
        int 0x80

        ; rax holds size or -errno
        test rax, rax
        js refill_error

        ; no error, so let's patch that possible 0x0A...
        cmp rax, BUFFERSIZE
        je refill_dont_patch

        add rax, TIBDATA
        dec rax
        mov [rax], byte ' '

refill_dont_patch:
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
        ; store the address of the link on Y
        call latest
        call fetch
        DPOP Y

find_setup:
        ; load rsi and rdi
        mov rsi, r10
        mov rdi, Y

        ; first move over the link, to the count+name
        add rdi, CELLSIZE

        ; compare the string lengths
find_check_lengths:
        xor rbx, rbx
        xor rcx, rcx
        mov bl, [rsi]
        mov cl, [rdi]
        ; store the immediate flag and the count separately
        mov dl, cl
        and dl, IMM
        xor cl, dl
        cmp bl, cl

        je find_check_names

        ; if not the same, next word
        ; follow the link
        mov Y, [Y]
        mov rdi, Y

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
        mov Y, [Y]
        mov rdi, Y

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
        test dl, dl
        jz find_return_non_immediate

find_return_immediate:
        DPUSH 1
        ret

find_return_non_immediate:
        DPUSH -1
        ret

.colon "'", tick                ; ( c"" -- xt )
        call bl_
        call word_
        call find
        call drop               ; nope, bad
        ret

.colon "EXECUTE", execute       ; ( xt -- )
        DPOP W
        call W                  ; what about a jump...?
        ret

.colon ";", semicolon, IMM
        mov rsi, [val_dp]
        mov byte [rsi], 0xC3
        inc rsi
        mov rdi, val_dp
        mov [rdi], rsi

        DPUSH 0
        call state
        call store
        ret

;; Forth 2012 says this ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
;; For now we will have ( n1 c-addr1 u1 -- n2 c-addr2 u2 ) ?
.colon ">NUMBER", tonumber
        DPOP X
        DPOP W

        ; bail out early if there are no digits to check
        test X, X
        jz tonumber_done

        ; always save BASE under TOS (the accumulator at this point)
        call base
        call fetch
        call swap

        xor r10, r10            ; clear the 64-bit register to load 8 bits

        ; check for sign and prefixes first!!!!
        mov r10b, [W]
        ; is it '? character, read until next '
        ; maybe we ignore this one for now...

        ; is it $? hex
        cmp r10, '$'
        je tonumber_hex

        ; is it #? decimal
        cmp r10, '#'
        je tonumber_decimal

        ; is it %? binary
        cmp r10, '%'
        je tonumber_binary

        ; the number is in the current base
        jmp tonumber_begin

tonumber_hex:
        DPUSH 16
        call base
        call store
        inc W
        dec X
        jmp tonumber_begin

tonumber_decimal:
        DPUSH 10
        call base
        call store
        inc W
        dec X
        jmp tonumber_begin

tonumber_binary:
        DPUSH 2
        call base
        call store
        inc W
        dec X
        jmp tonumber_begin

        ; and for the last three...
        ; is it -? negative

tonumber_begin:
        mov r10b, [W]
        mov rdx, 1
        ; check if it's a negative
        cmp r10, '-'
        je tonumber_negative
        jmp tonumber_one_digit

tonumber_negative:
        mov rdx, -1
        inc W
        dec X

        ; add each digit until we run out of (valid) digits
tonumber_one_digit:
        mov r10b, [W]

        ; is Y between '0' and '9'?
        cmp r10, '0'
        jl tonumber_done

        cmp r10, '9'
        jg tonumber_maybe_letter

        ; we have a digit, convert
        sub r10, '0'

        jmp tonumber_validate_digit

tonumber_maybe_letter:
        ; is Y between '0' and '9'?
        cmp r10, 'A'
        jl tonumber_done

        cmp r10, 'Z'
        jg tonumber_done

        ; we have a digit, convert
        sub r10, 'A'-10

        jmp tonumber_validate_digit

tonumber_validate_digit:
        ; is the value under BASE? if not, it's an invalid digit
        call base
        call fetch
        DPOP Y
        cmp r10, Y
        jge tonumber_done

tonumber_convert:
        ; first multiply n1 by BASE
        call base
        call fetch
        call multiply

        ; then add to the running total
        DPUSH r10
        call plus

tonumber_next_digit:
        inc W
        dec X
        jnz tonumber_one_digit

tonumber_done:
        call swap
        call base
        call store
        DPUSH rdx
        call multiply
        DPUSH W
        DPUSH X
        ret


.colon "INTERPRET", interpret
interpret_next_word:
        call bl_
        call word_

        ; finished if there are no more words left (WORD returns "")
        call dup
        call cfetch
        DPOP W
        test W, W
        jz interpret_end

        call find
        DPOP W
        test W, W

        ; if not found, it may be a number
        jz interpret_maybe_number

        ; if found, execute or compile it

        ; check immediacy
        ; W = 1   immediate
        ; W = -1  non-immediate
        test W, W
        jns interpreting_or_immediate

        ; check state
        call state
        call fetch
        DPOP X
        test X, X
        jz interpreting_or_immediate

interpret_compiling:
        DPOP W
        ; compile a near relative call, target address is in W
        call dp
        call fetch
        DPOP rdi
        mov byte [rdi], 0xE8

        ; obtain a 32 bit number to work with 32 bit signed
        call dp
        call fetch
        mov r13d, [PSP]
        call drop

        sub r12d, r13d       ; this is W as a dword
        sub r12d, 5          ; additional offset from next instruction
        mov [rdi+1], r12d    ; this is W as (now negative) dword

        ; update here
        call dp
        call fetch
        DPUSH 5
        call plus
        call dp
        call store

        jmp interpret_next_word

interpreting_or_immediate:
        ; interpreting or it's an immediate, execute it
        DPOP W
        call W
        jmp interpret_next_word

interpret_maybe_number:
        ; keep the original address on the stack?
        call dup

        ; we swipe the 0 first because I haven't written ROT and -ROT yet
        DPUSH 0
        call swap
        ; we can thrash the address copy
        call count
        ; >number takes (0 address count)
        call tonumber

        ; if we got any characters left, it's not a number
        DPOP W
        test W, W
        jnz interpret_not_found

        ; if there are no chars left, we can drop the address
        call drop
        ; we can also drop the original address
        call swap
        call drop

        ; IF WE'RE COMPILING WE MUST COMPILE THE NUMBER(!!!!!!)
        ; but if interpreting, we're done
        mov rcx, [val_state]
        test rcx, rcx
        jz interpret_next_word

        ; compile a call to LIT
        ; compute the relative address from here to LIT
        mov W, lit
        ; compile a near relative call, target address is in W
        call dp
        call fetch
        DPOP rdi
        mov byte [rdi], 0xE8

        ; obtain a 32 bit number to work with 32 bit signed
        call dp
        call fetch
        mov r13d, [PSP]
        call drop

        sub r12d, r13d       ; this is W as a dword
        sub r12d, 5          ; additional offset from next instruction
        mov [rdi+1], r12d    ; this is W as (now negative) dword

        ; compile the number immediately after
        DPOP r8
        mov qword [rdi+5], r8

        ; update here
        call dp
        call fetch
        DPUSH 5+CELLSIZE
        call plus
        call dp
        call store


        ; and we're left with the number, move along to the next word
        jmp interpret_next_word

interpret_not_found:
        call drop
        call drop
        call count
        call type
        DPUSH notFoundMsgStr
        DPUSH notFoundMsgLen
        call type
        call cr

        ; should clear the stack pointers... or is that QUIT's job?
        ;jmp interpret_end
        ret

interpret_end:
        call drop
        ret

;; temporary test word
.colon "NUM",num
        DPUSH 789
        ret

.colon "0",zero
        DPUSH 0
        ret

.colon "TWO",two
        DPUSH 2
        ret

.colon "TEN",ten
        DPUSH 10
        ret

.colon "SIXTEEN",sixteen
        DPUSH 16
        ret

.colon ".", period              ; ( n -- )
        ; if 0, just print 0 and exit
        DPOP rax
        test rax, rax
        jz period_zero

        ; display negatives somehow... only if BASE is 10
        cmp qword [val_base], 10
        jne period_begin_process

        ; print a '-' only if it's a negative
        test rax, rax
        jns period_begin_process

        xchg rax, r8
        DPUSH '-'
        call emit
        xchg rax, r8
        ; then negate then number and print it normally
        neg rax

period_begin_process:
        xor r8, r8

period_process_digit:
        xor rdx, rdx
        mov rbx, [val_base]
        div rbx
        add rdx, DIGITS            ; make a letter
        mov rdx, [rdx]
        DPUSH byte rdx
        inc r8
        test rax, rax
        jnz period_process_digit

period_emit_digit:
        ; no more digits, print them back from the stack
        call emit
        dec r8
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
        call drop               ; should do something with this flag(!)

        call interpret
        call quit_prompt
        jmp quit

quit_prompt:
        DPUSH promptStr
        DPUSH promptLen
        call type
        ret

.colon "ALLOT", allot           ; ( n -- )
        ; should clear the space too?
        call dp
        call fetch
        call plus
        call dp
        call store
        ret


.colon "[", leftbracket, IMM
        DPUSH 0
        call state
        call store
        ret

.colon "]", rightbracket, IMM
        DPUSH 1
        call state
        call store
        ret

.colon ",", comma
        call here
        call store
        call here
        DPUSH 8
        call plus
        call dp
        call store
        ret

.colon "C,", c_comma
        call here
        call cstore
        call here
        DPUSH 1
        call plus
        call dp
        call store
        ret

.colon ":", colon
        ; make an entry at HERE
        ; first store the address on LATEST at HERE
        call latest
        call fetch
        call dp
        call fetch
        call store

        call dp               ; store here at latest
        call fetch
        call dup
        call latest
        call store

        DPUSH CELLSIZE          ; update here
        call plus
        call dp
        call store

        call bl_                ; get the word name
        call word_              ; which will be on WORDBUFFER as a c-string

        ; then flags+count followed by the name
        call dup
        call cfetch
        DPUSH 1
        call plus
        DPOP rcx                ; we will copy count+1 bytes
        DPOP rsi
        DPUSH rcx               ; let's keep the size on the stack
        call dp
        call fetch
        DPOP rdi
        rep movsb

        ; update here (we left the count on the stack)
        call dp
        call fetch
        call plus               ; add HERE to the size we kept on the stack
        call dp
        call store

        ; finally switch to compile mode
        DPUSH 1
        call state
        call store

        ret


;; piece of code called for a CREATEd word
;; push into datastack the address immediately after the call
;; and return to the caller of the caller
created:
        pop r8
        DPUSH r8
        ret

;; piece of code called for a compiled literal
;; push into datastack the 64 bit number immediately after the call
;; and return to the address after it
lit:
        pop r8
        mov r9, [r8]
        DPUSH r9
        add r8, CELLSIZE
        push r8
        ret


.colon "CREATE", create
        ; make an entry at HERE
        ; first store the address on LATEST at HERE
        call latest
        call fetch
        call dp
        call fetch
        call store

        call dp               ; store here at latest
        call fetch
        call dup
        call latest
        call store

        DPUSH CELLSIZE          ; update here
        call plus
        call dp
        call store

        call bl_                ; get the word name
        call word_              ; which will be on WORDBUFFER as a c-string

        ; then flags+count followed by the name
        call dup
        call cfetch
        DPUSH 1
        call plus
        DPOP rcx                ; we will copy count+1 bytes
        DPOP rsi
        DPUSH rcx               ; let's keep the size on the stack
        call dp
        call fetch
        DPOP rdi
        rep movsb

        ; update here (we left the count on the stack)
        call dp
        call fetch
        call plus               ; add HERE to the size we kept on the stack
        call dp
        call store

        ; at runtime, move address of created to 64 bit register (rsi)
        ; compile 48 BE followed by 8 bytes of address
        call dp
        call fetch
        DPOP rdi
        mov [rdi], byte 0x48
        mov [rdi+1], byte 0xBE
        mov qword [rdi+2], created

        ; at runtime,  call indirect with the register (rsi)
        ; compile FF D6
        mov [rdi+10], byte 0xFF
        mov [rdi+11], byte 0xD6

        ; move HERE 12 bytes forward
        DPUSH 2+8+2
        call dp
        call fetch
        call plus
        call dp
        call store

        ret

.colon ">BODY", body            ; ( xt -- a-addr )
        DPUSH 12
        call plus
        ret

.colon "DOES>", does
        ; get LATEST, skip over link and name, go to code
        call latest
        call fetch
        DPUSH CELLSIZE
        call plus               ; skip over the link
        call dup
        call cfetch
        DPUSH 1
        call plus
        call plus   ; skipped over the name

        ; overwrite the call to CREATED with a call to HERE
        ; at runtime, move address of current HERE to 64 bit register (rsi)
        DPUSH 2
        call plus
        DPOP rdi
        call dp
        call fetch
        DPOP rsi
        mov qword [rdi], rsi

        ; compile code on HERE which:
        ; pops top-of-return-stack and pushes to datastack
        ; source code to compile to get the bytes...:
        mov rsi, doesPrelude
        mov rcx, doesPreludeLen
        call dp
        call fetch
        DPOP rdi
        rep movsb

        ; adjust HERE
        call dp
        call fetch
        DPUSH doesPreludeLen
        call plus
        call dp
        call store

        ; then the rest of code will be compiled as usual
        ret


end_of_builtins:
;; should I add a blob of uninitialised (or initialised) space here?

        resb 16384

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

        mov rsi, val_dp
        mov qword [rsi], end_of_builtins
        mov rsi, val_latest
        mov qword [rsi], does_entry ; THIS HAS TO BE MANUALLY UPDATED...(!)

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
        ;call display
        jmp coda


;;;; THIS IS THE EXIT POINT
coda:
        call bye

;; %include "testcode.asm"
