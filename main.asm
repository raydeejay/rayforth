;; this is a forth written in assembly... or at least it tries to be
;; (C) 2022 Sergi Reyner
;; MIT License

bits 64

;; design goals:
;; -------------

;; designed to run under Linux x64
;; the native cell size is 64-bit
;; must be able to interpret most words
;; must produce reasonably fast code
;; must be implemented in an easy to understand way
;; will attempt to follow the Forth2012 Standard (latest at the time of implementation)
;; should... provide a C interface


;; The Forth 2012 Standard is considered a suggestion/guideline

;; most of the core words can be implemented as macros (immediate
;; words) that compile small snippets of machine code, somewhat
;; similar to machineforth

;; should make a decision on whether to have dual sets of words
;; (DUP/DUP,) for interpretation/regular compiling vs inlining

;; how to implement J when having both FOR/NEXT and DO/LOOP? if at all...


;; once file access is implemented, determine what should be a
;; primitive and what should be a high level definition, then move the
;; high level code to boot.fs

;; things that are used frequently or interface with the OS:
;; --------------------------------
;; math words
;; stack words
;; syscalls


;; writing "fast" code in Forth is possible given a basic set of words
;; capable of assembling code


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
        TRUE equ -1
        FALSE equ 0
        MMAP_FLAGS equ 0x22 ; MMAP_ANONYMOUS|MMAP_PRIVATE
        MMAP_PROTECTION equ 0x7 ; RWE

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
        xchg TOS, [PSP]
%endmacro

%macro DPOP 1
        xchg TOS, [PSP]
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
.constant "TRUE", true, -1
.constant "FALSE", false, 0

.colon "EXIT", exit
        pop r8
        ret


.colon "@", fetch
        mov TOS, [TOS]
        ret

.colon "!",store
        mov r8, [PSP]
        mov [TOS], r8
        xchg TOS, [PSP+CELLSIZE]
        add PSP, CELLSIZE*2
        ret

.colon "C@", cfetch
        xor r8, r8
        xchg r8, TOS
        mov r15b, [r8]
        ret

.colon "C!", cstore
        mov r8, [PSP]
        mov byte [TOS], r8b
        xchg TOS, [PSP+CELLSIZE]
        sub PSP, CELLSIZE*2
        ret


.colon "RP@", rpFetch
        sub PSP, CELLSIZE
        xchg [PSP], TOS
        mov TOS, rsp            ; are we returning the right value here...? this is not inlined...
        add TOS, 8
        ret

.colon "R@", rfetch
        sub PSP, CELLSIZE
        xchg [PSP], TOS
        mov TOS, [rsp+8]
        ret

;; these words become shorter if code is inlined
.colon "R>", torstack
        pop r8
        pop r9
        push r8
        DPUSH r9
        ret

.colon ">R", fromrstack
        DPOP r8
        pop r9
        push r8
        push r9
        ret


.colon "0=", zeroEqual
        test TOS, TOS
        jz zeroEqualTrue
zeroEqualFalse:
        mov TOS, 0
        ret
zeroEqualTrue:
        mov TOS, -1
        ret

.colon "0<>", zeroNotEqual
        test TOS, TOS
        jnz zeroNotEqualTrue
zeroNotEqualFalse:
        mov TOS, 0
        ret
zeroNotEqualTrue:
        mov TOS, -1
        ret


.colon "=", equal
        cmp TOS, [PSP]
        je equalTrue
equalFalse:
        mov TOS, 0
        jmp equalDone
equalTrue:
        mov TOS, -1
equalDone:
        add PSP, CELLSIZE
        ret

.colon "<>", different
        cmp TOS, [PSP]
        jne differentTrue
differentFalse:
        mov TOS, 0
        jmp differentDone
differentTrue:
        mov TOS, -1
differentDone:
        add PSP, CELLSIZE
        ret


; .colon "<", lesserthan
;         xor r8, r8
;         xchg r8, TOS
;         cmp r8, [PSP]
;         jg lesserthan_yes
; lesserthan_done:
;         ret
; lesserthan_yes:
;         mov TOS, -1
;         jmp lesserthan_done

; .colon ">", greaterthan
;         xor r8, r8
;         xchg r8, TOS
;         cmp r8, [PSP]
;         jl greaterthan_yes
; greaterthan_done:
;         ret
; greaterthan_yes:
;         mov TOS, -1
;         jmp greaterthan_done


.colon "<", lesserthan
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        jg lesserthan_yes
lesserthan_done:
        ret
lesserthan_yes:
        mov TOS, -1
        jmp lesserthan_done

.colon ">", greaterthan
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        jl greaterthan_yes
greaterthan_done:
        ret
greaterthan_yes:
        mov TOS, -1
        jmp greaterthan_done


.colon "<=", lesserthanorequal
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        jge lesserthanorequal_yes
lesserthanorequal_done:
        ret
lesserthanorequal_yes:
        mov TOS, -1
        jmp lesserthanorequal_done

.colon ">=", greaterthanorequal
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        jle greaterthanorequal_yes
greaterthanorequal_done:
        ret
greaterthanorequal_yes:
        mov TOS, -1
        jmp greaterthanorequal_done


.colon "U<", ulesserthan
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        ja ulesserthan_yes
ulesserthan_done:
        ret
ulesserthan_yes:
        mov TOS, -1
        jmp ulesserthan_done

.colon "U>", ugreaterthan
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        jb ugreaterthan_yes
ugreaterthan_done:
        ret
ugreaterthan_yes:
        mov TOS, -1
        jmp ugreaterthan_done


.colon "U<=", ulesserthanorequal
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        jae ulesserthanorequal_yes
ulesserthanorequal_done:
        ret
ulesserthanorequal_yes:
        mov TOS, -1
        jmp ulesserthanorequal_done

.colon "U>=", ugreaterthanorequal
        xor r8, r8
        xchg r8, TOS
        cmp r8, [PSP]
        jbe ugreaterthanorequal_yes
ugreaterthanorequal_done:
        ret
ugreaterthanorequal_yes:
        mov TOS, -1
        jmp ugreaterthanorequal_done


.colon "+", plus
        add TOS, [PSP]
        add PSP, CELLSIZE
        ret

.colon "-", minus
        xchg TOS, [PSP]
        sub TOS, [PSP]
        add PSP, CELLSIZE
        ret

.colon "*", multiply            ; bit broken but works for reasonable numbers... xD
        imul TOS, [PSP]
        add PSP, CELLSIZE
        ret

;; Signed divide RDX:RAX by r/m64, with result stored in
;; RAX ← Quotient, RDX ← Remainder.
.colon "/MOD", dividemod
        xor rdx, rdx
        mov rax, [PSP]
        idiv TOS
        mov [PSP], rax
        mov TOS, rdx
        ret

.colon "/", divide
        xor rdx, rdx
        mov rax, [PSP]
        idiv TOS
        mov TOS, rax
        add PSP, CELLSIZE
        ret

.colon "MOD", mod
        xor rdx, rdx
        mov rax, [PSP]
        idiv TOS
        mov TOS, rdx
        add PSP, CELLSIZE
        ret


.colon "NAND", nand_
        and TOS, [PSP]
        not TOS
        add PSP, CELLSIZE
        ret

.colon "NOR", nor_
        or TOS, [PSP]
        not TOS
        add PSP, CELLSIZE
        ret

.colon "XNOR", xnor_
        xor TOS, [PSP]
        not TOS
        add PSP, CELLSIZE
        ret

.colon "AND", and_
        and TOS, [PSP]
        add PSP, CELLSIZE
        ret

.colon "OR", or_
        or TOS, [PSP]
        add PSP, CELLSIZE
        ret

.colon "XOR", xor_
        xor TOS, [PSP]
        add PSP, CELLSIZE
        ret


;; User-level applications use as integer registers for passing the
;; sequence %rdi, %rsi, %rdx, %rcx, %r8 and %r9. The kernel interface
;; uses %rdi, %rsi, %rdx, %r10, %r8 and %r9.

;; A system-call is done via the syscall instruction. The kernel
;; destroys registers %rcx and %r11.

;; The number of the syscall has to be passed in register %rax.

;; System-calls are limited to six arguments,no argument is passed
;; directly on the stack.

;; Returning from the syscall, register %rax contains the result of
;; the system-call. A value in the range between -4095 and -1
;; indicates an error, it is -errno.

;; Only values of class INTEGER or class MEMORY are passed to the
;; kernel.


.colon "SYSCALL/1", colonsyscall1 ; ( arg1 int -- result )
        DPOP rax
        DPOP rdi
        syscall
        DPUSH rax
        ret

.colon "SYSCALL/2", colonsyscall2
        DPOP rax
        DPOP rdi
        DPOP rsi
        syscall
        DPUSH rax
        ret

.colon "SYSCALL/3", colonsyscall3
        DPOP rax
        DPOP rdi
        DPOP rsi
        DPOP rdx
        syscall
        DPUSH rax
        ret

.colon "SYSCALL/4", colonsyscall4
        DPOP rax
        DPOP rdi
        DPOP rsi
        DPOP rdx
        DPOP r10
        syscall
        DPUSH rax
        ret

.colon "SYSCALL/5", colonsyscall5
        DPOP rax
        DPOP rdi
        DPOP rsi
        DPOP rdx
        DPOP r10
        DPOP r8
        syscall
        DPUSH rax
        ret

.colon "SYSCALL/6", colonsyscall6
        DPOP rax
        DPOP rdi
        DPOP rsi
        DPOP rdx
        DPOP r10
        DPOP r8
        DPOP r9
        syscall
        DPUSH rax
        ret

;; TYPE
.colon "TYPE", type ; ( addr n -- )
        call swap
        DPUSH 1
        DPUSH 1
        call colonsyscall3
        call drop
        ret

.colon "EMIT", emit
        ; instead store the char on the return stack
        DPOP r8
        push r8
        DPUSH rsp
        DPUSH 1
        call type
        ; drop the char on the return stack
        pop r8
        ret

.colon "KEY", key
;; ideally we should set the terminal to raw or something first
        push 0
        DPUSH 1
        DPUSH rsp
        DPUSH 0
        DPUSH 0
        call colonsyscall3
        call drop
        pop r8
        DPUSH r8
        ret

;; stack manipulation

.colon "DUP", dup               ; ( a -- a a )
        sub PSP, 8
        mov [PSP], TOS
        ret

.colon "SWAP", swap             ; ( a b -- b a )
        xchg [PSP], TOS
        ret

.colon "DROP", drop             ; ( a -- )
        xchg [PSP], TOS
        add PSP, CELLSIZE
        ret

.colon "OVER", over             ; ( a b -- a b a )
        mov r8, [PSP]
        DPUSH r8
        ret

.colon "NIP", nip               ; ( a b -- b )
        add PSP, CELLSIZE
        ret

.colon "TUCK", tuck             ; ( a b c -- b a b )
        ; a | b
        xchg [PSP], TOS         ; b | a
        sub PSP, CELLSIZE       ; b ? | a
        mov [PSP], TOS          ; b a | a
        mov TOS, [PSP+CELLSIZE] ; b a | b
        ret

.colon "ROT", rot               ; ( a b c -- b c a )
        DPOP r8
        call swap
        sub PSP, CELLSIZE
        mov [PSP], r8
        ret

.colon "-ROT", minusrot         ; ( a b c -- c a b )
        mov r8, [PSP]
        add PSP, CELLSIZE
        xchg [PSP], TOS
        DPUSH r8
        ret

.colon "PICK", pick
        shl TOS, 3               ; cell size is 8
        add TOS, PSP
        mov TOS, [TOS]
        ret

.colon "ROLL", roll
        ; keeping the index (or something) on TOS, we can rotate the
        ; elements and then advance PSP at the end to get rid of the
        ; second-on-stack, getting the correct value on TOS and on
        ; second-on-stack
        xchg r8, TOS
        mov r9, r8
        shl r9, 3               ; cell size is 8
        add r9, PSP
        mov TOS, [r9]
        ; now copy stack down (or up...?)
        mov rdi, r9
        sub r9, CELLSIZE
        mov rsi, r9
        mov rcx, r8
        ; IN REVERSE, stack grows downwards, so we want to start
        ; copying from the end, and decrement the pointers!!
        std
        rep movsq
        cld
        ; finally adjust the stack pointer to get rid of previous TOS
        add PSP, CELLSIZE
        ret

.colon "CR", cr
        DPUSH 10
        call emit
        ret

.colon "BYE", bye
        DPUSH 0
        DPUSH 60
        call colonsyscall1
        ; not that we ever get here...
        call drop
        ret

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

.colon "UNUSED", unused
        mov r8, end_of_dictionary
        sub r8, [val_dp]
        DPUSH r8
        ret

.colon "(", leftparen
        DPUSH ')'
        call word_
        call drop
        ret

.colon '\', backslash
        DPUSH 10
        call word_
        call drop
        ret

.colon "#!", shellsignature
        DPUSH 10
        call word_
        call drop
        ret

.variable "SOURCE-ID", sourceid, 0

.colon "REFILL", refill
        mov r8, [val_sourceid]
        cmp r8, -1
        je refill_error

        ; clear the input buffer
        mov rdi, TIBDATA
        mov rcx, BUFFERSIZE
        mov al, ' '
        rep stosb

        ; read a... line?
        DPUSH BUFFERSIZE
        DPUSH TIBDATA
        mov r8, [val_sourceid]
        DPUSH r8
        DPUSH 0
        call colonsyscall3
        DPOP rax

        ; rax holds size or -errno
        test rax, rax
        ;; exit when either error or EOF :-)
        js refill_error
        jz refill_error

        ; no error, reset >IN and return true
        DPUSH 0
        call TIBIN
        call store
        DPUSH -1
        ret

refill_error:
        DPUSH 0                 ; return false
        ret

.constant "R/O", rofam, 0
.constant "W/O", wofam, 1
.constant "R/W", rwfam, 2

; .colon 'S"', squote             ; always compiles a string (?)
;         DPUSH '"'
;         call word_
;         ; ignore errors, they never happen :D
;         ; also ignore length 0 because yeah :D
;         call count
;         DPOP rcx
;         DPOP rsi
;         mov rdi, val_dp
;         ; compile LITERAL address
;         DPUSH lit
;         call compilecomma
;         mov r8, rdi
;         add r8, 5+8+5+8+5       ; length of the lit calls and jump
;         DPUSH r8
;         call comma
;         ; compile LITERAL length
;         DPUSH lit
;         call compilecomma
;         DPUSH rcx
;         call comma
;         ; compile relative jump, offset LENGTH
;         mov W, rdi
;         mov byte [rdi], 0xE9
;         mov dword [rdi+1], ecx
;         ; store string inline
;         rep movsb
;         ; push address and count
;         DPUSH 0
;         DPUSH rcx
;         ret

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
        ; mov rdi, PSP
        ; ok so what if we move it to the return stack instead...?
        DPOP r8
        push 10                 ; newline
        push r8


word_skip_delimiters:
        mov rdi, rsp
        cmpsb
        je word_check_end

        ;; if not equal we may have a word
        ;; we still need to check for newline

maybe_newline:
        dec rsi
        add rdi, 7
        cmpsb
        je word_check_end

        ;; we do have a word
        jmp skipped_delimiters

word_check_end:
        ; if we went over the end of TIBDATA, we're done
        cmp rsi, TIBDATA+BUFFERSIZE
        je tibdata_was_empty

        ;; otherwise continue
        jmp word_skip_delimiters

tibdata_was_empty:
        mov rsi, WORDBUFFER
        mov qword [rsi], qword 0

        ; clean up the delimiters from the return stack
        ; call drop (?)
        pop r8
        pop r8
        DPUSH WORDBUFFER

        ret

skipped_delimiters:
        ; readjust the pointers back
        dec rsi
        DPUSH rsi               ; save the beginning of the word

find_closing_delimiter:
        ; we have a word, find the end
        ; the word address is on rsi (and on the stack)
        ; the delimiter is on rdi (and on the stack under TOS)

        ; compare char with delimiter
        mov rdi, rsp
        cmpsb

        ; if we find delimiter, we are past the word
        je found_closing_delimiter

        ;; repeat for newline
        dec rsi
        add rdi, 7
        cmpsb
        je found_closing_delimiter

        ; if we went over the end of TIBDATA, return right away
        cmp rsi, TIBDATA+BUFFERSIZE
        je found_closing_delimiter

        ;; otherwise move along to the next char
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

        ;; clear the delimiters off the stack
        ;; call drop (?)
        pop r8
        pop r8

        ; return the address of the parsed word
        DPUSH WORDBUFFER
        ret

.colon "FIND", find             ; ( c-addr -- c-addr 0 | xt 1 | xt -1 )
        ; store the address of the source string on r10
        ; mov r10, [PSP]
        DPOP r10
        DPUSH r10
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

.colon "[']", tickimm, IMM                ; ( c"" -- xt )
        call bl_
        call word_
        call find
        call drop               ; nope, bad
        DPUSH lit
        call compilecomma
        call comma
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
        ;mov r13d, [PSP]
        ;call drop
        DPOP X

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
        ;mov r13d, [PSP]
        ;call drop
        DPOP X

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
        push rax
        push r8
        call emit
        pop r8
        pop rax
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
        push r8
        call emit
        pop r8
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
        DPOP r8
        test r8, r8
        jz bye

        ;; call drop    ; should do something with this flag(!)

        call interpret
        call quit_prompt
        jmp quit

quit_prompt:
        DPUSH promptStr
        DPUSH promptLen
        call type
        ret

;; .colon "QUIT", quit
;;         ; debug loop
;;         call key
;;         call emit
;;         call quit_prompt
;;         jmp quit

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
        DPUSH CELLSIZE
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

.colon "IMMEDIATE", immediate
        ; find last entry
        call latest
        call fetch
        ; get to the length
        DPUSH CELLSIZE
        call plus
        call dup
        ; enable MSB
        call cfetch
        DPOP r8
        or r8, IMM
        DPUSH r8
        call swap
        call cstore
        ret

.colon "BREAK", break
        int3
        ret

.colon "POSTPONE", postpone, IMM
        ; parse input stream and find xt
        call bl_
        call word_
        call dup
        call cfetch
        DPOP W
        test W, W
        jz postpone_end        ; no more input
                                ; probably not the right thing to do
                                ; we should abort? don't have it yet xD

        call find
        DPOP W
        test W, W

        ; if not found, we should abort, don't have it yet xD
        jz postpone_end

        ; compile relative call
        DPOP W

        ; compile a near relative call, target address is in W
        call here
        DPOP rdi
        mov byte [rdi], 0xE8

        ; obtain a 32 bit number to work with 32 bit signed
        call here
        ;mov r13d, [PSP]
        ;call drop
        DPOP X

        sub r12d, r13d       ; this is W as a dword
        sub r12d, 5          ; additional offset from next instruction
        mov [rdi+1], r12d    ; this is W as (now negative) dword

        ; update here
        call here
        DPUSH 5
        call plus
        call dp
        call store
        ret

postpone_end:
        call drop
        ret

.colon "COMPILE@", compileat ; ( xt addr -- ) compiles a call...
        ; compile relative call
        DPOP rdi
        DPOP W

        ; obtain a 32 bit number to work with 32 bit signed
        DPUSH rdi
        ;mov r13d, [PSP]
        ;call drop
        DPOP X

        sub r12d, r13d       ; this is W as a dword
        sub r12d, 5          ; additional offset from next instruction

        ; compile a near relative call, target address is in W
        mov byte [rdi], 0xE8
        mov [rdi+1], r12d    ; this is W as (now negative, or not) dword

        ret

.colon "COMPILE,", compilecomma ; ( xt -- ) compiles a call...? execute? what?
        call here
        call compileat

        ; update here
        call here
        DPUSH 5
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
.colon "LIT", lit
        pop r8
        mov r9, [r8]
        DPUSH r9
        add r8, CELLSIZE
        push r8
        ret


.colon "(0branch)", zerobranch
        pop r9
        DPOP r8
        test r8, r8
        jz zerobranch_backward
        add r9, CELLSIZE
        push r9
        ret
zerobranch_backward:
        push qword [r9]
        ret

.colon "(branch)", branch
        pop r9
        push qword [r9]
        ret

.colon "(for)", innerfor
        ; slide the loop counter on the stack to second on return stack
        DPOP r8
        pop r9
        push r8
        push r9
        ret

.colon "(next)", innernext
        ; decrease the index by 1
        dec qword [rsp+8]
        ret

.colon "(endfor)", endfor
        ; remove the loop counter from second on return stack
        pop r8
        pop r9
        push r8
        ret

.colon "(do)", innerdo
        ;; put index and limit on the return stack
        DPOP r8
        DPOP r9
        pop r10
        push r9
        push r8
        push r10
        ret

.colon "(loop)", innerloop
        ; increase the loop counter by 1
        inc qword [rsp+8]
        ret

.colon "(enddo)", enddo
        ; remove the loop data from return stack
        pop r8
        pop r9
        pop r9
        push r8
        ret


.colon "I", i
        mov r8, [rsp+8]
        DPUSH r8
        ret

.colon "J", j
        mov r8, [rsp+24]
        DPUSH r8
        ret

.colon "(limit)", dolimit
        mov r8, [rsp+16]
        DPUSH r8
        ret




;; allocation:

;; just allocate 8 extra bytes, stick the length in the beginning,
;; return the pointer after the length to the user

;; freeing:
;; subtract 8 bytes from the address, get the length from there

;; resizing:
;; subtract 8 bytes from the address, store the new length there

;; dynamic memory - mmap() allocates 4Kb pages (so it should always be aligned...?)

;; test this ???

.colon "ALLOCATE", allocate     ; ( u -- addr ior )
        ; off fd flags prot len address 9 SYSCALL/6
        DPOP W                  ; save the length in W
        add W, CELLSIZE
        DPUSH 0
        DPUSH -1                ; -1 for compatibility
        DPUSH MMAP_FLAGS
        DPUSH MMAP_PROTECTION
        DPUSH W
        DPUSH 0
        DPUSH 9                 ; mmap
        call colonsyscall6

        ; if TOS is -1, return ( invalid-addr -1 ) straight ahead
        call dup
        test TOS, TOS
        js allocate_end

        ; otherwise store the length at the address, then increment
        ; address by CELLSIZE
        mov r8, [PSP]
        mov [r8], W
        add r8, CELLSIZE
        mov [PSP], r8
        ; and replace TOS with a 0
        xor TOS, TOS
allocate_end:
        ret

.colon "FREE", free     ; ( addr -- ior )
        ; len address 11 SYSCALL/2
        sub TOS, CELLSIZE
        mov r9, [TOS]
        DPUSH r9
        call swap
        DPUSH 11                ; munmap
        call colonsyscall2
        ret

.colon "RESIZE", resize     ; ( addr u -- addr' ior )
        ; new_addr flags new_len old_len addr 25 syscall/5
        DPOP W                  ; length
        DPOP X                  ; address
        DPUSH 0
        DPUSH 0                 ; or maybe MREMAP_MAYMOVE?
        add W, CELLSIZE         ; account for storing the size
        DPUSH W                 ; new length
        sub X, CELLSIZE
        mov r9, [X]
        DPUSH r9                ; old length
        DPUSH X                 ; address
        DPUSH 25                ; mremap
        call colonsyscall5

        ; if TOS is -1, return ( invalid-addr -1 ) straight ahead
        call dup
        test TOS, TOS
        js resize_end

        ; otherwise store the length at the address, then increment
        ; address by CELLSIZE
        mov r8, [PSP]
        mov [r8], W
        add r8, CELLSIZE
        mov [PSP], r8
        ; and replace TOS with a 0
        xor TOS, TOS
resize_end:
        ret

.colon "CMOVE", cmove           ; ( addr1 addr2 u -- )
        DPOP rcx
        DPOP rdi
        DPOP rsi
        test rcx, rcx
        jz cmove_end
        rep movsb
cmove_end:
        ret

.colon "FILL", fill             ; ( addr u c -- )
        DPOP rax
        DPOP rcx
        DPOP rdi
        test rcx, rcx
        jz fill_end
        rep stosb
fill_end:
        ret

.colon "ERASE", erase             ; ( addr u -- )
        DPUSH 0
        call fill
        ret

filenamestr:
        resb 4096


;; ( c-addr u fam -- fileid ior )
.colon "OPEN-FILE", openfile
        call minusrot           ; ( fam c-addr u )

        ;; clear filenamestr with zeros ( C-string compatibility )
        DPUSH filenamestr
        DPUSH 4096
        DPUSH 0
        call fill

        DPUSH filenamestr
        call swap               ; ( fam a-addr fstr u )
        call cmove              ; ( fam )  ; mode

        DPUSH 0                 ; flags

        DPUSH filenamestr       ; C-filename pointer

        DPUSH 2
        ;; mode flags c-string 2
        call colonsyscall3
        DPUSH rax
        ret


; last builtin word, for now, this is important because init uses this
; word to set up LATEST
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

        ; compile a near relative call, target address is in W
        mov W, created
        call dp
        call fetch
        DPOP rdi
        mov byte [rdi], 0xE8

        ; obtain a 32 bit number to work with 32 bit signed
        call dp
        call fetch
        ;mov r13d, [PSP]
        ;call drop
        DPOP X

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

        ret

end_of_builtins:
;; should I add a blob of uninitialised (or initialised) space here?

        resb 16384
end_of_dictionary:

;; the program code here
SECTION .text
align 8

global _start

;; perform various initialization stuff
warm:
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

        ret

init:
        call warm
        mov rsi, val_dp
        mov qword [rsi], end_of_builtins
        mov rsi, val_latest
        mov qword [rsi], create_entry ; THIS HAS TO BE MANUALLY UPDATED...(!)

        ; load boot.fs


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
