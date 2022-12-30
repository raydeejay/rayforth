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

;; rcx and r11 are destroyed by syscalls
;; should change Y to r8 and rewrite?

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

        notFoundMsgStr db " not found"
        notFoundMsgLen equ $-notFoundMsgStr

        promptStr db " ok", 10
        promptLen equ $-promptStr

        bootfsStr db "boot.fs"
        bootfsLen equ $-bootfsStr

;; here's where these things go, apparently
SECTION .bss
align 8

;; Parameter stack
        DATASTACK resb CELLSIZE*STACKSIZE
        DATASTACKBOTTOM equ $
        RETURNSTACKBOTTOM resb 8

;; Parameter Stack Macros

%define NOS [PSP]

%define NIP add PSP, CELLSIZE

%macro DROP 0
        mov TOS, NOS
        NIP
%endmacro

%macro DUP 0
        sub PSP, CELLSIZE
        mov qword NOS, TOS
%endmacro

%macro DPUSH 1
        DUP
        mov TOS, qword %1
%endmacro

%macro DPOP 1
        mov qword %1, TOS
        mov TOS, NOS
        NIP
%endmacro

%macro CLR 1
        xor %1, %1
%endmacro

%macro SWAP 0-1 r8
        mov %1, TOS
        mov TOS, NOS
        mov NOS, %1
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
        mov r8, NOS
        mov [TOS], r8
        mov TOS, [PSP+CELLSIZE]
        add PSP, CELLSIZE*2
        ret

.colon "+!",plusstore
        mov r8, NOS
        add [TOS], r8
        mov TOS, [PSP+CELLSIZE]
        add PSP, CELLSIZE*2
        ret

.colon "C@", cfetch
        movzx TOS, byte [TOS]
        ret

.colon "C!", cstore
        movzx r8, byte NOS
        mov byte [TOS], r8b
        mov TOS, [PSP+CELLSIZE]
        add PSP, CELLSIZE*2
        ret

.colon "C+!", cplusstore
        movzx r8, byte NOS
        add byte [TOS], r8b
        mov TOS, [PSP+CELLSIZE]
        add PSP, CELLSIZE*2
        ret

;; mostly for internal use
.colon "PSP", pointerOfNOS
        mov r8, PSP
        DUP
        mov TOS, r8
        ret

.colon "RP@", rpFetch
        DUP
        mov TOS, rsp
        add TOS, CELLSIZE  ; return value under this function's return

.colon "R@", rfetch
        DUP
        mov TOS, [rsp+CELLSIZE]
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
        mov r8, TOS
        CLR TOS
        mov W, -1
        test r8, r8
        cmovz TOS, W
        ret

.colon "0<>", zeroNotEqual
        mov r8, TOS
        CLR TOS
        mov W, -1
        test r8, r8
        cmovnz TOS, W
        ret

.colon "=", equal
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmove TOS, W
        NIP
        ret

.colon "<>", different
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovne TOS, W
        NIP
        ret

.colon "<", lesserthan
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovl TOS, W
        NIP
        ret

.colon ">", greaterthan
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovg TOS, W
        NIP
        ret

.colon "<=", lesserthanorequal
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovle TOS, W
        NIP
        ret

.colon ">=", greaterthanorequal
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovge TOS, W
        NIP
        ret

.colon "U<", ulesserthan
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovb TOS, W
        NIP
        ret

.colon "U>", ugreaterthan
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmova TOS, W
        NIP
        ret

.colon "U<=", ulesserthanorequal
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovbe TOS, W
        NIP
        ret

.colon "U>=", ugreaterthanorequal
        mov r8, TOS
        CLR TOS
        mov W, -1
        cmp NOS, r8
        cmovae TOS, W
        NIP
        ret

.colon "+", plus
        add TOS, NOS
        NIP
        ret

.colon "-", minus
        sub NOS, TOS
        mov TOS, NOS
        NIP
        ret

.colon "1+", increment
        inc TOS
        ret

.colon "1-", decrement
        dec TOS
        ret

.colon "*", multiply            ; bit broken but works for reasonable numbers... xD
        imul TOS, NOS
        NIP
        ret

;; Signed divide RDX:RAX by r/m64, with result stored in
;; RAX ← Quotient, RDX ← Remainder.
.colon "/MOD", dividemod
        xor rdx, rdx
        mov rax, NOS
        idiv TOS
        mov NOS, rax
        mov TOS, rdx
        ret

;; Unsigned divide RDX:RAX by r/m64, with result stored in
;; RAX ← Quotient, RDX ← Remainder.
;; ( ud u1 -- u2 u3 )
.colon "UM/MOD", umdividemod
        mov rdx, [PSP+CELLSIZE] ; which will be 0, but whatever...
        mov rax, NOS
        div TOS
        mov NOS, rax
        mov TOS, rdx
        ret

.colon "/", divide
        xor rdx, rdx
        mov rax, NOS
        idiv TOS
        mov TOS, rax
        NIP
        ret

.colon "MOD", mod
        xor rdx, rdx
        mov rax, NOS
        idiv TOS
        mov TOS, rdx
        NIP
        ret

.colon "MIN", min
        mov r8, NOS
        mov r9, TOS
        cmp r8, r9
        cmovl TOS, r8
        NIP
        ret

.colon "MAX", max
        mov r8, NOS
        mov r9, TOS
        cmp r8, r9
        cmovg TOS, r8
        NIP
        ret

.colon "ABS", _abs
        mov r8, TOS
        neg r8
        cmovns TOS, r8
        ret

.colon "NEGATE", negate
        neg TOS
        ret

.colon "NAND", nand_
        and TOS, NOS
        not TOS
        NIP
        ret

.colon "NOR", nor_
        or TOS, NOS
        not TOS
        NIP
        ret

.colon "XNOR", xnor_
        xor TOS, NOS
        not TOS
        NIP
        ret

.colon "AND", and_
        and TOS, NOS
        NIP
        ret

.colon "OR", or_
        or TOS, NOS
        NIP
        ret

.colon "XOR", xor_
        xor TOS, NOS
        NIP
        ret

.colon "INVERT", invert
        not TOS
        ret

.colon "2*", shift1left
        shl TOS, 1
        ret

.colon "2/", shift1right
        shr TOS, 1
        ret

.colon "LSHIFT", shiftleft
        mov rcx, TOS
        mov TOS, NOS
        add PSP, CELLSIZE
        shl TOS, cl
        ret

.colon "RSHIFT", shiftright
        mov rcx, TOS
        mov TOS, NOS
        add PSP, CELLSIZE
        shr TOS, cl
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
        DUP
        ret

.colon "2DUP", _2dup               ; ( a b -- a b a b )
        mov r8, NOS
        sub PSP, CELLSIZE*2
        mov [PSP+CELLSIZE], TOS
        mov NOS, r8
        ret

.colon "SWAP", swap             ; ( a b -- b a )
        SWAP
        ret

.colon "2SWAP", _2swap             ; ( a b c d -- c d a b )
        mov r8, [PSP+CELLSIZE*2]
        mov r9, NOS
        mov NOS, r8
        mov [PSP+CELLSIZE*2], r9
        mov r11, TOS
        mov r12, [PSP+CELLSIZE]
        mov [PSP+CELLSIZE], r11
        mov TOS, r12
        ret

.colon "DROP", drop             ; ( a -- )
        DROP
        ret

.colon "2DROP", _2drop             ; ( a b -- )
        mov TOS, [PSP+CELLSIZE]
        add PSP, CELLSIZE*2
        ret

.colon "OVER", over             ; ( a b -- a b a )
        mov r8, NOS
        DPUSH r8
        ret

.colon "2OVER", _2over             ; ( a b c d -- a b c d a b )
        sub PSP, CELLSIZE*2
        mov [PSP+CELLSIZE], TOS
        mov r8, [PSP+CELLSIZE*4]
        mov NOS, r8
        mov TOS, [PSP+CELLSIZE*3]
        ret

.colon "NIP", nip               ; ( a b -- b )
        NIP
        ret

.colon "TUCK", tuck             ; ( a b -- b a b )
        mov r8, NOS
        sub PSP, CELLSIZE
        mov NOS, r8
        mov [PSP+CELLSIZE], TOS
        ret

.colon "ROT", rot               ; ( a b c -- b c a )
        mov r8, NOS
        mov r9, [PSP+CELLSIZE]
        mov NOS, TOS
        mov [PSP+CELLSIZE], r8
        mov TOS, r9
        ret

.colon "-ROT", minusrot         ; ( a b c -- c a b )
        mov r8, NOS
        mov r9, [PSP+CELLSIZE]
        mov [PSP+CELLSIZE], TOS
        mov NOS, r9
        mov TOS, r8
        ret

.colon "PICK", pick
        shl TOS, 3               ; cell size is 8
        add TOS, PSP
        mov TOS, [TOS]
        ret

.colon "ROLL", roll
        ; all the elements to be rotated are on the PSP area,
        ; TOS holds the index, overwrite it with the final value
        mov r8, TOS
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
        ; finally adjust the stack pointer since we consumed the index
        NIP
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
.variable ">IN", TOIN, 0

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

.colon "(", leftparen, IMM
        DPUSH ')'
        call word_
        call drop
        ret

.colon '\', backslash, IMM
;;; '  ; work around nasm-mode highlighting
        DPUSH 10
        call word_
        call drop
        ret

.colon "#!", shellsignature, IMM
        DPUSH 10
        call word_
        call drop
        ret

;; ( c-addr u1 fileid -- u2 flag ior )
.colon "READ-LINE", readline
        DPOP W                  ; fid
        DPOP X                  ; max
        xor r8, r8              ; count
        DPOP r9                 ; c-addr

readline_next_char:
        cmp r8, X
        je readline_done

        DPUSH 1
        DPUSH r9
        DPUSH W
        DPUSH 0                 ; read syscall
        call colonsyscall3
        DPOP rax

        ;; rax holds size (0/1) or -errno
        test rax, rax
        ;; exit when either error
        js readline_error
        ;; or EOF
        jz readline_eof

        ;; if newline then done
        cmp byte [r9], 10
        je readline_done

        ;; move to next char
        inc r9
        inc r8
        jmp readline_next_char

readline_done:
        DPUSH r8
        DPUSH -1
        DPUSH 0
        ret

readline_eof:
        DPUSH r8
        DPUSH 0
        DPUSH 0
        ret

readline_error:
        DPUSH r8
        DPUSH r8                ; return values don't matter
        DPUSH -1                ; and ior
        ret

.variable "<sourceaddr>", sourceaddr, TIBDATA
.variable "<sourcelen>", sourcelen, BUFFERSIZE
.variable "SOURCE-ID", sourceid, 0
.variable "BLK", blk, 0

.colon "SOURCE", source
        mov r8, [val_sourceaddr]
        mov r9, [val_sourcelen]
        DPUSH r8
        DPUSH r9
        ret

;; ( -- f )
.colon "REFILL", refill
        mov r8, [val_sourceid]
        cmp r8, -1
        je refill_error

        ; clear the input buffer
        ; mov rdi, TIBDATA
        ; mov rcx, BUFFERSIZE
        mov rdi, [val_sourceaddr]
        mov rcx, [val_sourcelen]
        mov al, ' '
        rep stosb

        ; read a... line
        ; DPUSH TIBDATA
        ; DPUSH BUFFERSIZE
        mov r8, [val_sourceaddr]
        mov r9, [val_sourcelen]
        mov r10, [val_sourceid]
        DPUSH r8
        DPUSH r9
        DPUSH r10
        call readline

        DPOP W                  ; test ior
        test W, W
        js refill_error2

        DPOP X                  ; test flag
        test X, X
        jz refill_error

refill_done:
        ; no error, reset >IN and return true
        DPOP r8
        DPUSH 0
        call TOIN
        call store
        DPUSH -1
        ret

refill_error2:
        DPOP r8
refill_error:
        DPUSH 0                 ; return false
        ret

.constant "R/O", rofam, 0
.constant "W/O", wofam, 1
.constant "R/W", rwfam, 2

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
        ; call TIB
        ; call fetch
        mov r8, [val_sourceaddr]
        DPUSH r8
        call TOIN
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
        ; cmp rsi, TIBDATA+BUFFERSIZE-1
        mov r8, [val_sourceaddr]
        add r8, [val_sourcelen]
        dec r8
        cmp rsi, r8
        jge tibdata_was_empty

        ;; otherwise continue
        jmp word_skip_delimiters

tibdata_was_empty:
        ;; mov rsi, WORDBUFFER
        mov rsi, [val_dp]
        mov qword [rsi], qword 0

        ; clean up the delimiters from the return stack
        ; call drop (?)
        pop r8
        pop r8
        ;; DPUSH WORDBUFFER
        mov r8, [val_dp]
        DPUSH r8

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

        ; if we went over the end of input_buffer, return right away
        ; address A holding size S gives a max address of A+S-1
        ; cmp rsi, TIBDATA+BUFFERSIZE-1
        mov r8, [val_sourceaddr]
        add r8, [val_sourcelen]
        dec r8
        cmp rsi, r8
        je found_closing_delimiter

        ;; otherwise move along to the next char
        jmp find_closing_delimiter

found_closing_delimiter:
        ; update >IN
        ; with the difference between end of parsing and end of input bufer
        mov W, rsi
        sub W, [val_sourceaddr]
        DPUSH W
        call TOIN
        call store

        ; we have the end, calculate the length now
        ; rsi holds the end
        ; the word address is on the stack
        mov rcx, rsi
        DPOP W
        sub rcx, W            ; load the number of chars on RCX (end-start)
        dec rcx                 ; correct the off by one

        mov rsi, W
        ;; mov rdi, WORDBUFFER
        mov rdi, [val_dp]
        mov [rdi], cl           ; put the count

        ; put the string
        inc rdi
        rep movsb

        ;; clear the delimiters off the stack
        ;; call drop (?)
        pop r8
        pop r8

        ; return the address of the parsed word
        ;; DPUSH WORDBUFFER
        mov r8, [val_dp]
        DPUSH r8
        ret

.colon "FIND", find             ; ( c-addr -- c-addr 0 | xt 1 | xt -1 )
        ; store the address of the source string on r10
        ; mov r10, NOS
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

find_next_link:
        ; if they're different move to the next link
        mov Y, [Y]
        mov rdi, Y

        ; if the link is 0, not found
        test rdi, rdi
        jz find_not_found

        ; else check the next entry
        jmp find_setup

find_check_names:
        ; count is still loaded on bl and cl
        ; advance the pointers over the counts
        inc rsi
        inc rdi

        ; if no chars left (-1), they're equal, we're done
        dec rcx
        js find_word_found

        ; compare strings, count is already loaded in cl/rcx
        mov r8b, byte [rsi]
        mov r9b, byte [rdi]
        cmp r8b, r9b

        ; if they're equal keep checking
        je find_check_names

        ; retry with toggled case (but only if it's a letter!!)
        or r8b, 0b00100000      ; force "lowercase"
        sub r8b, 'a'            ; convert to 0-25
        cmp r8b, 'z'-'a'        ; check if it is a letter
        ja find_next_link

        or r9b, 0b00100000      ; force lowercase on dict letter
        sub r9b, 'a'            ; convert, no need for checks here
        cmp r8b, r9b
        je find_check_names

        jmp find_next_link

find_not_found:
        ; return c-addr and 0
        DPUSH 0
        ret

find_word_found:
        ; drop c-addr
        DROP
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
        DROP                    ; nope, bad
        ret

.colon "[']", tickimm, IMM                ; ( c"" -- xt )
        call bl_
        call word_
        call find
        DROP                    ; nope, bad
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

        CLR r10             ; clear the 64-bit register to load 8 bits

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
        ;mov r13d, NOS
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
        ;mov r13d, NOS
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
        call warm
        ret

interpret_end:
        call drop
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

        mov W, rax
        DPUSH '-'
        call emit
        mov rax, W
        ; then negate the number and print it normally
        neg rax

period_begin_process:
        CLR W

period_process_digit:
        CLR rdx
        mov rbx, [val_base]
        div rbx
        add rdx, DIGITS            ; make a letter
        mov rdx, [rdx]
        DPUSH byte rdx
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

;; hack to print numbers without spaces, until I implement U.R
.colon "..", period2              ; ( n -- )
        ; if 0, just print 0 and exit
        DPOP rax
        test rax, rax
        jz period_zero2

        ; display negatives somehow... only if BASE is 10
        cmp qword [val_base], 10
        jne period_begin_process2

        ; print a '-' only if it's a negative
        test rax, rax
        jns period_begin_process2

        mov W, rax
        DPUSH '-'
        call emit
        mov rax, W
        ; then negate then number and print it normally
        neg rax

period_begin_process2:
        CLR W

period_process_digit2:
        CLR rdx
        mov rbx, [val_base]
        div rbx
        add rdx, DIGITS            ; make a letter
        mov rdx, [rdx]
        DPUSH byte rdx
        inc W
        test rax, rax
        jnz period_process_digit2

period_emit_digit2:
        ; no more digits, print them back from the stack
        call emit
        dec W
        jnz period_emit_digit2

        jmp period_done2

period_zero2:
        DPUSH '0'
        call emit

period_done2:
        ret


.colon ".S", printstack
        DPUSH ' '
        DPUSH '['
        call emit
        call emit
        DPUSH 0              ; inject a 0 to move TOS down into memory
        mov r9, DATASTACKBOTTOM-CELLSIZE ; address of bottom cell on r9
        mov X, DATASTACKBOTTOM-CELLSIZE
        sub X, PSP
        shr X, 3                ; divide by 8, DEPTH on X

printstack_next_deepest:
        cmp r9, PSP
        jl printstack_done

        mov r8, [r9]
        DPUSH r8
        call period
        sub r9, CELLSIZE
        jmp printstack_next_deepest

printstack_done:
        DPOP r8                 ; clear the injected 0
        DPUSH ' '
        DPUSH ']'
        call emit
        call emit
        inc X
        DPUSH X
        call period
        ret

.variable "<prompt?>", _promptbool, -1

.colon "<noprompt>", _noprompt
        DPUSH 0
        call _promptbool
        call store
        ret

.colon "<prompt>", _prompt
        DPUSH -1
        call _promptbool
        call store
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
        mov r8, [val__promptbool]
        test r8, r8
        jz quit_prompt_end

        DPUSH promptStr
        DPUSH promptLen
        call type
quit_prompt_end:
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

        ; if immediate, must compile code to compile a relative call !!!

        ; compile relative call
        DPOP W

        ; compile a near relative call, target address is in W
        call here
        DPOP rdi
        mov byte [rdi], 0xE8

        ; obtain a 32 bit number to work with 32 bit signed
        call here
        ;mov r13d, NOS
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
        ;mov r13d, NOS
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
        ;; call word_              ; which will be on WORDBUFFER as a c-string
        call word_              ; which will be on HERE as a counted string

        ;; get the length and increment dp
        ;; then flags+count followed by the name
        ;; call dup
        ;; call cfetch
        ;; DPUSH 1
        ;; call plus
        ;; DPOP rcx                ; we will copy count+1 bytes
        ;; DPOP rsi
        ;; DPUSH rcx               ; let's keep the size on the stack
        ;; call dp
        ;; call fetch
        ;; DPOP rdi
        ;; rep movsb

        ; update here (we left the count on the stack)
        call dup
        call cfetch
        call plus
        DPUSH 1
        call plus               ; add HERE to the size we kept on the stack + 1
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
        ; inject a false result by default
        DUP
        CLR TOS
        mov Y, -1
        ; increase the loop counter by 1
        mov W, [rsp+CELLSIZE]
        add W, 1
        ; write back the increment
        mov qword [rsp+CELLSIZE], W
        ; check if = limit
        mov X, [rsp+CELLSIZE*2]
        cmp W, X
        ; set result if needed
        cmove TOS, Y
        ret

.colon "(+loop)", innerplusloop
        ; move TOS to r8, inject a false result by default
        mov r8, TOS
        DUP
        CLR TOS
        mov Y, -1
        ; increase the loop counter by r8 (positive or negative)
        mov W, [rsp+CELLSIZE]
        add W, r8
        ; write back the increment
        mov qword [rsp+CELLSIZE], W
        ; check limit, according to sign of r8
        test r8, r8
        js innerplusloopdown
innerplusloopup:
        mov X, [rsp+CELLSIZE*2]
        cmp W, X
        cmovge TOS, Y           ; set result if needed
        jmp innerplusloopdone
innerplusloopdown:
        mov X, [rsp+CELLSIZE*2]
        cmp W, X
        cmovle TOS, Y           ; set result if needed
innerplusloopdone:
        mov X, [rsp+CELLSIZE*2]
        cmp W, X
        ; set result if needed
        cmove TOS, Y
        ret

.colon "(enddo)", enddo
        ; remove the loop data from return stack
        pop r8
        pop r9
        pop r9
        push r8
        ret


.colon "I", i
        mov r8, [rsp+CELLSIZE]
        DPUSH r8
        ret

.colon "J", j
        mov r8, [rsp+CELLSIZE*3]
        DPUSH r8
        ret

.colon "(limit)", dolimit
        mov r8, [rsp+CELLSIZE*2]
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
        mov r8, NOS
        mov [r8], W
        add r8, CELLSIZE
        mov NOS, r8
        ; and replace TOS with a 0
        CLR TOS
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
        mov r8, NOS
        mov [r8], W
        add r8, CELLSIZE
        mov NOS, r8
        ; and replace TOS with a 0
        CLR TOS
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

.colon "BLANK", blank             ; ( addr u -- )
        DPUSH 32
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
        call colonsyscall3
        ;; must return ior
        call dup
        DPUSH 0
        call lesserthan
        ret

;; ( fileid -- ior )
.colon "CLOSE-FILE", closefile
        DPUSH 3                 ; close(fd)
        call colonsyscall1
        ret

;; ( addr n -- )
.colon "INCLUDED", included
        ; save input specification directly to rstack
        push qword 4
        push qword [val_sourceid]
        push qword [val_TOIN]
        push qword [val_sourcelen]
        push qword [val_sourceaddr]

        ; open the file read only
        DPUSH val_rofam
        call openfile

        ; ignore errors for now, what could possibly go wrong :-D
        DPOP r8
        ; store the source-id
        DPOP r8
        mov [val_sourceid], r8

        ; make the file the input source
        mov qword [val_sourcelen], BUFFERSIZE
        DPUSH BUFFERSIZE
        call allocate
        DPOP r8                 ; ignoring the errors again \o/
        DPOP r8
        mov [val_sourceaddr], r8

        ; store a 0 in BLK
        mov qword [val_blk], 0

        ; repeat until eof: read line, set >in to 0, interpret
included_next_line:
        call refill
        DPOP r8
        test r8, r8
        jz included_done

        mov qword [val_TOIN], 0
        call interpret
        jmp included_next_line

included_done:
        call drop               ; not sure why....
        ; free buffer
        mov r8, [val_sourceaddr]
        DPUSH r8
        call free
        DPOP r8                 ; and more error ignoring

        ; close file
        mov r8, [val_sourceid]
        DPUSH r8
        call closefile
        DPOP r8                 ; and yet some more

        ; restore input specification
        pop qword [val_sourceaddr]
        pop qword [val_sourcelen]
        pop qword [val_TOIN]
        pop qword [val_sourceid]
        pop qword r8            ; final error ignoring

        ret

;; perform various initialization stuff
.colon "ABORT", abort
        call warm
        jmp quit
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
        ;; call dup
        ;; call cfetch
        ;; DPUSH 1
        ;; call plus
        ;; DPOP rcx                ; we will copy count+1 bytes
        ;; DPOP rsi
        ;; DPUSH rcx               ; let's keep the size on the stack
        ;; call dp
        ;; call fetch
        ;; DPOP rdi
        ;; rep movsb

        ; update here (we left the address on the stack)
        call dup
        call cfetch
        call plus               ; add HERE to the size we kept on the stack
        DPUSH 1
        call plus
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
        ;mov r13d, NOS
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

        resb 65536
end_of_dictionary:

;; the program code here
SECTION .text
align CELLSIZE

global _start

warm:
        ; clear the buffers
        mov rdi, PADDATA
        mov rcx, BUFFERSIZE
        mov al, ' '
        rep stosb

        mov rdi, TIBDATA
        mov rcx, BUFFERSIZE
        mov al, ' '
        rep stosb

        ; reset the stacks
        mov rdi, DATASTACK
        mov rcx, CELLSIZE*STACKSIZE
        mov al, 0
        rep stosb

        mov PSP, DATASTACKBOTTOM
        CLR TOS

        ; should reset return stack here
        pop r8
        mov qword rsp, qword [RETURNSTACKBOTTOM]
        push r8

        ; reset STATE to interpret
        mov qword [val_state], 0

        ret

init:
        mov r8, rsp
        sub r8, CELLSIZE
        mov qword [RETURNSTACKBOTTOM], r8
        call warm
        mov rsi, val_dp
        mov qword [rsi], end_of_builtins
        mov rsi, val_latest
        mov qword [rsi], create_entry ; THIS HAS TO BE MANUALLY UPDATED...(!)
        ret

loadbootfs:
        DPUSH bootfsStr
        DPUSH bootfsLen
        mov qword [val__promptbool], 0
        call included
        mov qword [val__promptbool], 1
        call quit_prompt
        ret


;; --- more code ---

;; function things
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
        call loadbootfs
        call quit
        jmp coda


;;;; THIS IS THE EXIT POINT
coda:
        call bye
