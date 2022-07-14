;; this is a forth written in assembly... or at least it tries to be

bits 64

;; Constants
        CELLSIZE equ 8

;; static data stuff
SECTION .data
align 8
        helloStr db "RayForth v0", 10
        helloLen equ $ -helloStr

        keytestStr db "Press a key and it will be printed back", 10
        keytestStrLen equ $ -keytestStr

        program db "65 emit 10 emit", 10
        programLen equ $ -program


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
        mov %1, [rbp]
        add rbp, CELLSIZE
%endmacro


;; Other memory zones
PAD:
        resb 128
CURRENTWORD:
        resb 128

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
        mov rax, val_ %+ %2
        DPUSH [rax]
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
        DPOP rax
        mov qword rbx, qword [rax]
        DPUSH rbx
        ret

.colon "!",store
        DPOP rax
        DPOP rbx
        mov qword [rax], qword rbx
        ret

.colon "SP@", spFetch
        mov rax, rbp
        DPUSH rax
        ret

.colon "RP@", rpFetch
        mov rax, rsp
        DPUSH rax
        ret

.colon "0=", zeroEqual
        DPOP rax
        test rax, rax
        pushf
        pop rax
        shr rax, 6+8
        and rax, 1
        mov rbx, 0
        sub rbx, rax
        DPUSH rbx
        ret

.colon "+", plus
        DPOP rax
        DPOP rbx
        add rax, rbx
        DPUSH rax
        ret

.colon "NAND", nand
        DPOP rax
        DPOP rbx
        and rax, rbx
        not rax
        DPUSH rax
        ret

.colon "EXIT", exit
        pop rax
        pop rbx
        push rax
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
        mov rax, rbp
        DPUSH rax
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
        mov rax, [rbp]
        DPUSH rax
        ret

.colon "SWAP", swap
        DPOP rax
        DPOP rbx
        DPUSH rax
        DPUSH rbx
        ret

.colon "*", multiply
        DPOP rax
        DPOP rbx
        mul rbx
        DPUSH rax
        ret

.colon "CR", cr
        DPUSH 10
        call emit
        ret

.colon "C@", cfetch
        DPOP rax
        xor rbx, rbx
        mov bl, [rax]
        DPUSH rbx
        ret

.colon "C!", cstore
        DPOP rax
        DPOP rbx
        mov [rax], bl
        ret

.variable "HERE", here, 77

;; the program code here
SECTION .text
align 8

global _start

;; perform various initialization stuff
init:
        mov rdi, PAD
        mov rcx, 128
        mov al, 65
        rep stosb

        mov rdi, CURRENTWORD
        mov rcx, 32
        mov al, 32
        rep stosb

        mov rdi, DATASTACK
        mov rcx, CELLSIZE*64
        mov al, 0
        rep stosb

        mov rbp, DATASTACKBOTTOM
        ret


;; --- more code ---
;; function things
display:
        DPUSH PAD
        DPUSH 128
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


;; Outer interpreter stuff

;;;; THIS IS THE ENTRY POINT
_start:
        call init
        call hello
        call testword
        call display

;;;; THIS IS THE EXIT POINT
coda:
        mov rax, 1
        mov rbx, 0
        int 0x80

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

        ; test fetch and store, copy 4 bytes of hello string
        ; to the pad
        DPUSH helloStr+4
        call fetch
        DPUSH PAD
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

        ; test C!, emit X and modifies PAD too
        DPUSH 88
        DPUSH PAD
        call cstore
        DPUSH PAD
        call cfetch
        call emit

        ; end of tests
        call cr
        ret
