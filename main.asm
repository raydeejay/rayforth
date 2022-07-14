;; this is a forth written in assembly... or at least it tries to be

;; Constants
        CELLSIZE equ 4

;; static data stuff
SECTION .data
align 4
        helloStr db "RayForth v0", 10
        helloLen equ $ -helloStr

        program db "65 emit 10 emit", 10
        programLen equ $ -program


;; here's where these things go, apparently
SECTION .bss
align 4

;; Parameter stack
        DATASTACK resb CELLSIZE*64
        DATASTACKBOTTOM equ $ - CELLSIZE

;; Parameter Stack Macros
%macro DPUSH 1
        sub ebp, CELLSIZE
        mov [ebp], dword %1
%endmacro

%macro DPOP 1
        mov %1, [ebp]
        add ebp, CELLSIZE
%endmacro


;; Other memory zones
PAD:
        resb 128
CURRENTWORD:
        resb 128

;; dictionary here?

;; the program code here
SECTION .text
align 4

global _start

;; perform various initialization stuff
init:
        mov edi, PAD
        mov ecx, 128
        mov al, 65
        rep stosb

        mov edi, CURRENTWORD
        mov ecx, 32
        mov al, 32
        rep stosb

        mov edi, DATASTACK
        mov ecx, CELLSIZE*64
        mov al, 0
        rep stosb

        mov ebp, DATASTACKBOTTOM
        ret

;; primitives
;; @ (FETCH)
fetch:
        DPOP eax
        mov ebx, [eax]
        DPUSH ebx
        ret

;; ! (STORE)
store:
        DPOP eax
        DPOP ebx
        mov [eax], ebx
        ret

;; EMIT
emit:
        mov eax, ebp
        DPUSH eax
        DPUSH 1
        call asmtype
        add ebp, CELLSIZE
        ret

;; SP@
spFetch:
        mov eax, ebp
        DPUSH eax
        ret

;; RP@
rpFetch:
        mov eax, esp
        DPUSH eax
        ret

;; 0=
zeroEqual:
        DPOP eax
        test eax, eax
        lahf
        shr eax, 6+8
        and eax, 1
        mov ebx, 0
        sub ebx, eax
        DPUSH ebx
        ret

;; +
plus:
        DPOP eax
        DPOP ebx
        add eax, ebx
        DPUSH eax
        ret


;; --- more code ---
;; TYPE
asmtype:
        mov eax, 4
        mov ebx, 1
        DPOP edx
        DPOP ecx
        int 0x80
        ret

;; function things
display:
        DPUSH PAD
        DPUSH 128
        call asmtype
        call cr
        ret

hello:
        DPUSH helloStr
        DPUSH helloLen
        call asmtype
        ret

cr:
        DPUSH 10
        call emit
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
        mov eax, 1
        mov ebx, 0
        int 0x80

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
        DPOP eax
        add eax, '1'
        DPUSH eax
        call emit

        DPUSH 1
        call zeroEqual
        DPOP eax
        add eax, '1'
        DPUSH eax
        call emit

        ; test +, emit A
        DPUSH 60
        DPUSH 5
        call plus
        call emit

        call cr
        ret
