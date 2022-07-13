;; this is a forth written in assembly... or at least it tries to be

        CELLSIZE equ 4

%macro DPUSH 1
        mov dword [ebp], %1
        add ebp, CELLSIZE
%endmacro
        
%macro DPOP 1
        sub ebp, CELLSIZE
        mov dword %1, [ebp]
%endmacro
        
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
DATASTACK:
        resb CELLSIZE*64
PAD:
        resb 128
CURRENTWORD:
        resb 128

;; the program code here
SECTION .text
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

        mov ebp, DATASTACK
        mov edi, ebp
        mov ecx, CELLSIZE*64
        mov al, 0
        rep stosb

        ret

;; primitives
;; EMIT
asmemit:
        pop eax
        pop ecx
        push eax
        mov eax, 4
        mov ebx, 1
        mov edx, 1
        int 0x80
        ret

;; function things
display:
        mov eax, 4
        mov ebx, 1
        mov ecx, PAD
        mov edx, 128
        int 0x80
        ret

;; TYPE
asmtype:
        pop eax
        pop edx
        pop ecx
        push eax
        mov eax, 4
        mov ebx, 1
        int 0x80
        ret

hello:
        push helloStr
        push helloLen
        call asmtype
        ret



;; Outer interpreter stuff
read:
        ret
quit:
        call read
        ret

;;;; THIS IS THE ENTRY POINT
_start:
        call init
        call hello

        call display

;;;; THIS IS THE EXIT POINT
coda:
        mov eax, 1
        mov ebx, 0
        int 0x80
