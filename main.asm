;; this is a forth written in assembly... or at least it tries to be

;; Constants
        CELLSIZE equ 4

;; static data stuff
SECTION .data
align 4
        helloStr db "RayForth v0", 10
        helloLen equ $ -helloStr

        keytestStr db "Press a key and it will be printed back", 10
        keytestStrLen equ $ -keytestStr

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
;; colon definition   6 SQUARE link flags?    call DUP   call *   RET
;;  code definition   3 DUP  link flags?   mov ebp, eax   DPUSH eax  RET

%macro .entry 5
%1_entry:
        db %2, %3
        dd %4
        db %5
%1:
%endmacro

SECTION mysection,EWR
DICTIONARY:
;; primitives
;; @ (FETCH)
.entry fetch,1,"@",0,0
        DPOP eax
        mov ebx, [eax]
        DPUSH ebx
        ret

;; ! (STORE)
.entry store,1,"!",fetch_entry,0
        DPOP eax
        DPOP ebx
        mov [eax], ebx
        ret

;; SP@
spFetch_entry:
        db 3, "SP@"
        dd store_entry
        db 0
spFetch:
        mov eax, ebp
        DPUSH eax
        ret

;; RP@
rpFetch_entry:
        db 3, "RP@"
        dd spFetch_entry
        db 0
rpFetch:
        mov eax, esp
        DPUSH eax
        ret

;; 0=
zeroEqual_entry:
        db 2, "0="
        dd rpFetch_entry
        db 0
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
plus_entry:
        db 1, "+"
        dd zeroEqual_entry
        db 0
plus:
        DPOP eax
        DPOP ebx
        add eax, ebx
        DPUSH eax
        ret

;; NAND
nand_entry:
        db 4, "NAND"
        dd plus_entry
        db 0
nand:
        DPOP eax
        DPOP ebx
        and eax, ebx
        not eax
        DPUSH eax
        ret

;; EXIT
exit_entry:
        db 4, "EXIT"
        dd nand_entry
        db 0
exit:
        pop eax
        pop ebx
        push eax
        ret

;; KEY
;; ideally we should set the terminal to raw or something first
key_entry:
        db 3, "KEY"
        dd exit_entry
        db 0
key:
        sub ebp, 4
        mov eax, 3
        mov ebx, 1
        mov ecx, ebp
        mov edx, 1
        int 0x80
        ret

;; EMIT
emit_entry:
        db 4, "EMIT"
        dd key_entry
        db 0
emit:
        mov eax, ebp
        DPUSH eax
        DPUSH 1
        call type
        add ebp, CELLSIZE
        ret

;; end of SectorForth primitives, start of mine

;; TYPE
type_entry:
        db 4, "TYPE"
        dd emit_entry
        db 0
type:
        mov eax, 4
        mov ebx, 1
        DPOP edx
        DPOP ecx
        int 0x80
        ret

square_entry:
        db 6, "SQUARE"
        dd type_entry
        db 0
square:
        call dup
        call multiply
        ret

dup_entry:
        db 3, "DUP"
        dd square_entry
        db 0
dup:
        mov eax, [ebp]
        DPUSH eax
        ret

multiply_entry:
        db 1, "*"
        dd dup_entry
        db 0
multiply:
        DPOP eax
        DPOP ebx
        mul ebx
        DPUSH eax
        ret

cr_entry:
        db 2, "CR"
        dd multiply_entry
        db 0
cr:
        DPUSH 10
        call emit
        ret

.entry cfetch,2,"C@",cr_entry,0
        DPOP eax
        mov ebx, [eax]
        DPUSH bl
        ret

.entry cstore,2,"C!",cfetch_entry,0
        DPOP eax
        DPOP ebx
        mov [eax], bl
        ret

here_entry:
        db 4, "HERE"
        dd cstore_entry
        db 0
here_data:
        dd 0
here:
        DPUSH here_data
        ret

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
        mov eax, 1
        mov ebx, 0
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

        ; test NAND, emit O
        DPUSH 0xffffffff
        DPUSH 0xffffffb0
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
        call cfetch
        DPUSH '0'
        call plus
        call emit

        DPUSH ' '
        call emit

        DPUSH here_entry
        DPUSH 1
        call plus
        DPUSH 4
        call type

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
