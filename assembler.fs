VARIABLE <ModR/M>
VARIABLE <REX>
VARIABLE <opcode1>
VARIABLE <opcode2>

\ ADDRESSING ------------------------------------------
\ type is 0 for immediate, 1 for register
: rax  1 0  ; : rcx  1 1  ; : rdx  1 2  ; : rbx  1 3  ;
: rsp  1 4  ; : rbp  1 5  ; : rsi  1 6  ; : rdi  1 7  ;
: r8   1 8  ; : r9   1 9  ; : r10  1 10 ; : r11  1 11 ;
: r12  1 12 ; : r13  1 13 ; : r14  1 14 ; : r15  1 15 ;

\ type 3 is register indirect
: [rax]  3 0  ; : [rcx]  3 1  ; : [rdx]  3 2  ; : [rbx]  3 3  ;
: [rsp]  3 4  ; : [rbp]  3 5  ; : [rsi]  3 6  ; : [rdi]  3 7  ;
: [r8]   3 8  ; : [r9]   3 9  ; : [r10]  3 10 ; : [r11]  3 11 ;
: [r12]  3 12 ; : [r13]  3 13 ; : [r14]  3 14 ; : [r15]  3 15 ;

\ type 2 for memory
: mem   ( u -- 2 u )  2 swap ;

: rex.w <REX> c@ %01001000 or <REX> c! ;
: rex.r <REX> c@ %01000100 or <REX> c! ;
: rex.x <REX> c@ %01000010 or <REX> c! ;
: rex.b <REX> c@ %01000001 or <REX> c! ;

: ModR/M.mod!  ( bb -- )   %11 and 6 LSHIFT <ModR/M> c@ or <ModR/M> c! ;
: ModR/M.reg! ( bbb -- )  %111 and 3 LSHIFT <ModR/M> c@ or <ModR/M> c! ;
: ModR/M.rm!  ( bbb -- )  %111 and <ModR/M> c@ or <ModR/M> c! ;

\ COMPILING WORDS ----------------------------------------
: rex,            ( -- )  <REX> c@ dup if  dup c,  then drop ;
: ModR/M,         ( -- )  <ModR/M> c@ c, ;
: reset-assembler ( -- )  0 <ModR/M>  ! 0 <REX> !  0 <opcode1> ! 0 <opcode2> ! ;
: assemble/1      ( -- )  rex,  <opcode1> c@ c,  ModR/M,  reset-assembler ;

: assemble/2  ( -- )
  rex,  <opcode1> c@ c, <opcode2> c@ c,       \ compile 2-byte opcode
  ModR/M,  reset-assembler
;

: address32,  ( addr32 -- )
  dup c,  8 rshift dup c,  8 rshift dup c,  8 rshift c,
;

: >ModR/M.rm  ( u -- )  dup 7 > if  rex.b  then  ModR/M.rm! ;
: >ModR/M.reg ( u -- )  dup 7 > if  rex.r  then  ModR/M.reg! ;

\ INSTRUCTIONS -------------------------------------
: ret, $C3 c, ; immediate

: inc,  ( spec dst -- )
  \ figure out rex.w prefix
  rex.w                                 \ always 64-bit operand
  \ rearrange parameters
  swap
  \ validate operand
  dup 0= if  abort" operand cannot be immediate"  then
  \ register operand
  dup 1 = over 3 = or if
    \ figure out ModR/M
    1 = if  %11 ModR/M.mod! then    \ 11 direct
    >ModR/M.rm
    \ assemble instruction
    $FF <opcode1> c! assemble/1         \ always opcode FF 01, 64-bit
    exit                           \ drop spec and finish
  then
  \ memory operand
  dup 2 = if
    drop
    \ figure out ModR/M (ModR/M $25 means index=RSP,base=RBP, so 0
    %00 ModR/M.mod!                     \ indirect
    %100 ModR/M.reg!                    \ RSP INDEX
    %101 ModR/M.rm!                     \ RBP BASE
    \ RSP cannot be used as index, RBP cannot be used as base, so in the end
    \ it means the disp32 follows the ModR/M byte

    \ for memory address, we have to encode it as 32-bit offset from
    \ an index... not sure how the index is specified
    \ anyway, the opcode is 'FF 04' in that case, followed
    \ by a ModR/M byte of $25, then the disp32 (from... 0?) follows

    \ assemble instruction
    \ always opcode FF 04, 64-bit
    $FF <opcode1> C! $04 <opcode2> C! assemble/2
    $FFFFFFFF and                    \ trim to 32 bits because science
    address32,                       \ write address32
    exit                             \ finish
  then
  abort" invalid parameters"
; immediate

: add,  ( type src type dst -- )
  \ figure out rex.w prefix
  rex.w                               \ always 64-bit operands
  $01 <opcode1> C!                    \ $01 unless the src is indirect
  \ rearrange parameters             ( srcspec src dstspec dst )
  2swap swap -rot swap 2swap swap    ( src dst srcspec dstspec )
  \ validate operands
  dup 0= if  abort" destination cannot be immediate"  then
  dup 2 = if  abort" cannot encode dst address yet"  then
  over 0= if  abort" source cannot be immediate yet"  then
  over 2 = if  abort" cannot encode src address yet"  then
  dup 1 = if                      \ dst is direct
    drop                          \ drop dstspec ( src dst srcspec )
    dup 1 = if  %11 ModR/M.mod!  then  \ src is direct
    3 = if  swap $03 <opcode1> c!  then \ src is indirect, swap src/dst
    >ModR/M.rm  >ModR/M.reg  assemble/1
    exit
  then
  dup 3 = if                            \ dst is indirect
    drop                                \ drop dstspec
    3 = if  abort" two indirect operands"  then
    >ModR/M.rm  >ModR/M.reg  assemble/1
    exit
  then
  abort" wat"
; immediate

: sub,  ( type src type dst -- )
  \ figure out rex.w prefix
  rex.w                               \ always 64-bit operands
  $29 <opcode1> C!                    \ $01 unless the src is indirect
  \ rearrange parameters             ( srcspec src dstspec dst )
  2swap swap -rot swap 2swap swap    ( src dst srcspec dstspec )
  \ validate operands
  dup 0= if  abort" destination cannot be immediate"  then
  dup 2 = if  abort" cannot encode dst address yet"  then
  over 0= if  abort" source cannot be immediate yet"  then
  over 2 = if  abort" cannot encode src address yet"  then
  dup 1 = if                      \ dst is direct
    drop                          \ drop dstspec ( src dst srcspec )
    dup 1 = if  %11 ModR/M.mod!  then  \ src is direct
    3 = if  swap $2B <opcode1> c!  then \ src is indirect, swap src/dst
    >ModR/M.rm  >ModR/M.reg  assemble/1
    exit
  then
  dup 3 = if                            \ dst is indirect
    drop                                \ drop dstspec
    3 = if  abort" two indirect operands"  then
    >ModR/M.rm  >ModR/M.reg  assemble/1
    exit
  then
  abort" wat"
; immediate

variable timmy

: one+  ( n -- n+1 )  [ r15 ] inc, ;
: dupe  ( n -- n+n )  [ r15 r15 ] add, ;
\ : sub   ( n -- n )   [ r15 r14 ] add, [ r14 ] inc, [ r14 r15 ] sub, ;

: oneup ( -- ) [ timmy mem ] inc, ;
: addrup    [ [r15] ] inc, ;
