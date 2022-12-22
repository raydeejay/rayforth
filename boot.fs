#! /home/raydj/forth/rayforth/rayforth-launcher

( this is a test flag )
1234567890

: >NAME  8 + ;
: >CODE  8 + COUNT + ;

: RECURSE ['] (branch) COMPILE, LATEST @ >CODE , ; IMMEDIATE

: IF    ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE
: THEN  HERE SWAP ! ; IMMEDIATE

: FOR   ['] (for) COMPILE, HERE ; IMMEDIATE
: NEXT
  ['] (next) COMPILE,
  ['] I COMPILE,
  ['] 0= COMPILE,
  ['] (0branch) COMPILE, ,
  ['] (endfor) COMPILE,
; IMMEDIATE

: DODOES   R> R> SWAP >R ;
: (DOES>)  R>  LATEST @ >CODE COMPILE@ ;
: DOES>    ['] (DOES>) COMPILE, ['] DODOES COMPILE, ; IMMEDIATE

: CONSTANT CREATE , DOES> @ ;

2 CONSTANT TWO

: DO   ['] (do) COMPILE, HERE ; IMMEDIATE
: LOOP
  ['] (loop) COMPILE,
  ['] I COMPILE,
  ['] (limit) COMPILE,
  ['] = COMPILE,
  ['] (0branch) COMPILE, ,
  ['] (enddo) COMPILE,
; IMMEDIATE

: BEGIN HERE ; IMMEDIATE

: UNTIL
  ['] 0= COMPILE,
  ['] (0branch) COMPILE, ,
; IMMEDIATE

: AGAIN
  ['] (branch) COMPILE, ,
; IMMEDIATE

: WHILE    ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE

: REPEAT
  SWAP ['] (branch) COMPILE, ,
  HERE SWAP !
; IMMEDIATE


: WORDS
  LATEST @
  BEGIN
    DUP >NAME
    COUNT  $7F AND  TYPE BL EMIT
    @ DUP 0<>
  UNTIL
;

( print the test flag )
. CR
