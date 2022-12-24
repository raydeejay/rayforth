#! /home/raydj/forth/rayforth/rayforth-launcher

( this is a test flag )
\ 1234567890

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
\ . CR

: LITERAL  ( x -- ) ['] LIT COMPILE, , ; IMMEDIATE

: CHAR  BL WORD 1 + C@ ;
: [CHAR] BL WORD 1 + C@ POSTPONE LITERAL ; IMMEDIATE

\ some VT100 stuff
: <ESC>  27 EMIT ;
: <CSI>  [CHAR] [ EMIT ;

: PAGE  <ESC> <CSI> [CHAR] 2 EMIT [CHAR] J EMIT ;

\ a test word
: FOO  [ 3 4 + ] LITERAL . ;

: UNDER+  ( a b c -- a+c b )  ROT + SWAP ;
: SPACE   ( -- )  BL EMIT ;
: SPACES  ( n -- )  DUP 0 > IF  0 DO  BL EMIT  LOOP  EXIT THEN  DROP ;

: U.R  ( rlen u -- )
  TUCK  BEGIN  DUP  WHILE  -1 UNDER+ BASE @ /  REPEAT  DROP SPACES ..
;

: AT  ( x y -- )  <ESC> <CSI> .. [CHAR] ; EMIT .. [CHAR] H EMIT    ;

: ATTR:  ( "name" n -- )
  CREATE , DOES> <ESC> <CSI> @ EMIT [CHAR] m EMIT
;

CHAR 1 ATTR: <BOLD>
CHAR 2 ATTR: <LOW>
CHAR 4 ATTR: <UNDERLINE>
CHAR 5 ATTR: <BLINK>
CHAR 7 ATTR: <REVERSE>
CHAR 8 ATTR: <INVISIBLE>
