( this is a test flag )
1234567890

: IF    ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE
: THEN  HERE SWAP ! ; IMMEDIATE

: >CODE
    8 + COUNT + ;

: RECURSE ['] (branch) COMPILE, LATEST @ >CODE , ; IMMEDIATE

: TESTTHIS  DUP 0= IF DROP EXIT THEN DUP . 1 - RECURSE ;
( 5 TESTTHIS )

: FOR   ['] (for) COMPILE, HERE ; IMMEDIATE
: NEXT
    ['] (next) COMPILE,
    ['] I COMPILE,
    ['] 0= COMPILE,
    ['] (0branch) COMPILE, ,
    ['] (endfor) COMPILE,
; IMMEDIATE

: FOO   5  FOR   I .   3 FOR 65 EMIT NEXT  NEXT  ;
( FOO )

: DODOES   R> R> SWAP >R ;
: (DOES>)  R>  LATEST @ >CODE COMPILE@ ;
: DOES>    ['] (DOES>) COMPILE, ['] DODOES COMPILE, ; IMMEDIATE

: CONSTANT CREATE , DOES> @ ;

2 CONSTANT TWO

( 11 22 TWO . . TWO . . )

( print the test flag )
.
