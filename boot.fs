#! /home/raydj/forth/rayforth/rayforth-launcher

( this is a test flag )
\ 1234567890

: HEX       16 BASE ! ;
: DECIMAL   10 BASE ! ;
: BINARY     2 BASE ! ;

: CELL+  ( addr -- addr' ) 8 + ;
: CELLS  ( n -- n*cellsize ) 8 * ;

: S>D    ( u -- ud )   0 SWAP ;

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
: VARIABLE CREATE 0 , ;

: DO   ['] (do) COMPILE, HERE ; IMMEDIATE
: LOOP
  ['] (loop) COMPILE,
  ['] (0branch) COMPILE, ,
  ['] (enddo) COMPILE,
; IMMEDIATE
: +LOOP
  ['] (+loop) COMPILE,
  ['] (0branch) COMPILE, ,
  ['] (enddo) COMPILE,
; IMMEDIATE

: BEGIN   HERE ; IMMEDIATE
: UNTIL   ['] 0= COMPILE,  ['] (0branch) COMPILE, , ; IMMEDIATE
: AGAIN   ['] (branch) COMPILE, , ; IMMEDIATE
: WHILE   ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE
: REPEAT  SWAP ['] (branch) COMPILE, ,  HERE SWAP ! ; IMMEDIATE

: WORDS
  LATEST @
  BEGIN
    DUP >NAME
    COUNT  $7F AND  TYPE BL EMIT
    @ DUP 0<>
  UNTIL
  DROP
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

: UNDER+  ( a b c -- a+c b )  ROT + SWAP ;
: SPACE   ( -- )  BL EMIT ;
: SPACES  ( n -- )  DUP 0 > IF  0 DO  BL EMIT  LOOP  EXIT THEN  DROP ;
: ZEROS   ( n -- )  DUP 0 > IF  0 DO  [CHAR] 0 EMIT  LOOP  EXIT THEN  DROP ;

CREATE <pno> 256 ALLOT
VARIABLE #<pno>

\ these should respect BASE, rewrite (the CHAR 0 + part)
: <# ( -- )  <pno> 256 ERASE  255 #<pno> ! ;
: #  ( ud1 -- ud2 )
  BASE @ UM/MOD  [CHAR] 0 +  <pno> #<pno> @ +  C!
  -1 #<pno> +!  S>D
;
: #S ( ud1 -- 0 0 )  BEGIN  # DUP  WHILE REPEAT  ;
: #> ( xd -- c-addr u )  2DROP <pno> 256 + #<pno> @ - #<pno> @ ;


: U.R  ( rlen u -- )
  TUCK  BEGIN  -1 UNDER+ BASE @ / DUP  WHILE REPEAT  DROP SPACES ..
;

: U0.R  ( rlen u -- )
  TUCK  BEGIN  -1 UNDER+ BASE @ / DUP  WHILE REPEAT  DROP ZEROS ..
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

0 CONSTANT BLACK
1 CONSTANT RED
2 CONSTANT GREEN
3 CONSTANT YELLOW
4 CONSTANT BLUE
5 CONSTANT MAGENTA
6 CONSTANT CYAN
7 CONSTANT WHITE
9 CONSTANT NOCOLOR

: FG  ( n -- )  <ESC> <CSI> 3 .. .. [CHAR] m EMIT ;
: BG  ( n -- )  <ESC> <CSI> 4 .. .. [CHAR] m EMIT ;

: (ior)  ( n -- ior ) 0 < ;

: READ-FILE  ( c-addr u1 fid -- u2 ior )
  ROT SWAP 0 SYSCALL/3 DUP (ior)
;

: WRITE-FILE  ( c-addr u1 fid -- u2 ior )
  ROT SWAP 1 SYSCALL/3 DUP (ior)
;

: WRITE-LINE  ( c-addr u1 fid -- u2 ior )
  ROT SWAP 1 SYSCALL/3 DUP (ior)
  BL  1 SYSCALL/3 DUP (ior)
;

\ fix to make fully compliant...?

: FORTH-READ-LINE  ( c-addr u1 fileid -- u2 flag ior )
  ROT SWAP 0 -ROT            ( u1 0 c-addr fileid )
  BEGIN
    2DUP 1 SWAP              ( u1 0 c-addr fileid c-addr 1 fileid )
    READ-FILE ( u1 0 c-addr fileid u2 ior )
    IF  >R 2DROP 2DROP R> DUP TRUE EXIT  THEN DROP
    ( u1 0 c-addr fileid u2 )
  WHILE  ( u1 0 c-addr fileid )                          \ if no chars read, exit
      OVER C@ 10 =  IF  2DROP NIP TRUE FALSE EXIT  THEN  \ if newline exit
      1 UNDER+ 2SWAP 1+ ( c-addr+1 fileid u1 0+1 )
      2DUP =  IF  DROP -ROT 2DROP TRUE FALSE EXIT  THEN   \ if max chars exit
      2SWAP             ( u1 0+1 c-addr+1 fileid )

      SWAP COUNT 10 =  IF  2DROP NIP TRUE FALSE EXIT  THEN  \ if newline exit
      SWAP 2SWAP 1+     ( c-addr+1 fileid u1 0+1 )
      2DUP =  IF  DROP -ROT 2DROP TRUE FALSE EXIT  THEN   \ if max chars exit
      2SWAP             ( u1 0+1 c-addr+1 fileid )
  REPEAT
  2DROP 2DROP 0 FALSE FALSE
;

: FILE-POSITION  ( fid -- ud ior )
  1 0 ROT 8 SYSCALL/3 S>D DUP (ior)
;

: FILE-SIZE  ( fid -- ud ior )
  DUP DUP FILE-POSITION DROP
  ROT 2 0 ROT 8 SYSCALL/3 >R
  ROT 8 SYSCALL/3 DROP
  DROP R> S>D DUP (ior)
;

: REPOSITION-FILE  ( ud fid -- ior )
  ( use the sure-to-be 0 as SEEK_SET )
  8 SYSCALL/3 (ior)
;

\ only one buffer for now
CREATE <STRINGBUFFER> 256 ALLOT

: <S">  ( "string" -- addr n )
  [CHAR] " WORD
  COUNT TUCK <STRINGBUFFER> C!
  SWAP <STRINGBUFFER> 1 + SWAP CMOVE
  <STRINGBUFFER> COUNT
;

: (S")  ( R: addr -- R: addr> )  R>  COUNT  2DUP +  >R  ;

: S"  ( "string" -- addr n )
  STATE @ 0= IF  <S">  EXIT THEN
  ['] (S") COMPILE,  [CHAR] " WORD
  COUNT + DP !
; IMMEDIATE

: (abort")  S" ¯\_(ツ)_/¯ <{ " TYPE TYPE S"  }" TYPE ABORT ;

: ABORT"  ( "msg" -- )
  ['] (S") COMPILE,  [CHAR] " WORD
  COUNT + DP !
  \ ['] S" LITERAL ['] COMPILE, COMPILE,
  ['] (abort") COMPILE,
; IMMEDIATE


\ format is:
\ TIBDATA len  >IN @  0    4  for stdin
\ addr len     >IN @  fid  4  for files

: SAVE-INPUT     ( -- xn..x1 n )
  \ both stdin and file have the same number and kind of args
  SOURCE  >IN @  SOURCE-ID @  4
;

: RESTORE-INPUT  ( nx..x1 n -- f )
  \ both stdin and file have the same number and kind of args
  DROP
  SOURCE-ID ! >IN ! <sourcelen> ! <sourceaddr> !
  TRUE
;

: FORTH-INCLUDED ( addr n -- )
  \ save current input specification ( addr size >in source-id n )
  SAVE-INPUT >R >R >R >R >R
  \ open the file
  R/O OPEN-FILE ( ior ) DROP
  \ store the fid in source-id
  SOURCE-ID !
  \ make the file the input source (??)
  1024 DUP <sourcelen> !
  ALLOCATE ( ior ) DROP <sourceaddr> !

  \ store 0 in BLK
  0 BLK !

  \ repeat until eof
  \   read line into input buffer, set >in to 0,  interpret
  BEGIN
    REFILL
  WHILE
      0 >IN !  INTERPRET
  REPEAT

  \ free buffer
  SOURCE DROP FREE ( ior ) DROP
  \ close file
  \ restore input specification
  R> R> R> R> R> RESTORE-INPUT ( flag ) DROP
;

: (dump)  ( addr -- )
  HEX
  8 OVER U0.R  SPACE DUP 16 + SWAP DO 2 I C@ U0.R SPACE LOOP
  DECIMAL CR ;
: DUMP    ( addr n -- )  OVER + SWAP  DO  I (dump)  16 +LOOP ;

: (fetch32) ( addr -- u32 )
  0 SWAP
  32 0 DO
    COUNT I LSHIFT UNDER+
  8 +LOOP
  DROP
  DUP $80000000 AND IF  $FFFFFFFF00000000 OR  THEN \ sign extend
;


: XT>NAME  ( addr -- addr' )
  DUP >R BEGIN                          \ addr' | addr0
    DUP C@ $7F AND 1+                   \ addr' len?+1 | addr0
    OVER + R@ <>                        \ addr' notfound? | addr0
  WHILE
      1-                                \ addr'-1 | addr0
  REPEAT
  R> DROP                               \ addr'
;

s" see.fs" INCLUDED

: args   ( -- args-addr )  rp0@ 3 cells + ;
: nargs  ( -- n )  rp0@ 2 cells + @ ;
: ctype  ( cstr -- )  begin count dup while emit repeat 2drop ;
: ?args  ( -- )
  nargs dup .
  args swap 0 do
    dup @ ctype space cell+
  loop drop
;

: forget ( "name" -- )
  bl word find 0= if abort" word not found" then
  xt>name 1 cells -  dup dp !  @ latest !
;

: HELLO
  S" boot.fs loaded" TYPE CR
  UNUSED . S" bytes available" TYPE CR
;

HELLO
