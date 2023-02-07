#! /home/raydj/forth/rayforth/rayforth-launcher

CREATE builtins---->

: INCLUDE  ( "filename" -- )  BL WORD COUNT INCLUDED ;

: EXIT $C2 C, 0 C, 0 C, ; IMMEDIATE

: HEX       16 BASE ! ;
: DECIMAL   10 BASE ! ;
: BINARY     2 BASE ! ;

: CELL+  ( addr -- addr' ) 8 + ;
: CELLS  ( n -- n*cellsize ) 8 * ;

: >FLAGS  CELL+ ;
: >NAME   >FLAGS 1+ ;
: >CODE   >NAME COUNT + ;

: RECURSE ['] (branch) COMPILE, LATEST @ >CODE , ; IMMEDIATE

: DODOES   R> R> SWAP >R ;
: (DOES>)  R>  LATEST @ >CODE COMPILE@ ;
: DOES>    ['] (DOES>) COMPILE, ['] DODOES COMPILE, ; IMMEDIATE

0 ,                             ( sigh... )

: IF    ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE
: ELSE  ['] (branch)  COMPILE, HERE 0 ,  HERE ROT ! ; IMMEDIATE
: THEN  HERE SWAP ! ; IMMEDIATE
: ?IF   ['] ?DUP COMPILE, ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE

: CONSTANT CREATE , DOES> @ ;
: VARIABLE CREATE 0 , ;

\ for some reason alignment seems to matter... this should help
: aligned  ( u-addr -- a-addr )  4 rshift 4 lshift 16 + ;
: align    ( -- )  here aligned dp ! ;

: 2@  ( addr -- u1 u2 )  dup cell+ @ swap @ ;
: 2!  ( u1 u2 addr -- )  tuck ! cell+ ! ;

: 2CONSTANT CREATE , , DOES> 2@ ;
: 2VARIABLE CREATE 0 , 0 , ;

: WITHIN ( u/n lo hi -- f ) OVER - >R - R> U< ;

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
: UNLOOP  ['] (enddo) COMPILE, ; IMMEDIATE
: LEAVE   ( not sure how to go about this right now...) ;

: BEGIN   HERE ; IMMEDIATE
: UNTIL   ['] (0branch) COMPILE, , ; IMMEDIATE
: AGAIN   ['] (branch) COMPILE, , ; IMMEDIATE
: WHILE   ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE
: ?WHILE  ['] ?DUP COMPILE, ['] (0branch) COMPILE, HERE 0 , ; IMMEDIATE
: REPEAT  SWAP ['] (branch) COMPILE, ,  HERE SWAP ! ; IMMEDIATE

\ the following logic is prone to false positives, but only if the
\ word name exceeds 32 characters, at which point you have a naming
\ problem, which is way worse
: XT>NAME  ( addr -- addr' )
  DUP >R BEGIN                          \ addr' | addr0
    1-                                  \ addr'-1 | addr0
    DUP C@ 1+                           \ addr'-1 len?+1 | addr0
    OVER + R@ =                         \ addr'-1 found? | addr0
  UNTIL
  R> DROP                               \ addr'
;

: XT>LINK  ( addr -- addr' )  XT>NAME 1 CELLS - 1- ;

INCLUDE localwords.fs

: WORDS
  LATEST @
  BEGIN
    DUP >NAME
    COUNT  TYPE BL EMIT
    @ DUP 0=
  UNTIL
  DROP
;

: LITERAL  ( x -- ) ['] LIT COMPILE, , ; IMMEDIATE

: CHAR  BL WORD 1 + C@ ;
: [CHAR] BL WORD 1 + C@ POSTPONE LITERAL ; IMMEDIATE

\ some VT100 stuff
: <ESC>  27 EMIT ;
: <CSI>  [CHAR] [ EMIT ;

: PAGE   <ESC> <CSI> [CHAR] H EMIT
         <ESC> <CSI> [CHAR] 2 EMIT [CHAR] J EMIT ;

: UNDER+  ( a b c -- a+c b )  ROT + SWAP ;
: SPACE   ( -- )  BL EMIT ;
: SPACES  ( n -- )  DUP 0 > IF  0 DO  BL EMIT  LOOP  EXIT THEN  DROP ;
: ZEROS   ( n -- )  DUP 0 > IF  0 DO  '0 EMIT  LOOP  EXIT THEN  DROP ;

CREATE <pno> 256 ALLOT LOCAL
VARIABLE #<pno> LOCAL
VARIABLE sign? LOCAL

: <# ( u/n -- u )  <pno> 256 ERASE  256 #<pno> !  dup 0 < sign? ! abs ;
: #  ( u1 -- u2 )
  0  BASE @ UM/MOD  SWAP BASEDIGITS + c@  <pno> #<pno> @ +  C!
  -1 #<pno> +!
;
: #S ( u1 -- 0 )  BEGIN  # DUP  WHILE REPEAT ;
: #> ( u -- c-addr u )  DROP  <pno> #<pno> @ +  256 #<pno> @ - ;
: HOLD  ( c -- )  <pno> #<pno> @ +  C!  -1 #<pno> +! ;
: SIGN  ( u -- u )  sign? @ 0 < IF  '- HOLD  THEN ;

: U.R  ( rlen u -- )
  TUCK  BEGIN  -1 UNDER+ BASE @ / DUP  WHILE REPEAT  DROP SPACES ..
;

: U0.R  ( rlen u -- )
  TUCK  BEGIN  -1 UNDER+ BASE @ / DUP  WHILE REPEAT  DROP ZEROS ..
;

: AT  ( x y -- )
  <ESC> <CSI> 1+ .. [CHAR] ; EMIT 1+ .. [CHAR] H EMIT
;

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

: (ior)  ( n -- ior ) 0 < ; LOCAL

: READ-FILE  ( c-addr u1 fid -- u2 ior )
  ROT SWAP 0 SYSCALL/3 DUP (ior)
;

: WRITE-FILE  ( c-addr u1 fid -- u2 ior )
  ROT SWAP 1 SYSCALL/3 DUP (ior)
;

: WRITE-LINE  ( c-addr u1 fid -- u2 ior )
  dup >R  ROT SWAP 1 SYSCALL/3 DUP (ior)
  10 1 PSP 1 cells + R> 1 SYSCALL/3 DUP (ior)
;

: FILE-POSITION  ( fid -- u ior )
  1 0 ROT 8 SYSCALL/3 DUP (ior)
;

: FILE-SIZE  ( fid -- u ior )
  DUP DUP FILE-POSITION DROP
  ROT 2 0 ROT 8 SYSCALL/3 >R
  ROT 8 SYSCALL/3 DROP
  DROP R> DUP (ior)
;

: REPOSITION-FILE  ( u fid -- ior )
  0 ( <-SEEK_SET ) -rot  8 SYSCALL/3 (ior)
;

\ only one buffer for now
local.start
CREATE <STRINGBUFFER> 256 ALLOT LOCAL

: <S">  ( "string" -- addr n )
  [CHAR] " WORD
  COUNT TUCK <STRINGBUFFER> C!
  SWAP <STRINGBUFFER> 1 + SWAP CMOVE
  <STRINGBUFFER> COUNT
;
local.end

: (S")  ( R: addr -- addr u | R: addr> )  R>  COUNT  2DUP +  >R  ;

: S"  ( "string" -- addr n )
  STATE @ 0= IF  <S">  EXIT THEN
  ['] (S") COMPILE,  [CHAR] " WORD
  COUNT + DP !
; IMMEDIATE

: ."  ( "name" -- )  POSTPONE S"  ['] TYPE COMPILE, ; IMMEDIATE
: .(  ( "name" -- )  ') WORD COUNT TYPE ;

: (abort")  ( u -- )
  IF  S" ¯\_(ツ)_/¯ <{ " TYPE TYPE S"  }" TYPE CR ABORT  THEN
  2DROP
; LOCAL

: ABORT"  ( "msg" -- )
  ['] (S") COMPILE,  [CHAR] " WORD
  COUNT + DP !
  ['] ROT COMPILE,
  ['] (abort") COMPILE,
; IMMEDIATE

\ reads the newline too... :-/
: ACCEPT  ( c-addr +n1 -- +n2 )  0 read-file abort" ACCEPT error" 1- ;


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

: (dump)  ( addr -- )
  HEX
  8 OVER U0.R  SPACE DUP 16 + SWAP DO 2 I C@ U0.R SPACE LOOP
  DECIMAL CR
; LOCAL
: DUMP    ( addr n -- )  OVER + SWAP  DO  I (dump)  16 +LOOP ;

INCLUDE see.fs

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
  bl word find 0=  abort" word not found"
  xt>name 1 cells -  dup dp !  @ latest !
;

: VALUE CREATE , DOES> @ ;

: <TO>  ( "string" u -- )
  BL WORD FIND 0= ABORT" value not found"
  \ maybe should test if it's a value
  \ on the other hand, read what you write...?
  5 + !                         \ skip code, store in data
; LOCAL

: (TO)  ( u | R: addr -- R: addr> )
  R> DUP CELL+ >R  @ !
; LOCAL

: TO  ( "name" u -- )
  STATE @ 0= IF  <TO>  EXIT THEN
  ['] (TO) COMPILE,
  BL WORD FIND 0= ABORT" value not found"
  5 + ,
; IMMEDIATE

: <+TO>  ( "string" u -- )
  BL WORD FIND 0= ABORT" value not found"
  5 +  DUP @ UNDER+ !           \ skip code, store in data
; LOCAL

: (+TO)  ( u | R: addr -- R: addr> )
  R> DUP CELL+ >R  @  DUP @ UNDER+  !
; LOCAL

: +TO  ( "name" u -- )
  STATE @ 0= IF  <+TO>  EXIT THEN
  ['] (+TO) COMPILE,
  BL WORD FIND 0= ABORT" value not found"
  5 + ,
; IMMEDIATE

: DEFER  ( "name" -- )
  CREATE 0 ,                    \ maybe a default deferred instead?
  DOES> @ EXECUTE
;

\ BLOCKS code --------------------------------------------------
variable BLK
variable SCR
variable cbuf LOCAL

variable <blockfd> LOCAL

: USE  ( "name" -- )
  <blockfd> @ ?dup if  close-file abort" error closing blocks file" then
  0 <blockfd> !
  BL WORD COUNT r/w open-file abort" error opening blocks file"
  <blockfd> !
;

\ block -1 is not valid in a mapping, it means "no block"  (yes?)
create <mapping>  LOCAL 64 CELLS allot
create <buffers>  LOCAL 64 1024 * allot
<mapping> 64 cells -1 fill

\ bitvector to keep track of updated buffers
variable <updated> LOCAL
: BIT      ( u -- mask )  1 swap lshift ;
: CLEAR  ( mask addr -- )  tuck @ xor swap ! ;
: SET    ( mask addr -- )  tuck @ or  swap ! ;
: TEST   ( mask addr -- f )  tuck @ and 0<> ;

: buf>map  ( baddr -- maddr )  <buffers> - 128 / <mapping> + ; LOCAL
: map>buf  ( baddr -- maddr )  <mapping> - 128 * <buffers> + ; LOCAL
: map>bit  ( maddr -- ubit )   <mapping> - 8 / BIT ; LOCAL
: map>blk  ( maddr -- blk# )   @ ; LOCAL
: range    ( -- limit base )  <mapping> dup 64 cells under+ ; LOCAL
: updated? ( maddr -- f )  map>bit <updated> @ AND 0<> ; LOCAL

: read     ( u baddr -- )
  swap 1024 * <blockfd> @ reposition-file abort" error seeking blocks file"
  1024 <blockfd> @ READ-FILE ABORT" error reading blocks file"
  drop
; LOCAL

: write    ( maddr -- )
  dup  map>blk 1024 * <blockfd> @ reposition-file abort" error seeking blocks file"
  map>buf 1024 <blockfd> @ write-file abort" error writing blocks file"
  drop
; LOCAL

: mapping  ( u -- maddr/false )
  range DO
    DUP I @ = IF  drop I UNLOOP EXIT  THEN
  1 CELLS +LOOP
  drop FALSE
; LOCAL

: map    ( u maddr -- )  ! ; LOCAL

: unmap  ( maddr -- )
  dup updated? IF  dup write  THEN
  dup map>bit <updated> clear
  -1 swap !
; LOCAL

: BLOCK   ( u -- baddr )
  DUP mapping  ?DUP IF  nip map>buf  dup cbuf ! exit  then
  -1 mapping  ?DUP IF  2dup map  map>buf  tuck read  dup cbuf ! exit then
  \ unmap... but which block? always the first one?
  \ use some LRU strategy where mapping positions swap on access?
  <mapping>  dup unmap  2dup map  map>buf  tuck read  dup cbuf !
;

: BUFFER  ( u -- addr )
  DUP mapping  ?DUP IF  nip map>buf  dup cbuf ! exit  then
  -1 mapping  ?DUP IF  tuck map  map>buf  dup cbuf ! exit  then
  <mapping>  dup unmap  tuck map  map>buf  dup cbuf !
;

: EMPTY-BUFFERS  ( -- )  <mapping> 64 CELLS erase ;

: SAVE-BUFFERS   ( -- )
  range do  I updated? if  I write  then  1 cells +loop  0 <updated> !
;

: FLUSH   ( -- ) save-buffers <mapping> 64 cells erase ;

\ can be factored better, maybe... not right now :-)
\ block 0 cannot be LOADed, because BLK would be set to 0,
\ which makes the terminal the input source,
\ however it can be BLOCKed, LISTed, etc
: LOAD    ( i*x u -- j*x )
  dup 0= if  true abort" Cannot LOAD block 0"  then
  >R save-input R>
  dup  block  <sourceaddr> !  blk !
  1024 <sourcelen> !
  0 >in !
  interpret
  restore-input  0 blk !  0= abort" restore-input failed"
;

: THRU    ( i * x u1 u2 -- j * x ) 1+ swap DO  I load  LOOP ;

: UPDATE  ( -- )
  cbuf @ buf>map map>bit <updated> @ OR <updated> !
;

: LIST    ( u -- )
  dup SCR !
  block 16 0 DO  I 64 * OVER + 64 type cr  loop
  drop
;

\ extend backslash, REFILL, and EVALUATE (which we don't have yet)
: \  ( "some text" -- )
  BLK @ IF
    >in @   6 rshift 6 lshift   64 + >in ! exit
  THEN
  postpone \
; IMMEDIATE
\ BLOCKS code end ----------------------------------------------

include string.fs

CREATE needle 32 allot LOCAL
CREATE haystack 32 allot LOCAL

: WORDS.LIKE  ( "name" -- )
  BL WORD COUNT  needle c!  needle COUNT CMOVE
  needle COUNT upcase!

  LATEST @ BEGIN  ( linkaddr )
    DUP >NAME COUNT  haystack c!  haystack COUNT CMOVE
    haystack COUNT upcase!
    haystack COUNT needle COUNT SEARCH -ROT 2DROP
    IF  DUP >NAME COUNT type space  THEN
    @ DUP 0=
  UNTIL
;

\ fork and exec things
: fork  ( -- f )  $39 syscall/0 ;

CREATE <args>  LOCAL here 1024 dup allot erase
CREATE <args*> LOCAL here 16 cells dup allot erase
VARIABLE <argc> LOCAL
0 CONSTANT NULLENV LOCAL

: add-arg  ( c-addr u addr -- addr' )
  dup  <args*> <argc> @ cells +  ! \ set up the pointer in args*
  2dup 2>R                         \ save address and count
  swap cmove                       \ copy to <args>
  2R> +                            \ restore and move past
  0 over c! 1+                     \ store 0 and move past
  0 over c!                        \ store another 0, end of array
  1 <argc> +!                      \ increment arg count
; LOCAL

: exec  ( c-addrn un ... c-addr1 u1 argc c-addr u -- n )
  0 <argc> !  <args*> 16 cells erase
  <args> add-arg                   \ first arg is the pathname
  swap ?dup if
    0 do  add-arg  loop            \ copy args
  then  drop
  NULLENV <args*> <args>  $3B syscall/3
;

: (system)  ( c-addrn un ... c-addr1 u1 argc c-addr u -- )
  fork dup 0 < abort" error forking" if
    ( wait for child )
    2drop exit
  then
  exec ." error executing" bye     \ execute in forked process
;

: system  ( c-addrn un ... c-addr1 u1 argc "pathname" -- )
  BL WORD COUNT (system)
;

\ --- thoughts on non-buffered KEY and such -------------
\ use stty -icanon -echo before starting
\ use stty icanon echo after exiting
\ echoing will be disabled, but so will be buffering
\ not disabling echo causes double characters and oddness
\ anyway we need a way to see what's being typed
\ key will still be blocking but not buffered anymore
\ -------------------------------------------------------

: buffered    ( -- )
  s" icanon" s" echo" 2 s" /bin/stty" (system) drop 2drop 2drop
;

: unbuffered  ( -- )
  s" -icanon" s" -echo" 2 s" /bin/stty" (system) drop 2drop 2drop
;

\ key?
CREATE pollfd
0 c, 0 c, 0 c, 0 c,             \ fd
1 c, 0 c,                       \ events
0 c, 0 c,                       \ revents

: poll  ( timeout nfds fds* -- u )
  $07 syscall/3  dup -1 = abort" poll error"
;

: key?  ( -- f )  0 1 pollfd poll ;


\ almost ready to boot

\ Process all local words defined either in assembly or boot.fs. The
\ very first word defined is TRUE, so we take that address as the
\ start of local area.
' TRUE XT>LINK local.end

: HELLO
  S" boot.fs loaded" TYPE CR
  UNUSED . S" bytes available" TYPE CR
;

HELLO
