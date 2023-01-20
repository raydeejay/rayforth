\ Local words
\ Inspired by Carol Pruitt's article on Forth Dimensions V6N6
\ http://www.forth.org/fd/FD-V06N6.pdf

$40 constant LOCALBIT

: LOCAL        ( -- )  latest @ >FLAGS  DUP c@ LOCALBIT or SWAP c! ;
: local?       ( addr -- f )  >FLAGS  c@ LOCALBIT and ;

( local.start ) here

variable link.from LOCAL

: keep         ( addr -- addr' )  dup link.from ! @ ; LOCAL
: discard      ( addr -- addr' )  @ dup link.from @ ! ; LOCAL

: ?delink ( addr f -- addr' )
  dup  local? if  discard exit  then  keep
; LOCAL

: local.start  ( -- start-of-local-area )  here ;
: local.end    ( start-of-local-area -- )
  latest @  DUP link.from !
  @  begin  2dup <=  while  ?delink  repeat  2drop
;

local.end
