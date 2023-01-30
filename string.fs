\ these words are likely better off being written in asm?...

local.start

0 VALUE caddr1 LOCAL
0 VALUE caddr2 LOCAL
0 VALUE u1 LOCAL
0 VALUE u2 LOCAL

: COMPARE  ( caddr1 u1 caddr2 u2 -- -1/0/1 )
  TO u2 TO caddr2  TO u1 TO caddr1

  BEGIN
    caddr2 count caddr1 count ( caddr2+1 c2 caddr1+1 c1 )
    ROT ( caddr2+1 caddr1+1 c1 c2 )

    2DUP < IF  2DROP 2DROP -1  EXIT  THEN
    > IF  2DROP 1  EXIT  THEN

    TO caddr1 TO caddr2
    -1 +TO u1  -1 +TO u2

    u1 0= IF  u2 IF  -1 EXIT  THEN  0 EXIT  THEN
    u2 0= IF  1 EXIT  THEN
  AGAIN
;

local.end

local.start

0 VALUE caddr1 LOCAL
0 VALUE caddr2 LOCAL
0 VALUE u1 LOCAL
0 VALUE u2 LOCAL

: SEARCH  ( caddr1 u1 caddr2 u2 -- caddr3 u3 f )
  2OVER  TO u1 TO caddr1 TO u2 TO caddr2  ( caddr1 u1 )

  BEGIN
    caddr1 u2 caddr2 u2 COMPARE 0= IF  2DROP caddr1 u2 TRUE EXIT  THEN
    1 +TO caddr1  -1 +TO u1
    u1 u2 <
  UNTIL
  FALSE
;

local.end

: upcase!  ( c-addr u -- )
  OVER + SWAP DO  I C@ 'a 'z 1+ WITHIN IF  -32 I c+!  THEN  LOOP
;
