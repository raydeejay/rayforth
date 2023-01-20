local.start

: (fetch32) ( addr -- u32 )
  0 SWAP
  32 0 DO
    COUNT I LSHIFT UNDER+
  8 +LOOP
  DROP
  DUP $80000000 AND IF  $FFFFFFFF00000000 OR  THEN \ sign extend
; LOCAL

: (see-call)  ( addr xt -- addr' )
  XT>NAME COUNT TYPE SPACE      \ display name
  4 +                                   \ next adddress
; LOCAL

: (see-lit)  ( addr -- addr' )
  16 OVER @ HEX U0.R DECIMAL            \ display callee address
  8 +                                   \ next adddress
; LOCAL

: (see-string)  ( addr -- addr' )
  DUP COUNT
  [CHAR] " EMIT TYPE [CHAR] " EMIT      \ display string
  COUNT +                               \ next adddress
; LOCAL

: (see-absolute)  ( addr -- addr' )
  5 - 8 - DUP @ (see-call)              \ display name
  4 + 5 +                               \ next address
; LOCAL

: (see)  ( addr -- addr' f )
  16 OVER HEX U0.R DECIMAL SPACE        \ display address
  DUP C@ $E8 = IF
    1+                         \ move to next position
    DUP (fetch32)              \ read next 4 bytes which are an offset
    OVER 4 + +                 \ calculate xt
    \ check xt against LIT and friends
    \ special case those
    ['] LIT OVER = IF  DROP 4 + S" LIT " TYPE (see-lit) TRUE EXIT  THEN
    ['] (0branch) OVER = IF  (see-call) (see-lit) TRUE EXIT  THEN
    ['] (branch) OVER = IF  (see-call) (see-lit) TRUE EXIT  THEN
    ['] (s") OVER = IF  (see-call) (see-string) TRUE EXIT  THEN
    ['] COMPILE, OVER = IF  (see-call) (see-absolute) TRUE EXIT  THEN
    \ otherwise print as a call
    (see-call) TRUE EXIT
  THEN
  DUP C@ $C2 = IF
    1+ COUNT SWAP COUNT ROT + ABORT" unexpected $C2 operand"
    S" EXIT" TYPE TRUE EXIT
  THEN
  DUP C@ $C3 = IF  S" ;" TYPE 1+ FALSE EXIT THEN
  FALSE
; LOCAL

: SEE ( "name" -- )
  BL WORD FIND 0= IF DROP EXIT THEN
  \ S" Address: " TYPE 8 OVER U0.R
  \ CELL+ DUP C@ $80 AND IF  SPACE S" IMMEDIATE" TYPE  THEN CR
  \ COUNT  2DUP TYPE  +
  \ 256 dump
  BEGIN (see) CR WHILE REPEAT DROP
;

local.end
