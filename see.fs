: (see-call)  ( addr -- addr' )
  XT>NAME COUNT TYPE                    \ display name
  4 +                                   \ next adddress
;

: (see-lit)  ( addr -- addr' )
  16 OVER @ HEX U0.R DECIMAL            \ display callee address
  8 +                                   \ next adddress
;

: (see-string)  ( addr -- addr' )
  DUP COUNT SPACE TYPE                  \ display string
  COUNT +                               \ next adddress
;

: (see-0branch)  ( addr -- addr' )
  S" LIT  " TYPE
  16 OVER @ HEX U0.R DECIMAL            \ display callee address
  8 +                                   \ next adddress
;

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
    ['] (s") OVER = IF  (see-call) (see-string) TRUE EXIT  THEN
    \ otherwise print as a call
    (see-call) TRUE EXIT
  THEN
  FALSE
;

: SEE ( "name" -- )
  BL WORD FIND 0= IF DROP EXIT THEN
  \ S" Address: " TYPE 8 OVER U0.R
  \ CELL+ DUP C@ $80 AND IF  SPACE S" IMMEDIATE" TYPE  THEN CR
  \ COUNT  2DUP TYPE  +
  \ 256 dump
  BEGIN (see) CR WHILE REPEAT DROP
;
