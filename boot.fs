: IF    LIT [ ' (0branch) , ] COMPILE, HERE 0 , ; IMMEDIATE
: THEN  HERE SWAP ! ; IMMEDIATE

: >CODE
    8 + COUNT + ;

: RECURSE  LIT [ ' (branch) , ] COMPILE, LATEST @ >CODE , ; IMMEDIATE
