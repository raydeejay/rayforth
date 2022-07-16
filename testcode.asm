testexitinner:
        DPUSH 'A'
        call emit
        call exit               ; exits from outer
        ret

testexitouter:
        call testexitinner
        DPUSH 'B'
        call emit
        ret

testword:
        ; test stack and emit, emit EDC
        DPUSH 'C'
        DPUSH 'D'
        DPUSH 'E'
        call emit
        call emit
        call emit
        call cr

        ; test fetch and store, copy 8 bytes of hello string (offset 4)
        ; to the pad ("orth V0", 10)
        DPUSH helloStr+4
        call fetch
        DPUSH PADDATA
        call store

        ; test 0=, emit 0 and 1
        DPUSH 0
        call zeroEqual
        DPOP rax
        add rax, '1'
        DPUSH rax
        call emit

        DPUSH 1
        call zeroEqual
        DPOP rax
        add rax, '1'
        DPUSH rax
        call emit

        ; test +, emit A
        DPUSH 60
        DPUSH 5
        call plus
        call emit

        ; test NAND, emit O
        DPUSH 0xffffffffffffffff
        DPUSH 0xffffffffffffffb0
        call nand
        call emit

        ; test exit, emit A but not B
        call testexitouter

        ; test for KEY
        DPUSH keytestStr
        DPUSH keytestStrLen
        call type
        call key
        call emit

        ; test SQUARE colon definition (but not the whole entry)
        ; also tests * and DUP code definitions
        ; emit C
        DPUSH 8
        call square
        DPUSH 3
        call plus
        call emit

        ; test HERE as a variable, emit E
        call here
        call fetch              ; save the value of here on the stack

        DPUSH '*'
        call emit

        DPUSH 69
        call here
        call store
        call here
        call fetch
        call emit

        call here
        call store              ; and restore the value of here
        call cr

        ; test dictionary structure
        DPUSH here_entry
        DPUSH CELLSIZE
        call plus
        call cfetch
        DPUSH '0'
        call plus
        call emit

        DPUSH ' '
        call emit

        DPUSH here_entry
        DPUSH CELLSIZE
        call plus
        DPUSH 1
        call plus
        DPUSH 4
        call type
        call cr

        ; more test for dictionary structure
        ; print the name of the word preceeding HERE
        DPUSH here_entry
        call fetch              ; address of the linked entry

        DPUSH CELLSIZE
        call plus               ; address of the count

        call dup
        call cfetch             ; get the count and move it out of the way
        call swap

        DPUSH 1                 ; increment the address
        call plus

        call swap

        call type               ; print the name
        call cr


        ; test C@, emit F from Forth
        DPUSH helloStr
        DPUSH 3
        call plus
        call cfetch
        call emit

        ; test C!, emit X and modifies PADDATA too
        DPUSH 88
        DPUSH PADDATA
        call cstore
        DPUSH PADDATA
        call cfetch
        call emit

        call cr
        ret

testword2:
        ; test REFILL, BL, WORD, and COUNT
        call refill
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr
        call bl_
        call word_
        call count
        call type
        DPUSH '.'
        call emit
        call cr

        ; end of tests
        call cr
        ret
