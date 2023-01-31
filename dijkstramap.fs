\ let's define a map as uhhh 40x20
CREATE map here 40 20 * dup allot blank

: >map  ( x y map -- addr )  swap 40 * + + ;
: map@  ( x y map -- c )  >map c@ ;
: map!  ( c x y map -- )  >map c! ;

: maprow{  ( u "data" -- )
  40 * map +  '} word count  >R swap R>  cmove
;

0  maprow{ ########################################}
1  maprow{ #         #              #             #}
2  maprow{ #         #              #   X         #}
3  maprow{ #         #              #             #}
4  maprow{ #         #              #             #}
5  maprow{ #         #              #             #}
6  maprow{ #                        #             #}
7  maprow{ ###############          ##########  ###}
8  maprow{ #                                      #}
9  maprow{ #                                      #}
10 maprow{ #                                      #}
11 maprow{ #########                              #}
12 maprow{ #                                      #}
13 maprow{ #                 ######################}
14 maprow{ #                                      #}
15 maprow{ #                                      #}
16 maprow{ #                                      #}
17 maprow{ ##########                             #}
18 maprow{ #                                      #}
19 maprow{ ########################################}

\ To get a Dijkstra map, you start with an integer array representing
\ your map, with some set of goal cells set to zero and all the rest
\ set to a very high number.

\ Iterate through the map's "floor" cells -- skip the impassable wall
\ cells.

\ If any floor tile has a value greater than 1 regarding to its
\ lowest-value floor neighbour (in a cardinal direction - i.e. up,
\ down, left or right; a cell next to the one we are checking), set it
\ to be exactly 1 greater than its lowest value neighbor.

\ Repeat until no changes are made.

\ The resulting grid of numbers represents the number of steps that it
\ will take to get from any given tile to the nearest goal.

\ djikstra map stuff
CREATE dmap  here 40 20 * dup allot blank
CREATE dmap* here 40 20 * dup allot blank

: >dijk  ( c -- c' )
  dup BL = if drop 120 exit then
  dup '# = if drop 127 exit then
  dup 'X = if drop 0 exit then
;

: makedmap
  20 0 do
    40 0 do
      I J map map@  >dijk  I J dmap map!
    loop
  loop
;

: 3dup   ( u1 u2 u3 -- u1 u2 u3 u1 u2 u3 )  >R 2DUP R@ -ROT R> ;
: 3drop  ( u1 u2 u3 -- )  drop 2drop ;

local.start

: upneigh     ( x y addr -- c )  -rot 1- rot  map@ ; LOCAL
: downneigh   ( x y addr -- c )  -rot 1+ rot  map@ ; LOCAL
: leftneigh   ( x y addr -- c )  rot 1- -rot  map@ ; LOCAL
: rightneigh  ( x y addr -- c )  rot 1+ -rot  map@ ; LOCAL

: dijk@  ( x y dmap -- u f )
  \ we process only inside the border (caller responsibility)?
  3dup map@  >R                          \ store this cell's weight
  R@ 127 = if  3drop R> FALSE exit  then \ skip walls, keep value
  3dup upneigh >R                        \ store neighbour weight
  3dup downneigh >R                      \ store neighbour weight
  3dup leftneigh >R                      \ store neighbour weight
  3dup rightneigh >R                     \ store neighbour weight
  3drop                         \ we're done with the coords/map
  R> R> R> R> min min min       \ find lowest
  R>                            \ ( lowest current )
  2dup swap - 1 > if  drop 1+ TRUE exit  then \ cap higher at lowest+1
  nip FALSE                     \ otherwise keep current value
;

: iterate  ( -- f )
  FALSE
  20 0 do
    40 0 do
      I J dmap  dijk@ swap  I J dmap* map!  OR
    loop
  loop
  dmap* dmap 40 20 *  cmove
;

: .djik  ( c -- )
  dup 127 = if  drop '# emit exit  then
  BL + 126 min emit
; LOCAL

: whole    ( -- )
  0 0 at
  20 0 do
    40 0 do
      I J dmap map@ .djik
    loop cr
  loop
;

\ include dijkstramap.fs
\ makedmap
\ : foo begin  whole iterate  while  100 ms  repeat ;
\ foo
