.PS
# `Sources.m4'
cct_init
textwid = 0.7
movewid = 2pt__
moveht = 0.275
define(`NL',`; move down $1; right_')
{
   {source     ; move ; "`source'" ljust} NL
   {source(,I) ; move ; "`source(,I)'" ljust} NL
   {source(,i) ; move ; "`source(,i)'" ljust} NL
   {source(,V) ; move ; "`source(,V)'" ljust} NL
   {source(,v) ; move ; "`source(,v)'" ljust} NL
   {source(,AC); move ; "`source(,AC)'" ljust} NL
   {source(,X) ; move ; "`source(,X)'" ljust} NL
   {source(,F) ; move ; "`source(,F)'" ljust} NL
   {source(,G) ; move ; "`source(,G)'" ljust} NL
   {source(,Q) ; move ; "`source(,Q)'" ljust}
}
   move right_ 2.0
{
   {source(,,0.4) ; move ;  "`source(,,0.4)'" ljust} NL(0.35)
   {source(,P) ; move ; "`source(,P)'" ljust} NL
   {source(,U) ; move ; "`source(,U)'" ljust} NL
   {source(,R) ; move ; "`source(,R)'" ljust} NL
   {source(,S) ; move ; "`source(,S)'" ljust} NL
   {source(,T) ; move ; "`source(,T)'" ljust} NL
   {source(,L) ; move ; "`source(,L)'" ljust} NL
   {nullator   ; move ; "`nullator'" ljust} NL
   {norator    ; move ; "`norator'" ljust}
}
   move right_ 2.0
   {source(,"uA");move; "`source(,\"uA\")'" wid 1.2 ljust}NL(0.31)
   {consource     ; move ;  "`consource'" ljust }    NL(0.32)
   {consource(,I) ; move ;  "`consource(,I)'" ljust} NL(0.32)
   {consource(,i) ; move ;  "`consource(,i)'" ljust} NL(0.32)
   {consource(,V) ; move ;  "`consource(,V)'" ljust} NL(0.32)
   {consource(,v) ; move ;  "`consource(,v)'" ljust} NL(0.3)
   {battery       ; move ;  "`battery'" ljust}       NL
   {battery(,3,R) ; move ;  "`battery(,3,R)'" ljust}

.PE
