% `Sources.m4'
.PS
cct_init
textwid = 0.7
movewid = 2pt__
moveht = 0.275
define(`NL',`; move down $1; right_')
{
   {source     ; move ; "`{\tt source}'" ljust} NL
   {source(,I) ; move ; "`{\tt source(,I)}'" ljust} NL
   {source(,i) ; move ; "`{\tt source(,i)}'" ljust} NL
   {source(,V) ; move ; "`{\tt source(,V)}'" ljust} NL
   {source(,v) ; move ; "`{\tt source(,v)}'" ljust} NL
   {source(,AC); move ; "`{\tt source(,AC)}'" ljust} NL
   {source(,X) ; move ; "`{\tt source(,X)}'" ljust} NL
   {source(,F) ; move ; "`{\tt source(,F)}'" ljust} NL
   {source(,G) ; move ; "`{\tt source(,G)}'" ljust} NL
   {source(,Q) ; move ; "`{\tt source(,Q)}'" ljust}
}
   move right_ 2.0
{
   {source(,,0.4) ; move ;  "`{\tt source(,,0.4)}'" ljust} NL(0.35)
   {source(,P) ; move ; "`{\tt source(,P)}'" ljust} NL
   {source(,U) ; move ; "`{\tt source(,U)}'" ljust} NL
   {source(,R) ; move ; "`{\tt source(,R)}'" ljust} NL
   {source(,S) ; move ; "`{\tt source(,S)}'" ljust} NL
   {source(,T) ; move ; "`{\tt source(,T)}'" ljust} NL
   {source(,L) ; move ; "`{\tt source(,L)}'" ljust} NL
   {nullator   ; move ; "`{\tt nullator}'" ljust} NL
   {norator    ; move ; "`{\tt norator}'" ljust}
}
   move right_ 2.0
   {source(,"$\mu$A");move; "`{\tt source(,\"\$\char92mu\$A\")}'" ljust}NL(0.31)
   {consource     ; move ;  "`{\tt consource}'" ljust }    NL(0.32)
   {consource(,I) ; move ;  "`{\tt consource(,I)}'" ljust} NL(0.32)
   {consource(,i) ; move ;  "`{\tt consource(,i)}'" ljust} NL(0.32)
   {consource(,V) ; move ;  "`{\tt consource(,V)}'" ljust} NL(0.32)
   {consource(,v) ; move ;  "`{\tt consource(,v)}'" ljust} NL(0.3)
   {battery       ; move ;  "`{\tt battery}'" ljust}       NL
   {battery(,3,R) ; move ;  "`{\tt battery(,3,R)}'" ljust}

.PE
