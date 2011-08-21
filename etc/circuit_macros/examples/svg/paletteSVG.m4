.PS
# This is the source for a prototype svg library of electric circuit
# elements.  Each element is a compound svg object with corners on grid points.
#
# Processing svg.m4 and this file thought dpic -v gives
# output that can be read into Inkscape.
#
define(`SIdefaults',`
 scale = 25.4
 arcrad = 5
 arrowht = 2.5
 arrowwid = arrowht/2
 boxht = 12
 boxrad = 0
 boxwid = 20
 circlerad = 5
 dashwid = 1.5
 ellipseht = 12
 ellipsewid = 16
 lineht = 12
 linewid = 12
 moveht = 12
 movewid = 12
 textht = ifsvg(4,0)
 textoffset = 1
 textwid = 0
')

ifdef(`cct_init',,`include(HOMELIB_`'libcct.m4)')
cct_init
ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')
SIdefaults

                             # Assume dpic scale is in mm
gunit = 1                    # Inkscape grid dimension
divert(-1)
define(`elen_',`(10*gunit)') # default 2-terminal element length
define(`dimen_',`(10*gunit)')# reference dimension for element bodies
define(`L_unit',`(2*gunit)') # logic gate grid size
define(`N_diam',1)           # L_unit size of `not' circles

                             # grid round and chop
define(`g_round',`(int((`$1')/gunit+100000.5   )-100000)*gunit')
define(`g_chop', `(int((`$1')/gunit+100000.9999)-100000)*gunit')
                             # snap to grid
define(`g_snap',`(g_round((`$1').x),g_round((`$1').y))')

# Box containing an element.  The element must be in a box that is
# intersected by a horizontal or vertical line from entry to exit.
define(`Gridbox',`[ command "<g>"
  S: Here; `$1'; F: Here
  if S.y == F.y then {
    move from (max(S.x,F.x),F.y+g_chop(last[].n.y-F.y)) \
           to (min(S.x,F.x),F.y-g_chop(F.y-last[].s.y)) } \
  else {
    move from (F.x+g_chop(last[].e.x-F.x),max(S.y,F.y)) \
           to (F.x-g_chop(F.x-last[].w.x),min(S.y,F.y)) }
  command "</g>" ] ')

# Place compound object corners at grid intersections
define(`Below',
  `with .n at g_snap(ifelse(`$1',,`last []',`$1').s)-(0,2*gunit)')
define(`Right',
  `with .w at g_snap(ifelse(`$1',,`last []',`$1').e)+(2*gunit,0)')

# for debug
define(`prpos',`print (`$1').x/gunit,(`$1').y/gunit')
divert(0)dnl

[ 

rpoint_(down_ elen_)
A:Gridbox( resistor )
  Gridbox( resistor(down_ dimen_*1/2) ) Right
  Gridbox( resistor(,,E) ) Right
  Gridbox( resistor(,,Q) ) Right
  Gridbox( resistor(,,H) ) Right
  Gridbox( resistor(,,N) ) Right
  Gridbox( inductor(,W) ) Right
  Gridbox( inductor(,W,,M) ) Right
  Gridbox( inductor(,L) ) Right
  Gridbox( inductor ) Right
  Gridbox( inductor(,,,M) ) Right
  Gridbox( capacitor ) Right
  Gridbox( capacitor(,C) ) Right
  Gridbox( capacitor(,P) ) Right
  Gridbox( capacitor(,E) ) Right
  Gridbox( capacitor(,K) ) Right
  Gridbox( diode ) Right
  Gridbox( diode(,K) ) Right
  Gridbox( diode(,Z) ) Right
  Gridbox( diode(,S) ) Right
  Gridbox( diode(,L) ) Right
  Gridbox( diode(,V) ) Right
  Gridbox( diode(,v) ) Right
  Gridbox( diode(,T) ) Right
  Gridbox( diode(,CR) ) Right
  Gridbox( diode(,D) ) Right
  Gridbox( diode(,B) ) Right
  Gridbox( diode(,LE) ) Right
  Gridbox( [circle diam dimen_*0.7] ) Right
  Gridbox( xtal ) Right
  dot(at g_snap(last [].e+(2*gunit,0)))

A:Gridbox( source ) Below(A)
  Gridbox( source(,I) ) Right
  Gridbox( source(,i) ) Right
  Gridbox( source(,V) ) Right
  Gridbox( source(,v) ) Right
  Gridbox( source(,AC) ) Right
  Gridbox( source(,S) ) Right
  Gridbox( source(,X) ) Right
  Gridbox( source(,F) ) Right
  Gridbox( source(,P) ) Right
  Gridbox( source(,U) ) Right
  Gridbox( source(,R) ) Right
  Gridbox( source(,T) ) Right
  Gridbox( source(,L) ) Right
  Gridbox( nullator ) Right
  Gridbox( norator ) Right
  Gridbox( consource ) Right
  Gridbox( consource(,I) ) Right
  Gridbox( consource(,i) ) Right
  Gridbox( consource(,V) ) Right
  Gridbox( consource(,v) ) Right
  Gridbox( source(,Q) ) Right
  Gridbox( source(,G) ) Right

A:Gridbox( battery(up_ elen_) ) Below(A)
  Gridbox( battery(up_ elen_,3) ) Right
  Gridbox( ttmotor ) Right
  Gridbox( memristor ) Right
  Gridbox( tline ) Right
  Gridbox( fuse ) Right
  Gridbox( fuse(,D) ) Right
  Gridbox( fuse(,B) ) Right
  Gridbox( fuse(,C) ) Right
  Gridbox( fuse(,S) ) Right
  Gridbox( fuse(,HB) ) Right
  Gridbox( fuse(,HC) ) Right
  Gridbox( cbreaker ) Right
  Gridbox( cbreaker(,,D) ) Right
  Gridbox([variable(`[]',A)]) Right
  Gridbox([variable(`[]',P)]) Right
  Gridbox([variable(`[]',L)]) Right
  Gridbox([variable(`[]',N)]) Right
  Gridbox([variable(`[]',uN)]) Right
  Gridbox([line to Here+(Rect_(dimen_/6,45))]) Right
  Gridbox([line up dimen_*0.06 then right dimen_*0.12 \
    then up dimen_*0.06]) Right
  Gridbox([em_arrows]) Right
  Gridbox([em_arrows(N)]) Right
  Gridbox([em_arrows(ND)]) Right
  Gridbox([em_arrows(I)]) Right
  Gridbox([em_arrows(ID)]) Right
  Gridbox([em_arrows(E)]) Right
  Gridbox([em_arrows(ED)]) Right

A:Gridbox( [right_; ground] ) Below(A)
  Gridbox( [right_; ground(,T)] ) Right
  Gridbox( [right_; ground(,,F)] ) Right
  Gridbox( [right_; ground(,,E)] ) Right
  Gridbox( [right_; ground(,,S)] ) Right
  Gridbox( [right_; ground(,,L)] ) Right
  Gridbox( [right_; ground(,,P)] ) Right
  Gridbox( transformer(right_ 6*gunit,L) ) Right
  Gridbox( transformer(right_ 8*gunit,,2,8) ) Right
  Gridbox( transformer(,,8,W,4) ) Right
  Gridbox( transformer(,,9,AL) ) Right
  Gridbox( transformer(,R,8,AW) ) Right

  rpoint_(right_)
A:Gridbox( switch ) Below(A)
  Gridbox( switch(,,O) ) Right
  Gridbox( switch(,,D) ) Right
  Gridbox( switch(,,OD) ) Right
  Gridbox( switch(,,C) ) Right
  Gridbox( switch(,,B) ) Right
  Gridbox( switch(,C,B) ) Right
  Gridbox( dswitch ) Right
  Gridbox( dswitch(,,WBK) ) Right
  Gridbox( dswitch(,,WBuD) ) Right
  Gridbox( dswitch(,,WBF) ) Right
  Gridbox( dswitch(,,WBKF) ) Right
  Gridbox( dswitch(,,WBL) ) Right
  Gridbox( dswitch(,,WBKL) ) Right

A:Gridbox( dswitch(,,WBT) ) Below(A)
  Gridbox( dswitch(,,WdBKC) ) Right
  Gridbox( dswitch(,,WBM) ) Right
  Gridbox( dswitch(,,WBCO) ) Right
  Gridbox( dswitch(,,WBCMP) ) Right
  Gridbox( dswitch(,,WBCY) ) Right
  Gridbox( dswitch(,,WBCZ) ) Right
  Gridbox( dswitch(,,WBCE) ) Right
  Gridbox( dswitch(,,WBRH) ) Right
  Gridbox( dswitch(,,WBRdH) ) Right
  Gridbox( dswitch(,,WBRHH) ) Right

A:Gridbox( dswitch(,,WBMMR) ) Below(A)
  Gridbox( dswitch(,,WBMM) ) Right
  Gridbox( dswitch(,,WBMR) ) Right
  Gridbox( dswitch(,,WBEL) ) Right
  Gridbox( dswitch(,,WBLE) ) Right
  Gridbox( dswitch(,,WBKEL) ) Right
  Gridbox( antenna ) Right
  Gridbox( antenna(,,L) ) Right
  Gridbox( antenna(,,T) ) Right
  Gridbox( antenna(,,S) ) Right
  Gridbox( antenna(,,D) ) Right
  Gridbox( antenna(,,P) ) Right
  Gridbox( antenna(,,F) ) Right

A:Gridbox( amp(right_ elen_*10/8) ) Below(A)
D1:Gridbox( integrator ) Right
  Gridbox( delay ) Right
  Gridbox( opamp ) Right
  Gridbox( opamp(,,,,T) ) Right
  Gridbox( amp(,dimen_/2) ) Right
  Gridbox( integrator(,dimen_/2) ) Right
  Gridbox( delay(,delay_rad_) ) Right
  Gridbox( opamp(,,,dimen_*2/3) ) Right
  Gridbox( opamp(,,,dimen_*2/3,T) ) Right

right_
A:Gridbox( relay(2) ) Below(A)
  Gridbox( relay(2,O) ) Right
  Gridbox( relay(2,C) ) Right
  Gridbox( relay ) Right
  Gridbox( contact ) Right
  Gridbox( contact(O) ) Right
  Gridbox( contact(C) ) Right

up_
A:Gridbox( [circle rad 29/10*gunit] ) Below(A)
  Gridbox( bi_tr ) Right
  Gridbox( bi_tr(,R) ) Right
  Gridbox( bi_tr(,,P) ) Right
  Gridbox( bi_tr(,R,P) ) Right

  Gridbox( j_fet ) Right
  Gridbox( j_fet(,R) ) Right
  Gridbox( j_fet(,,P,) ) Right
  Gridbox( j_fet(,R,P,) ) Right
  Gridbox( e_fet(,,,) ) Right
  Gridbox( e_fet(,R,,) ) Right

  Gridbox( e_fet(,,P,) ) Right
  Gridbox( e_fet(,R,P,) ) Right
  Gridbox( d_fet(,,,) ) Right
  Gridbox( d_fet(,R,,) ) Right
  Gridbox( d_fet(,,P,) ) Right
  Gridbox( d_fet(,R,P,) ) Right

A:Gridbox( igbt ) Below(A)
  Gridbox( ujt ) Right
  Gridbox( ujt(,,P) ) Right
rpoint_(down_)
  Gridbox( thyristor ) Right
  Gridbox( thyristor(,B) ) Right
  Gridbox( thyristor(,BG) ) Right
  Gridbox( thyristor(,A) ) Right
  Gridbox( thyristor(,C) ) Right
rpoint_(up_)
  Gridbox( j_fet ) Right
  Gridbox( e_fet ) Right
  Gridbox( e_fet(,,,S) ) Right
  Gridbox( e_fet(,,P) ) Right
  Gridbox( e_fet(,,P,S) ) Right
  Gridbox( d_fet ) Right
  Gridbox( d_fet(,,,S) ) Right
  Gridbox( d_fet(,,P,) ) Right
  Gridbox( d_fet(,,P,S) ) Right
  Gridbox( j_fet ) Right
  Gridbox( j_fet(,,P) ) Right
  Gridbox( mosfet(,,uHSDF) ) Right

rpoint_(right_)
A:Gridbox( tgate ) Below(A)
  Gridbox( tgate(,B) ) Right
  Gridbox( ptrans ) Right

A:Gridbox( AND_gate ) Below(A)
  Gridbox( OR_gate ) Right
  Gridbox( NAND_gate ) Right
  Gridbox( NOR_gate ) Right
  Gridbox( XOR_gate ) Right
  Gridbox( NXOR_gate ) Right
  Gridbox( BUFFER_gate ) Right
  Gridbox( NOT_gate ) Right

# input lines for OR-like gates.
define(`OR_inlgth',`(-sqrt(OR_rad^2-G_hht^2)+sqrt(OR_rad^2-`$1'^2))*L_unit')

  Gridbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(0)}
    {line right_ OR_inlgth(2) from Here+svec_(0,2)}
    {line same                from Here-svec_(0,2)}
    {line right_ OR_inlgth(1) from Here+svec_(0,1)}
    {line same                from Here-svec_(0,1)}] ) Right
  Gridbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(0)}
    {line right_ OR_inlgth(2) from Here+svec_(0,2)}
    {line same                from Here-svec_(0,2)}] ) Right
  Gridbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(2) from Here+svec_(0,2)}
    {line same                from Here-svec_(0,2)}] ) Right
  Gridbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(0)}] ) Right

right_
A:Gridbox( speaker ) Below(A)
  Gridbox( speaker(,,H) ) Right
  Gridbox( bell ) Right
  Gridbox( microphone ) Right
  Gridbox( buzzer ) Right
  Gridbox( buzzer(,,C) ) Right
  Gridbox( earphone ) Right
  Gridbox( earphone(,,C) ) Right
  Gridbox( nport ) Right
  Gridbox( nterm ) Right

A:Gridbox( FlipFlop(D) )  Below(A)
  Gridbox( FlipFlop(T) )  Right
  Gridbox( FlipFlop(RS) ) Right
  Gridbox( FlipFlop(JK) ) Right

 ] with .nw at (0,0)

"A draft palette of elements for Inkscape using a 1 mm grid" \
  ljust below at last [].sw
.PE
