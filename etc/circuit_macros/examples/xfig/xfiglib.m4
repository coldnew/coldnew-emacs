.PS
# This is the source for a prototype xfig library of electric circuit
# elements.  Each element is a compound xfig object with corners on grid points.
# Connection points (to which lines may be drawn) should also be on grid
# points.
#
# Processing postscript.m4 and this file thought dpic -r gives postscript
# output that can be read into CorelDraw and, probably, Adobe Illustrator.
#

ifdef(`cct_init',,`include(HOMELIB_`'libcct.m4)')
cct_init
ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')


gunit = 1/16                 # default xfig grid dimension = (1/16) in
divert(-1)
define(`elen_',`8*gunit')    # default 2-terminal element length
define(`dimen_',`8*gunit')   # reference dimension for element bodies
define(`L_unit',`gunit')     # logical gate grid size
define(`N_diam',1)           # L_unit size of `not' circles

                             # grid round and chop
define(`g_round',`(int((`$1')/gunit+100000.5   )-100000)*gunit')
define(`g_chop', `(int((`$1')/gunit+100000.9999)-100000)*gunit')
                             # snap to grid
define(`g_snap',`(g_round((`$1').x),g_round((`$1').y))')

# Box containing an element.  The element must be in a box that is
# intersected by a horizontal or vertical line from entry to exit.
define(`Xfigbox',`[ S: Here; `$1' ; F: Here
   if S.y == F.y then { move from (max(S.x,F.x),F.y+g_chop(last[].n.y-F.y)) \
                               to (min(S.x,F.x),F.y-g_chop(F.y-last[].s.y)) } \
   else { move from (F.x+g_chop(last[].e.x-F.x),max(S.y,F.y))\
                 to (F.x-g_chop(F.x-last[].w.x),min(S.y,F.y))
   } ] ')

# Place compound object corners at grid intersections
define(`Below',
  `with .nw at g_snap(ifelse(`$1',,`last []',`$1').sw)-(0,2*gunit)')
define(`Right',
  `with .nw at g_snap(ifelse(`$1',,`last []',`$1').ne)+(2*gunit,0)')

# for debug
define(`prpos',`print (`$1').x/gunit,(`$1').y/gunit')
divert(0)dnl

[ [

rpoint_(up_ elen_)
A:Xfigbox( resistor )
  Xfigbox( inductor(,W) ) Right
  Xfigbox( inductor(,W,,M) ) Right
  Xfigbox( inductor ) Right
  Xfigbox( inductor(,,,M) ) Right
  Xfigbox( capacitor(down_ elen_,C) ) Right
  Xfigbox( capacitor ) Right
  Xfigbox( diode(down_ elen_) ) Right
  Xfigbox( diode(down_ elen_,Z) ) Right
  Xfigbox( diode(down_ elen_,B) ) Right
  Xfigbox( xtal ) Right
  Xfigbox( down_ ; { [right_; ground] }; move down_ 3*gunit ) Right
  Xfigbox( {move down_ gunit}; [right_; ground(,T)] ) Below
  Xfigbox( move up_ 2*gunit; dot(at last move.c) ) Below

B:Xfigbox( source ) Below(A)
  Xfigbox( source(,I) ) Right
  Xfigbox( source(,AC) ) Right
  Xfigbox( consource ) Right
  Xfigbox( consource(,I) ) Right
  Xfigbox( battery ) Right
  Xfigbox( battery(,3) ) Right
  Xfigbox( ebox ) Right

rpoint_(right_ elen_ from B)
C:Xfigbox( switch ) Below(B)
  Xfigbox( switch(,,O) ) Below
D:Xfigbox( switch(,,C) ) Below
  Xfigbox( amp(right_ elen_*10/8) ) with .nw at C.ne+(gunit,0)
D1:Xfigbox( delay ) Right
  Xfigbox(define(`dimen_',dimen_*6/5)dnl
     transformer(down_ 6*gunit,L)
     define(`dimen_',`8*gunit') ) Right

define(`bi_tr_adj',`with .E at Here
  line ifelse(`$1',,left,right) gunit*2/3 from last [].B')

define(`dimen_',`5/6*8*gunit')      # Adjust transistors to grid
  Xfigbox( [move right_ g_chop(29/10*gunit*2)
      circle rad 29/10*gunit at last move] ) Right
up_
  Xfigbox( {bi_tr bi_tr_adj}; move up_ gunit ) \
   with .se at (g_round(D1.w.x),g_round(D.s.y))
  Xfigbox( {bi_tr(,R) bi_tr_adj(R)}; move up_ gunit ) Right
  Xfigbox( {bi_tr(,,P) bi_tr_adj}; move up_ gunit ) Right
  Xfigbox( {bi_tr(,R,P) bi_tr_adj(R)}; move up_ gunit ) Right

define(`dimen_',15/16*8*gunit)
define(`fet_adj',`with .S at Here
  line ifelse(`$1',,left,right) gunit*3/4 from last [].G')
define(`fet_move',`move up_ 5*gunit from last [].S-(0,gunit)')

E:Xfigbox( {j_fet fet_adj}; fet_move ) Below(D)
  Xfigbox( {j_fet(,R) fet_adj(R)}; fet_move ) Right
  Xfigbox( {j_fet(,,P,) fet_adj(R)}; fet_move ) Right
  Xfigbox( {j_fet(,R,P,) fet_adj(R)}; fet_move ) Right
  Xfigbox( {e_fet(,,,) fet_adj(R)}; fet_move ) Right
  Xfigbox( {e_fet(,R,,) fet_adj(R)}; fet_move ) Right

F:Xfigbox( {e_fet(,,P,) fet_adj}; fet_move ) Below(E)
  Xfigbox( {e_fet(,R,P,) fet_adj(R)}; fet_move ) Right
  Xfigbox( {d_fet(,,,) fet_adj}; fet_move ) Right
  Xfigbox( {d_fet(,R,,) fet_adj(R)}; fet_move ) Right
  Xfigbox( {d_fet(,,P,) fet_adj}; fet_move ) Right
  Xfigbox( {d_fet(,R,P,) fet_adj(R)}; fet_move ) Right

right_
G:Xfigbox( OR_gate(1) ) Below(F)
  Xfigbox( NOR_gate ) Right
  Xfigbox( XOR_gate ) Right
  Xfigbox( NXOR_gate ) Right

# input lines for OR-like gates.
define(`OR_inlgth',`(-sqrt(OR_rad^2-G_hht^2)+sqrt(OR_rad^2-`$1'^2))*L_unit')

H:Xfigbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(0)}
    {line right_ OR_inlgth(2) from Here+svec_(0,2)}
    {line same                from Here-svec_(0,2)}
    {line right_ OR_inlgth(1) from Here+svec_(0,1)}
    {line same                from Here-svec_(0,1)}] ) Below(G)
  Xfigbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(0)}
    {line right_ OR_inlgth(2) from Here+svec_(0,2)}
    {line same                from Here-svec_(0,2)}] ) Right
  Xfigbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(2) from Here+svec_(0,2)}
    {line same                from Here-svec_(0,2)}] ) Right
  Xfigbox( [ {move right_ gunit}
    {move from last move-svec_(0,G_hht) to last move+svec_(0,G_hht)}
    {line right_ OR_inlgth(0)}] ) Right

  Xfigbox( AND_gate ) Right
  Xfigbox( NAND_gate ) Right
  Xfigbox( line right_ L_unit/2; BUFFER_gate ) Right
  Xfigbox( line right_ L_unit/2; NOT_gate ) Right

# Deleted because of labels:
#right_
#textht = 10/72
#I:Xfigbox( FlipFlop(D) )  Below(H)
#  Xfigbox( FlipFlop(T) )  Right
#J:Xfigbox( FlipFlop(RS) ) Below(I)
#  Xfigbox( FlipFlop(JK) ) Right

  ]
  box invis wid last [].wid+2*gunit ht last [].ht+2*gunit at last []
] with .sw at (1,1)
#print last [].wid,last[].ht
.PE
