% MosfetElements.m4
.PS
cct_init
s_init(MosfetElements)
  linewid = linewid*1.5
  linethick = 1.5

define(`mfgrid',
 `tr_xy_init(,m4_U,m4R)define(`m4R',)
  ltt = linethick; linethick = 0.4
  m4gdim = ifelse(`$1',,4,`($1)')
  command "\color{lightgray}"
  for x=-m4gdim to m4gdim do {
    line color "lightgray" from tr_xy(x,0) to tr_xy(x,2*m4gdim)
    if pmod(x,2)==0 then { sprintf("{\tiny %g}",x) at last line.start below }}
  for y=0 to 2*m4gdim do {
    line color "lightgray" from tr_xy(-m4gdim,y) to tr_xy(m4gdim,y)
    if pmod(y,2)==0 then { sprintf("{\tiny %g}",y) at last line.start rjust }}
  linethick = ltt
  command "\color{black}"
  Grid: box invis wid 2*m4gdim*m4_xyU ht 2*m4gdim*m4_xyU \
    with .sw at tr_xy(-m4gdim,0)
')
define(`gridnext',`with .Grid.w at last [].Grid.e + (m4_xyU*2,0)')
define(`gridbelow',`with .Grid.nw at `$1'.Grid.sw+(0,-linewid/2)')
define(`Gmosfet',`mfgrid; Q: mosfet(,,`$1') with .S at Grid.sw + (2*m4_xyU,0)')

R1: [ Gmosfet(B); "B" at Q.Bl ljust ]
 [ Gmosfet(D); "D" at Q.Dl ljust ] gridnext
 [ Gmosfet(E); "E" at Q.Channel below ] gridnext
 [ Gmosfet(F); "F" at Q.Channel below ] gridnext
 [ Gmosfet(G); "G" at Q.Gl ljust ] gridnext
 [ Gmosfet(H); "H" at Q.Hl ljust ] gridnext
 [ Gmosfet(L); "L" at Q.Ll ] gridnext
 [ Gmosfet(M); "M" at Q.Glh below ] gridnext

R2:[ Gmosfet(uM); "uM" at Q.Gl ] gridbelow(R1)
 [ Gmosfet(dM); "dM" at Q.Gl ] gridnext
 [ Gmosfet(Pz); "Pz" at Grid.s ] gridnext
 [ Gmosfet(Q); "Q" at Q.Ql above ] gridnext
 [ Gmosfet(R); "R" at Q.Rl below ] gridnext
 [ Gmosfet(S); "S" at Q.Sl ljust ] gridnext
 [ Gmosfet(dT); "dT" at Q.Tl below ] gridnext
 [ Gmosfet(X); "X" at Q.Xh above ] gridnext

# [       Q: mosfet
#    {"S" at Q.S rjust}
#    {"D" at Q.D ljust}
#    {"G" at Q.G rjust}
#    s_box(``mosfet'') at Q.s below] 
#
# [       Q: mosfet(,,DSELuBQ)
#    s_box(``mosfet(,,DSELuBQ)'') at Q.s below] with .n at last [].s + (0,-0.2) 
#
# [       Q: mosfet(,,DSEMuBQ)
#    s_box(``mosfet(,,DSEMuBQ)'') at Q.s below] with .n at last [].s + (0,-0.2) 
#
# [       Q: mosfet(,,DSEdMuBQ)
#    s_box(``mosfet(,,DSEdMuBQ)'') at Q.s below] with .n at last [].s + (0,-0.2) 
#
# [       Q: mosfet(right dimen_,,DSELuBQN)
#    {"S" at Q.S rjust}
#    {"D" at Q.D ljust}
#    {"G" at Q.G rjust}
#    s_box(``mosfet(right dimen\_,,DSELuBQN)'') at Q.s below] \
#      with .n at last [].s + (0,-0.2) 

.PE
