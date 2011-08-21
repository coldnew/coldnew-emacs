% `woodchips.m4'
divert(-1)

define(`grid',`[
  psset_(linecolor=lightgray)
  B: box invis wid 1.0 ht 0.3
  d = 0.08
  for x=B.ht to B.wid by d do {
    line from B.nw+(x,0) thick 0.4 down B.ht left B.ht }
  psset_(linecolor=black)
 ]')

divert(0)dnl
.PS
sinclude(CMman.dim)
s_init(woodchips)

  G: grid
  f_box(Wood chips) with .w at G

  f_box(color "lightgray" thickness 2 rad 2pt__,"\huge$n^{%g}$",4-1) \
   with .w at G.e+(1.2,0)

.PE
