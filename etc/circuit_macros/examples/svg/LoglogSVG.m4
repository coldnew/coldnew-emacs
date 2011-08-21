.PS
# Loglog.m4'
gen_init
s_init(Loglog)
sinclude(tst.dim)

define(`rint',`sign(`$1')*int(abs(`$1')+.5)')
S:[
  horiz = 4
  vert = horiz*2/3
  tmax = 10
  tmin = 1
  vmax = 100
  vmin = 0.1
  tic = 0.08
  denv = log(vmax) - log(vmin)
  denh = log(tmax) - log(tmin)

  Origin: Here
  define(`coord',
    `Origin+((log(`$1')-log(tmin))/denh*horiz,(log(`$2')-log(vmin))/denv*vert)')

thinlines_
# Left axes and labels
  psset_(linecolor=gray)
  for i = rint(log(vmin)) to rint(log(vmax)) do {
    move to coord(tmin,exp(i))
    { sprintf("%g",exp(i)) wid 0.2 rjust }
    line right horiz
    if i < rint(log(vmax)) then {
      for j = 2 to 9 do { move to coord(tmin,j*exp(i))
        { line right horiz }
        if (j==2) || (j==5) then { sprintf("%g",j*exp(i)) rjust }
        } } }

#Horizontal axis and labels
  for t = rint(log(tmin)) to rint(log(tmax)) do {
    move to coord(exp(t),vmin)
    { sprintf("%g",exp(t)) wid 0.15 below }
    line up vert
    if t < rint(log(tmax)) then {
      for j = 2 to 9 do { move to coord(j*exp(t),vmin)
        { line up vert }
        if (j==2) || (j==5) then { sprintf("%g",j*exp(t)) below }
        } } }
  psset_(linecolor=black)

# arrow from Origin up vert chop 0 chop -.1
  line from Origin up vert
  "svg_it(y(t))" wid 0.3 at Origin+(-.1,.5*vert) rjust

# arrow from Origin right horiz chop 0 chop -.1
  line from Origin right horiz
  "svg_it(t)" at Origin+(.5*horiz,-.1) below

thicklines_
  alpha = 2.5
  beta = 3.0
  tm = max(tmin,(vmin/alpha)^(1/beta))
  tM = min(tmax,(vmax/alpha)^(1/beta))
  line from coord(tm,alpha*tm^beta) to coord(tM,alpha*tM^beta)
P: box invis fill_(1) wid 0.75 ht 0.18 \
   "svg_it(y(t) = 1.5 t)" with .w at coord(2.005,14)
   "svg_it(svg_small(3))" with .w at P.ne ljust

  alpha = 0.05
  beta = 2.0
  tm = max(tmin,(vmin/alpha)^(1/beta))
  tM = min(tmax,(vmax/alpha)^(1/beta))
  line from coord(tm,alpha*tm^beta) to coord(tM,alpha*tM^beta)
Q: box invis fill_(1) wid 0.80 ht 0.18 \
   "svg_it(y(t) = 0.05 t)" with .e at coord(4.6,1.4)
   "svg_it(svg_small(2))" with .w at Q.ne ljust

]

# box wid S.wid ht S.ht at S
# print (S.wid,S.ht)/(1 pc__)

.PE
