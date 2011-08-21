.PS
# worm.m4
  gen_init

`define case { exec sprintf("$%g",floor($1+0.5)+1); }'
 ifpostscript(defineRGBprimaries)

`define hsvtorgb {
  h_ = $1; s_ = $2; v_ = $3
  f_ = h_/60-floor(h_/60)
  p_ = v_*(1-s_)
  q_ = v_*(1-f_*s_)
  t_ = v_*(1-(1-f_)*s_)
  case(pmod(floor(h_/60),6)+1,
    $4 = v_; $5 = t_; $6 = p_,
    $4 = q_; $5 = v_; $6 = p_,
    $4 = p_; $5 = v_; $6 = t_,
    $4 = p_; $5 = q_; $6 = v_,
    $4 = t_; $5 = p_; $6 = v_,
    $4 = v_; $5 = p_; $6 = q_)
  }'

Worm: [
  linethick_(0)
  dimen = 1.25       # scale parameter; try dimen=2 for a larger picture
  w = 0.15 * dimen   # worm radius
  nhues = 265
  S: dimen,0
  for inx = 1 to nhues do {
    theta = inx*360/nhues
    R: dimen*cosd(3*theta), dimen*sind(2*theta)
    D: R-(S.x,S.y)
    t = sqrt(max(0,w^2/(D.x^2+D.y^2)-1/4))
    Ra: 0.5<S,R> + (t*D.y,-t*D.x)
    Rb: 0.5<S,R> - (t*D.y,-t*D.x)
    hsvtorgb(theta+120,1,1,r,g,b)
    rgbfill( r,g,b,
      arc  cw from Ra to Rb rad w with .c at S
      arc ccw from Rb to Ra rad w with .c at R )
    S: R
    }
  ]

   "\large R" at 0.9<Worm.se,Worm.ne> ljust
   "\large G" at Worm.e ljust
   "\large B" at 0.1<Worm.se,Worm.ne> ljust
   "\large Y" at 0.1<Worm.sw,Worm.nw> rjust
   "\large M" at Worm.w rjust
   "\large C" at 0.9<Worm.sw,Worm.nw> rjust
   box invis ht Worm.ht wid Worm.wid+0.4 at Worm

[ thicklines_
  C: circle rad 1.5 at 0,0
  smallrad = C.rad/2
  for t=0 to 330 by 30 do {
    line from Rect_(C.rad-0.05,90-t) to Rect_(C.rad+0.05,90-t)
    sprintf("$%g^{\circ}$",t) at Rect_(C.rad+0.2,90-t)
    }
  "\large R" at Rect_(C.rad+0.45,90-0)
  "\large Y" at Rect_(C.rad+0.45,90-60)
  "\large G" at Rect_(C.rad+0.45,90-120)
  "\large C" at Rect_(C.rad+0.45,90-180)
  "\large B" at Rect_(C.rad+0.45,90-240)
  "\large M" at Rect_(C.rad+0.45,90-300)
  linethick_(0)

  for t = 1 to 90 do {
    hsvtorgb(t/90*360,1,1,r,g,b)
    setrgb(r,g,b,tmpcolor)
    line shaded "tmpcolor" outlined "tmpcolor" \
      from C to Rect_(C.rad-0.05,90-(t-0.5)/90*360) \
      then to Rect_(C.rad-0.05,90-(t+0.5)/90*360) \
      then to C
    resetrgb }

  R: circle diam C.rad at Rect_(C.rad*3/8,90) color "red"
  G: circle diam C.rad at Rect_(C.rad*3/8,-30) color "green"
  B: circle diam C.rad at Rect_(C.rad*3/8,-150) color "blue"

  RG: cintersect(R,R.rad,G,G.rad)
  GR: cintersect(R,R.rad,G,G.rad,R)
  GB: cintersect(G,G.rad,B,B.rad)
  BG: cintersect(G,G.rad,B,B.rad,R)
  BR: cintersect(B,B.rad,R,R.rad)
  RB: cintersect(B,B.rad,R,R.rad,R)

# The following could be done other ways depending on the postprocessor
`define lozenge {
  arc from $1$2 to $2$1 with .c at $2
  arc from $2$1 to $1$2 with .c at $1 }'

# yellow
linethick = 0.1; rgbfill(1,1,0,lozenge(R,G))
linethick = 0.4; rgbdraw(1,1,0,lozenge(R,G))

# cyan
linethick = 0.1; rgbfill(0,1,1,lozenge(G,B))
linethick = 0.4; rgbdraw(0,1,1,lozenge(G,B))

# magenta
linethick = 0.1; rgbfill(1,0,1,lozenge(B,R))
linethick = 0.4; rgbdraw(1,0,1,lozenge(B,R))

`define innersegment {
    arc cw from RB to GR with .c at R
    arc cw from GR to BG with .c at G
    arc cw from BG to RB with .c at B }'
linethick = 0.1; rgbfill(1,1,1,innersegment)
linethick = 0.4; rgbdraw(1,1,1,innersegment)

] with .w at Worm.e+(0.5,0) 

.PE
