.PS
# `Audio.m4'
cct_init

define(`In123',`
thinlines_
     arrow <- from `$1'.In1 up 0.05 left 0.15
     "svg_it(In1)" wid 0.25 rjust above
     arrow <- left 0.15 from `$1'.In2
     "svg_it(In2)" rjust
     arrow <- from `$1'.In3 down 0.05 left 0.15
     "svg_it(In3)" rjust below
thicklines_ ')

L:[
  S: speaker
  `"speaker"' at S.s+(0,-0.2) below
  In123(S)
  thinlines_
     arrow <- from S.In4 left 0.05 up 0.15
     "svg_it(In4)" above rjust
     arrow <- from S.In5 right 0.05 up 0.15
     "svg_it(In5)" above ljust
     arrow <- from S.In6 left 0.05 down 0.15
     "svg_it(In6)" below rjust
     arrow <- from S.In7 right 0.05 down 0.15
     "svg_it(In7)" below ljust
     spline <- from S.Box.e+(0,0.05) right 0.27 up 0.10 \
      then right 0.1 up 0.2
     "svg_it(Box)" above
  thicklines_
  ]
[
  H: speaker(,,H)
  `"...(,,H)"' at H.s+(0,-0.30) below
  ] with .sw at last [].se
[
  B: bell
  `"bell"' at B.s+(0,-0.2) below
  In123(B)
  thinlines_
  arrow <- from B.Box.n+(-0.1,0) up 0.15 left 0.1 ; "svg_it(Box)" above
  arrow <- from B.Circle.n up 0.15 ; "svg_it(Circle)" above
  thicklines_
  ] with .sw at last [].se+(0.35,0)
[
  M: microphone
  `"microphone"' at M.s+(0,-0.2) below
  In123(M)
  thinlines_
  arrow <- from M.Circle.n up 0.15 ; "svg_it(Circle)" above
  thicklines_
  ] with .sw at last [].se+(0.4,0)
[
  Z: buzzer
  `"buzzer"' at Z.s+(0,-0.2) below
  In123(Z)
  thinlines_
  arrow <- from Z.Box.n up 0.15 ; "svg_it(Box)" above
  thicklines_
  ] with .sw at last [].se+(0.4,0)
R:[
  Z: buzzer(,,C)
  `"buzzer(,,C)"' at Z.s+(0,-0.2) below
  In123(Z)
  thinlines_
  spline <- from 0.8<Z.Face.s,Z.Face.n> right 2*arrowht up 0.5*arrowht \
    then up 0.15 right 0.05 
    "svg_it(Face)" wid 0.5 above
  thicklines_
  ] with .sw at last [].se+(0.4,0)
[[
  E: earphone
  `"earphone"' at E.s+(0,-0.2) below
  In123(E)
  thinlines_
  arrow <- from E.Box.n up 0.15 left 0.15 ; "svg_it(Box)" above
  thicklines_
  ]
 [
  E: earphone(,,C)
  `"earphone(,,C)"' at E.s+(0,-0.2) below
  thinlines_
  arrow <- from E.L up 0.15 left 0.15; "svg_it(L)" rjust
  arrow <- from E.R up 0.15 right 0.15; "svg_it(R)" ljust
  "svg_it(N)" at E.N above
  "svg_it(C)" at E.C
  thicklines_
  ] with .sw at last [].se+(0.4,0)
] with .n at (0.5 between L and R,L.s)+(0,-0.2)

.PE
