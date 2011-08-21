% `Audio.m4'
.PS
cct_init

define(`In123',`
thinlines_
     arrow <- from `$1'.In1 up 0.05 left 0.15
     "{\sl In1}sp_" rjust above
     arrow <- left 0.15 from `$1'.In2
     "{\sl In2}sp_" rjust
     arrow <- from `$1'.In3 down 0.05 left 0.15
     "{\sl In3}sp_" rjust below
thicklines_ ')

L:[
  S: speaker
  `"\tt speaker"' at S.s+(0,-0.2) below
  In123(S)
  thinlines_
     arrow <- from S.In4 left 0.05 up 0.15
     "{\sl In4}" above rjust
     arrow <- from S.In5 right 0.05 up 0.15
     "{\sl In5}" above
     arrow <- from S.In6 left 0.05 down 0.15
     "{\sl In6}" below rjust
     arrow <- from S.In7 right 0.05 down 0.15
     "{\sl In7}" below
     spline <- from S.Box.e+(0,0.05) right 0.27 up 0.10 \
      then right 0.1 up 0.2
     "\sl Box" above
  thicklines_
  ]
[
  H: speaker(,,H)
  `"\tt $\ldots$(,,H)"' at H.s+(0,-0.30) below
  ] with .sw at last [].se
[
  B: bell
  `"\tt bell"' at B.s+(0,-0.2) below
  In123(B)
  thinlines_
  arrow <- from B.Box.n+(-0.1,0) up 0.15 left 0.1 ; "\sl Box" above
  arrow <- from B.Circle.n up 0.15 ; "\sl Circle" above
  thicklines_
  ] with .sw at last [].se+(0.35,0)
[
  M: microphone
  `"\tt microphone"' at M.s+(0,-0.2) below
  In123(M)
  thinlines_
  arrow <- from M.Circle.n up 0.15 ; "\sl Circle" above
  thicklines_
  ] with .sw at last [].se+(0.4,0)
[
  Z: buzzer
  `"\tt buzzer"' at Z.s+(0,-0.2) below
  In123(Z)
  thinlines_
  arrow <- from Z.Box.n up 0.15 ; "\sl Box" above
  thicklines_
  ] with .sw at last [].se+(0.4,0)
R:[
  Z: buzzer(,,C)
  `"\tt buzzer(,,C)"' at Z.s+(0,-0.2) below
  In123(Z)
  thinlines_
  spline <- from 0.8<Z.Face.s,Z.Face.n> right 2*arrowht up 0.5*arrowht \
    then up 0.15 right 0.05 
    "\sl Face" above
  thicklines_
  ] with .sw at last [].se+(0.4,0)
[[
  E: earphone
  `"\tt earphone"' at E.s+(0,-0.2) below
  In123(E)
  thinlines_
  arrow <- from E.Box.n up 0.15 left 0.15 ; "\sl Box" above
  thicklines_
  ]
 [
  E: earphone(,,C)
  `"\tt earphone(,,C)"' at E.s+(0,-0.2) below
  thinlines_
  arrow <- from E.L up 0.15 left 0.15; "\sl L" rjust
  arrow <- from E.R up 0.15 right 0.15; "\sl R" ljust
  "\sl N" at E.N above
  "\sl C" at E.C
  thicklines_
  ] with .sw at last [].se+(0.4,0)
] with .n at (0.5 between L and R,L.s)+(0,-0.2)

.PE
