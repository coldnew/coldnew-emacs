% Three.m4: three-phase oscillator
.PS
  cct_init

define(`section',`[ I: Here
  resistor(right elen_*0.75); llabel(,R/2)
T: dot
  line down dimen_/2 then right_ dimen_/4
  Amp: opamp with .In1 at Here 
  line left dimen_/4 from Amp.In2 then down dimen_/4; G: ground(,,)
  resistor(from T to (Amp.Out,T)); llabel(,R)
O: dot
  line to Amp.Out
  dot(at T+(dimen_/4,0)); line up dimen_*0.6
  capacitor(right_ Amp.Out.x-Here.x-dimen_/4,C); llabel(,2C)
  line to (Here,T); dot
  ]')

A: section
B: section with .I at A.O
C: section with .I at B.O

  ground(at (A.I,A.G)+(dimen_/3,0),,)
P:potentiometer(down_ dimen_/2,,0.75,dimen_/2) with .End at Here
  line from P.Start to (A.I,P.Start); dot
  resistor(up_ dimen_*0.75 from P.T1)
  line to A.T

  dot(at B.Amp.Out+(dimen_/3,0)); { "$V_c$" above }
  resistor(down_ dimen_*0.75)
  { dot; line right (C.T.x-Here.x)/2
    reversed(`diode',up_ dimen_)
    line to C.T }
  resistor(down_ dimen_*0.75)
  dot
  line from C.O right dimen_/4 then down C.O.y-Here.y then to (A.I,Here) \
    then to A.I

  arrow from A.O up dimen_*0.5 then right dimen_/4 up dimen_/4
  "$`0^\circ'$" above
  arrow from B.O up dimen_*0.5 then right dimen_/4 up dimen_/4
  "$`120^\circ'$" above
  arrow from C.O up dimen_*0.5 then right dimen_/4 up dimen_/4
  "$`-120^\circ'$" above

.PE
