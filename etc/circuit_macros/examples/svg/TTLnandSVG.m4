.PS
# TTLnand.m4
  cct_init
  up_
Q3: bi_tr
  "Q3" at Q3.E above ljust
  dot(at Q3.C); reversed(`diode',up_ dimen_/2)
Q4: bi_tr with .E at Here
  "Q4" at Q4.E above ljust
  resistor(up_ dimen_ from Q4.C); rlabel(,`svg_norm(130)')
V: dot
  line left_ dimen_*0.75 from Q4.B; dot
  line left_ dimen_*0.75 from Q3.B; dot
Q2: bi_tr(up_ Q4.B.y-Q3.B.y) with .E at Here
  "Q2" at 1/2 between Q2.E and Q2.C ljust
  resistor(from Q2.C to (Q2.C,V)); rlabel(,`svg_norm(1.6 k)'); dot
  resistor(from Q2.E down_ dimen_); llabel(,`svg_norm(1 k)')
  line to (Q4.E,Here)
G: dot
  line to Q3.E
  line left_ dimen_/4 from Q2.B
  right_
Q1: bi_trans(,,BCdE2BU) with .C at Here
  "Q1" at Q1.Bulk above rjust
  resistor(up_ from Q1.B to (Q1.B,V)); rlabel(,svg_norm(4 k))
  line to V

  line left dimen_ from Q1.E2
X1: dot; "X1" wid 0.2 rjust above at last [].w
X2: dot(at (X1,Q3.C)); "X2" rjust at last [].w
   AA: line invis right dimen_ from X2
  line from X2 to Intersect_(AA,Q1.Em1) then to Q1.E1
X3: dot(at 2 between X1 and X2); "X3" rjust below at last [].w
   AA: line invis right dimen_ from X3
  line from X3 to Intersect_(AA,Q1.Em0) then to Q1.E0

W: X2+(dimen_/2,0)
N: V+(0,dimen_/4)
S: (N.x,N.y-2*(N.y-W.y))
  line from V to 2 between V and N
Vcc:dot; "+5 svg_norm(V)" ljust at last [].e
  line from G to (G,S)
  ground
  line from N to (W,N) then to (W,S) then to S
  arc rad N.y-W.y ccw to N with .c at 1/2 between N and S

  line right (N.y-S.y)/2+dimen_/2 from Q3.C
Z: dot
  "svg_it(Z = X1 . X2 . X3)" wid 1.0 ljust
  line left 0.75 from last "".ne

.PE
