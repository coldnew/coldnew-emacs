% I2L.m4
.PS
  ifdef(`NOR_gate',,`include(HOMELIB_`'liblog.m4)')
  cct_init

[
  down_
T1: bi_trans(,R,BC2uEBU)
  dy = T1.C0.y-T1.C1.y
  dx = T1.C0.x-T1.Bulk.x
  up_
Q1: bi_trans(,,BC1dEBU) with .C1 at (T1.C2.x+dy+dx+dx,T1.C0.y-dy)
Q2: bi_trans(,,BC1dEBU) with .C1 at Q1.E+(0,-dy*2)
Q3: bi_tr(,,) with .B at Q2.C0+(2*dy,0)
  down_
T2: bi_trans(,R,BC1uEBU) with .C1 at (Q3.C.x+2*dx+2*dy,Q1.C0.y+dy)
  up_
X1: bi_tr(,,) with .B at (T2.C0.x+dy+dx,Q1.C0.y)
X2: bi_tr(,,) with .B at (X1.B,Q3.C)

  line down dx from Q2.E
G: dot
  line from T1.B to (T1.B,G) then to (X1.E.x+dy,G.y) then to X1.E+(dy,0) \
    then to X1.E
  line from X2.E right dy; dot

  line from Q1.E down dy then to (T1.B.x,Q1.E.y-dy); dot
  line from T1.C0 to (Q3.B,T1.C0) then to Q3.B; dot; line to Q2.C0

A: dot(at (T1.B,Q1.B)-(dx,0)); { "$A$" rjust }
  line to Q1.B
  line from T1.C2 to (T1.C2,Q1.B); dot

  line from Q2.B to (A,Q2.B); dot; { "$B$" rjust }
  line right dy from T1.C1 then down T1.C1.y-Q2.B.y; dot

VB: dot(at (T2.E.x+2*dx,T1.E.y+dy)); { "$V_B$" ljust }
  line to (T1.E,Here) then to T1.E
  line from T2.E to (T2.E,VB); dot

  line from Q1.C1 to (Q3.C,Q1.C1) then to Q3.C; dot; line to X2.B

  line from Q1.C0 to X1.B
  dot(at Q1.C0+(dy,0)); line to (Here,Q2.C1) then to Q2.C1

  line from Q3.E to (Q3.E,G); dot; ground

  line from T2.B to (T2.B,G); dot

  line from T2.C1 to (T2.C1,X1.B); dot
  "$\overline{A+B}$" below at Here+(0,-2pt__)

  line from T2.C0 to (X2.B,T2.C0) then to X2.B; dot
  "$\overline{A+\overline{B}}$" below at (T2.E,Here)+(0pt__,-2pt__)

]

[
right_

jog = AND_ht/2*L_unit

  X1: NOR_gate
    line right jog from X1.Out; "$\overline{A+B}$" ljust
  X2: NOR_gate at X1+svec_(0,-AND_ht*3/2)
    line right jog from X2.Out; "$\overline{A+\overline{B}}$" ljust
    line left jog from X2.In2
    right_
  NOT: NOT_gate with .Out at Here
    line left jog from NOT.In1 then up X1.In2.y-NOT.In1.y; dot
    line from X1.In2 to Here chop 0 chop -jog
  B: dot; "$B$" rjust
    line from X1.In1 to (B,X1.In1); dot; "$A$" rjust
    line left jog from X2.In1 then up X1.In1.y-X2.In1.y; dot

] with .e at last [].w-(1,0)

.PE
