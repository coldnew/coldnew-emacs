% PPA.m4
% Push-pull amplifier
.PS
cct_init
command "{\sf"

Vs: source(up_ elen_,"$V_S$")
  line right elen_/2
T1: transformer(down_ elen_,,,W,10) with .P1 at Here
  { dot(at T1.TP+(-dimen_/8,dimen_/3)) }
  { dot(at T1.TS+(dimen_/8,dimen_*2/3))}
  { dot(at T1.TS+(dimen_/8,-dimen_/8)) }
  line from T1.P2 to Vs.start

  line up_ dimen_*3/4 from T1.S1 then right_ dimen_*3/4
  up_
Q1: bi_tr(,L,,E) with .B at Here
  line down_ dimen_*3/4 from T1.S2 then right_ dimen_*3/4
  down_
Q2: bi_tr(,R,,E) with .B at Here
  line from Q1.E to Q2.E
R2: resistor(left_ dimen_ from last line); llabel(,R_2)
  dot; {dot(at R2.start) }
  line to T1.TS
  line down_ dimen_/2 right_ dimen_/2 from R2.start
  ground

  line right_ dimen_/6 from R2.start
Vcc: battery(right_ elen_,2); rlabel(,V_{cc})
  dot
  line up dimen_*2/3 then to (R2.start.x,Here.y+dimen_*2/3)
  resistor(to (R2.end,Here)); rlabel(,R_1); line to R2.end
  line right_ dimen_/2 from Vcc.end
T2: transformer(up_ elen_,,,W,10) with .TS at Here
  line from T2.S2 to (T2.S2,Q1.C) then to Q1.C
  line from T2.S1 to (T2.S1,Q2.C) then to Q2.C

  line right elen_/2 from T2.P2
  resistor(down_ elen_); llabel(,R_L)
  line to T2.P1

command "}%"
.PE
