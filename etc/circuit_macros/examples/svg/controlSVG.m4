.PS
# control.m4
#.PS 5i
cct_init
  linewid = linewid*0.8
  circlerad = 0.35/2
  bw = boxwid/2
  bh = boxht/2
PID: [
  {"svg_it(r(s))" above ljust}
  arrow
S1: circle
  line right "svg_it(e(s))" above
  { arrow right ; box ht bh wid bw "svg_it(K`'svg_sub(p))"
    arrow right linewid-circlerad ; S2: circle }
  { line up linewid ; arrow right ; box ht bh wid bw "svg_it(K`'svg_sub(D) s)"
    line to (S2,Here) ; arrow to S2.n }
  { line down linewid ; arrow right
    box ht bh wid bw "svg_it(K`'svg_sub(I) /s)"
    line to (S2,Here) ; arrow to S2.s }
  arrow right from S2.e "svg_it(u(s))" above
  box "svg_it(G(s))"
  arrow right ; "svg_it(y(s))" above rjust at Here+(0,2pt__)
  line down boxht*3/2 from last arrow.c then left last arrow.c.x-S1.x
  arrow to S1.s
  "-" below rjust
  ]
  "(a) PID control" below ljust at PID.sw+(0,-5pt__)

Obs: [ {"svg_it(r)" above ljust}
  arrow
S: circle
  "-" below rjust at S.s
  arrow <- down linewid/2 from S.s
  box ht bh wid bw "svg_it(K)"
  arrow <- down linewid/2
  line right linewid "svg_it(z)" above
Obs:box wid linewid*5.5 ht boxht*5/4 "svg_it((d/dt)z = (A - LC) z + B u + L y)" 
  "Observer" above ljust at Obs.sw
  arrow <- right linewid/2 from 3/4<Obs.se,Obs.ne>
  line to (Here,S)
  {"svg_it(u)" above at Here+(linewid/2,0)}
  arrow from S.e to Here+(linewid,0)
Plant: box wid boxwid*1.7 ht boxht*5/4 \
    "svg_it((d/dt) x = Ax + Bu)" "svg_it(y = Cx)"
  "Plant" above ljust at Plant.sw
  arrow right from Plant.e
  "svg_it(y)" above rjust at Here+(0,2pt__)
  move to last arrow.c 
  line to (Here, 1/4<Obs.se,Obs.ne>)
  arrow to (Obs.e,Here)
  ] with .nw at PID.sw+(0,-0.5)
  "(b) Output feedback with a full-order observer" below ljust \
   at Obs.sw+(0,-5pt__)

NL: [boxwid = boxwid*1.8
  boxht = boxht*3/2
  thicklines_
  scale = scale*0.8
  circlerad = 0.3/2
R: box "reference" "svg_it(R)"
   arrow right linewid*4/3 "svg_it(y`'svg_sub(c ref))" above
Gh: box "\"inverse\"" "of svg_it(G)"
   arrow from Gh.s down "svg_it(y`'svg_sub(m ref))" ljust
E: circle
   arrow from E.e right "svg_it(e)" above
   box "stabilizer" "svg_it(K)"
   arrow "svg_it(du)" above
D: circle
   line from Gh.e to (D,Gh.e) "svg_it(u`'svg_sub(ref))" above
   arrow to D.n
   arrow from D.e right "svg_it(u)" above
G: box "plant" "svg_it(G)"
   arrow <- from G.n up boxht/2 
   "disturbance svg_it(z)" above
   arrow right from G.e + (0,boxht/4)
   "svg_it(y`'svg_sub(c))" ljust
   arrow right from G.e + (0,-boxht/4)
   "svg_it(y`'svg_sub(m))" ljust
   move to last arrow.c
   arrow down Here.y-G.s.y+boxht*2/3 then left Here.x-E.x then to E.s
   "svg_it(-)" at Here + (-0.15,-0.1)
   arrow <- down from R.s
   "svg_it(W) control input" at Here - (0,0.1)

UL:R.nw +(-boxht/2,boxht/2)
UR:(D.e,UL) + (0.1,0)
   line dashed from UL to UR then to (UR,G.s+(0,-boxht*3/2))
   line dashed to (Gh.w,Here) "Controller" above
   line dashed to (Here,Gh.s+(0,-boxht/2))
   line dashed to (UL,Here) then to UL
  ] with .nw at Obs.sw + (0,-0.5)
  "(c) Nonlinear feedforward (for performance) and small-signal feedback
   (for stability)" wid Obs.wid+0.4 below ljust at NL.sw+(0,-5pt__)
.PE
