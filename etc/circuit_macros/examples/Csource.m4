% Csource.m4
.PS
  ifmpost(command "verbatimtex")
  ifpostscript(,`command \
   "\newcommand\scriptstack[2][c]{\hbox{\scriptsize\shortstack[#1]{#2}}}%"
   command "{\scriptsize"')
  ifmpost(command "etex")

cct_init
  linewid = 0.4

define(`npair',`[define(`m4lr',ifelse(`$1',,l,r))
  J: mosfet(down_ dimen_,ifelse(`$1',,R,L),uHSDF,E)
     "\scriptstack[l]{$Q_{`$2'}$\\ 2N5464}" at J.n+(0,8pt__)
  G: J.G
     line ifelse(`$1',,right,left) dimen_/4 from J.D
  Q: bi_tr(up_ dimen_,ifelse(`$1',,L,R),,E) with .B at Here
     ifelse(`$1',,r,l)label(,`\scriptstack[m4lr]{$Q_{`$3'}$\\ 2N4239}')
  E: Q.E
  S: dot(at (Q.C,J.S))
     line from J.S to S then to Q.C ]')

define(`ppair',`[define(`m4lr',ifelse(`$1',,l,r))
  J: mosfet(down_ dimen_,ifelse(`$1',,R,L),dHSDF,E)
     "\scriptstack[l]{$Q_{`$2'}$\\ 2N3819}" at J.s-(0,8pt__)
  G: J.G
     line ifelse(`$1',,right,left) dimen_/4 from J.S
  Q: bi_tr(down_ dimen_,ifelse(`$1',,R,L),P,E) with .B at Here
     ifelse(`$1',,l,r)label(,`\scriptstack[m4lr]{$Q_{`$3'}$\\ 2N4236}')
  E: Q.E
  S: dot(at (Q.C,J.D))
     line from J.D to S then to Q.C ]')

  R2: resistor(down_ dimen_)
    llabel(,`\scriptstack[l]{$R_2$\\ 150}')
    dot
  Q13: bi_tr(down_ dimen_,,P,E) with .E at Here
    rlabel(,`\scriptstack[r]{$Q_{13}$\\ 2N4236}')
    line down 3.1*elen_ from Q13.C
  Q14: bi_tr(up_ dimen_,R,,E) with .C at Here 
    llabel(,`\scriptstack[r]{$Q_{14}$\\ 2N4239}')
    dot(at Q14.E)
  { line left_ dimen_*1.3 then up_ dimen_
  R1: potentiometer(up_ dimen_) with .Start at Here
    rlabel(,`\scriptstack[l]{$R_1$\\ $20\,$K}')
    { ground(at R1.T1) }
    line from R1.End to (R1.End,Q13.E) then to Q13.E }
  { dot(at Q13.C+(0,-elen_))
    line to (R1.R,Here)+(-dimen_/2,0)
    { arrowline(right_ dimen_/2); llabel(,i_1) }
  V1: gap(down_ dimen_,1); rlabel(+,V_1,-) 
    line down_ dimen_/4 chop dotrad_ chop 0; ground(,T) }
  R3: resistor(down_ dimen_)
    llabel(,`\scriptstack[l]{$R_3$\\ 150}')
    dot(at Q13.C+(0,-2*elen_))
    line right_ dimen_/2
  A1: opamp with .In2 at Here;  "$A_1$" at A1.C
    "`\scriptstack[l]{Nexus\\ SQ-10A}'" at A1.s below
    line left_ dimen_/4 from A1.In1 then up_ dimen_/2 then right dimen_
    dot
  { resistor(up_ elen_)
    llabel(,`\scriptstack[r]{$R_{14}$\\ $19\,$K}')
    line to (Here,Q13.E) then to Q13.E }
    line to (A1.Out,Here)
    dot
  { line to A1.Out
    dot
    resistor(down_ to (Here,Q14.E))
    llabel(,`\scriptstack[l]{$R_{13}$\\ $91\,$K}')
    line to Q14.E }
  R6: resistor(right_ dimen_)
    rlabel(,`\scriptstack[l]{$R_6$\\ $15\,$K}')
    dot
    line to (Here,A1.In1) then right_ dimen_/4
  A2: opamp with .In1 at Here; "$A_2$" at A2.C
    "`\scriptstack[l]{Nexus\\ SQ-10A}'" at A2.s below
    line left_ dimen_/4 from A2.In2
    ground

    dot(at A2.Out)
  DP1: npair(,3,4) with .E at (A2.Out,Q14.B)+(elen_*1.25,0)
    line from A2.Out to (A2.Out,DP1.G) then to DP1.G
    line from DP1.E to (DP1.E,R3.bottom); dot
  D4: reversed(`diode',up_ dimen_/2 from DP1.S)
  D3: reversed(`diode',up_ dimen_/2 from Here+(0,-dimen_/6))
    llabel(,`\scriptstack[r]{$D_4$\\ OMC-V\\ $D_3$}')
  DP2: ppair(,1,2) with .E at (DP1.E,Q13.B)
  D1: diode(down_ dimen_/2 from DP2.S)
  D2: diode(down_ dimen_/2 from Here+(0,dimen_/6))
    rlabel(,`\raisebox{5pt}{\scriptstack[r]{$D_1$\\ $D_2$\\ OMC-V}}')
    dot(at (Here,A2))
    line to D2.bottom
    line from DP2.G to (A2.Out,DP2.G) then to A2.Out
    line from DP2.E to (DP2.E,R2.top); dot

  T: dot(at (DP1.E,A2)+(dimen_,0))
  DP3: npair(,7,8) with .G at (Here,DP1.G)+(dimen_/4,0)
    dot(at DP3.E)
    line to Q14.B
  DP4: ppair(,5,6) with .G at (DP3.G,DP2.G)
    dot(at DP4.E)
    line to Q13.B
    line from DP3.G to (T,DP3.G) then to (T,DP4.G) then to DP4.G
  R11: resistor(down_ dimen_ from DP4.S)
    rlabel(,`\scriptstack[r]{$R_{11}$\\ $20$}')
  D5: diode(up_ from DP3.S to (DP3.S,A2),Z)
    llabel(,`\scriptstack[r]{$D_5$\\ 1N4729$_{\vphantom{1}}$}')
    line to R11.end
  C4: capacitor(right_ dimen_*1.3 from DP3.S,C)
    llabel(`C_4'); rlabel(,`0.68\,\mu\hbox{F}')
  C3: capacitor(right_ dimen_*1.3 from DP4.S,C)
    llabel(`C_3'); rlabel(,`0.68\,\mu\hbox{F}')
    dot
    dot(at (Here,R6))
  C2: reversed(`capacitor',left_ dimen_*1.3,C)
    rlabel(,,`C_2'); llabel(,`0.1\,\mu\hbox{F}')
    line to R6.end

  R5: resistor(up_ from R6.end to (R6.end,Q13.B))
    rlabel(,`\scriptstack[l]{$R_5$\\ $100\,$K}')
    dot(at R5.top-(0,dimen_/2))
    line left_ dimen_/2
    capacitor(down_ dimen_,C)
    rlabel(,`\scriptstack[r]{$C_1$\\ $1\,\mu$F}')
    ground(,T)

    line from DP4.E to (DP4.E,R2.bottom)
    resistor(up_ to (Here,R2.top))
    rlabel(,`\scriptstack[l]{$R_9$\\ $910$}')
    dot
    resistor(down_ from DP3.E to (DP3.E,R3.bottom))
    llabel(,`\scriptstack[l]{$R_{10}$\\ $910$}')
    dot

    line from R5.top to (R5,Q13.E)
    resistor(right_ elen_)
    llabel(,`\scriptstack[l]{$R_4$\\ $100\,$K}')
    line to (C4.end,Here) then to C4.end
    
  DP5: npair(L,11,10) with .E at (C4.end,DP1.E)+(elen_*0.8,0)
    line from DP5.E to (DP5.E,R3.bottom); dot
    line from R3.bottom to Here+(dimen_/2,0)
    dot(,,1)
    `"${}-E_{CC}$"' ljust
  D6: diode(up_ from DP5.S to (DP5.S,A2),Z)
    llabel(,`\scriptstack[r]{$D_6$\\ 1N4728}')
  DP6: ppair(L,12,9) with .E at (DP5.E,DP2.E)
    line from DP6.E to (DP6.E,R2.top); dot
    line from R2.top to Here+(dimen_/2,0)
    dot(,,1)
    `"${}+E_{CC}$"' ljust
    resistor(down_ from DP6.S to (DP6.S,C2))
    rlabel(,`\scriptstack[r]{$R_{12}$\\ $20$}')
    dot
  { line to C2.e }
    line to D6.end

  Vr: dot(at (DP6.G,V1.top)+(dimen_/4,0))
    line from DP6.G to (Vr,DP6.G) then to (Vr,DP5.G) then to DP5.G
    reversed(`arrowline',right_ dimen_/2 from Vr); llabel(,i_2)
  V2: gap(down_ dimen_,1); llabel(+,V_2,-)
    line down_ dimen_/4 chop dotrad_ chop 0; ground(,T)

    dot(at (Vr,T))
    resistor(left_ to (DP6.E,Here))
    rlabel(,`\scriptstack[l]{$R_{15}$\\ $200$}')
    line to (DP1.E,Here)
    dot

  command "ifmpost(verbatimtex) ifpostscript(,}%) ifmpost(etex)"
.PE
