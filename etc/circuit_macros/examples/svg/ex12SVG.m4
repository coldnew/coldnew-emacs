.PS
# ex12.m4
cct_init

[
   fetht = dimen_*0.9

G: ground
Q4: e_fet(up_ fetht) with .S at G
Q3: e_fet(up_ fetht) with .S at Q4.D

Q2: e_fet(down_ fetht,R,P) with .G at Q3.Channel+( dimen_*0.25,dimen_*1.8)
Q1: e_fet(down_ fetht,R,P) with .G at Q2.G + (-dimen_*1.25,0)

   line left_ dimen_ from Q3.G
A: dot; "svg_it(A)" wid 0.15 rjust
   line left_ dimen_ from Q4.G
B: dot; "svg_it(B)" rjust

   line from Q1.G to (Q1.G,Q3.D) then to (Q1.Channel,Q3.D) \
     then to (Q1.Channel,B);dot
   line from Q2.G to (Q3.Channel,Q2.G) then to (Q3.Channel,Q3.D) \
             then to (Q3.G,Q3.D) then to Q3.G;dot
   line from Q1.D to Q2.D
   line from Q1.S to Q2.S
   dot(at (G,Q1.S)) ; line up dimen_/3 ; "+5 V" rjust
   line from Q3.D to (Q3.D,Q2.D);dot
   dot(at Q3.D)
   line right_ dimen_ ; dot ; "svg_it(~(AB))" above
]

[
  del = dimen_/3

define(`pair',`[
Q1: c_fet(up_ dimen_,,P)
  line right dimen_*2/3 from Q1.D
 {dot(at last line.c); reversed(`source',up_ dimen_,I); Rail: Here
  line right dimen_/2 with .c at Here}
Q2: c_fet(up_ dimen_,R,P) with .D at Here
  line down del*2 from Q2.S
  resistor(down_ dimen_)
Gnd: ground(,T,S)
  ]')

P1: pair
Rail: P1.Rail 
Vc: dot(at P1.Q2.S+(0,-del)); "svg_it(V`'svg_sub(c))" rjust
  line from P1.Q1.G to (P1.Q1.G,Rail)+(0,del/2); "svg_it(S+)" ljust

  line right_ del from P1.Q2.G
 {dot(at last line.c); line up del; "svg_it(V`'svg_sub(ref))" above}
  pht = P1.Rail.y-P1.Gnd.y

P2: pair with .Q1.G at Here
Vcp: dot(at P2.Q2.S+(0,-del*2)); {"svg_it(V`'svg_sub(c'))" above rjust}
 {line to (P1.Q1.S,Here) then to P1.Q1.S}
  line from P2.Q2.G to (P2.Q2.G,Rail)+(0,del/2); "svg_it(S-)" rjust

  line from Vc to (P2.Q1.S,Vc) then to P2.Q1.S

define(`cpair',`[
Rail: line right dimen_/2
Q1: c_fet(up_ dimen_,,P) with .D at last line.c
Q2: c_fet(up_ dimen_) with .S at Rail+(0,-pht)
  ground(at Q2.S,T,S)
  line from Q1.G to Q2.G
  line from Q1.S to Q2.D
  ]')

S1: cpair with .Rail at P2.Rail+(P2.Q2.G.x-P2.Rail.x+P2.Q2.G.x-P2.Q2.D.x+del,0)
S2: cpair with .Q1.G at (S1.Q1.D.x+del,S1.Q1.G.y)
S3: cpair with .Rail at 2 between S1.Rail and S2.Rail

  line from Vcp to (S2.Q2.G,Vcp); dot
  dot(at (S2.Q2.D,Here)); line to (S3.Q2.G,Here); dot
  dot(at (S3.Q2.D,Here)); arrow right del
  "svg_it(e`'svg_sub(2))" ljust
  dot(at P2.Q1.S); line to (S1.Q1.G,Here); dot
  dot(at (S1.Q2.D,Here)); arrow to (last arrow.end,Here)
  "svg_it(e`'svg_sub(1))" ljust
  

] with .sw at last [].se+(0.25,0)

[
  elen=0.2

VDD: dot; llabel(,V`'svg_sub(DD),)
  line down_ elen/2
P1: mosfet(up_,,ZSDFdTX,) with .D at Here

MIDDLE: line from P1.S down_ elen/2

N1: mosfet(up_,,ZSDFTX,) with .D at Here
N2: mosfet(up_,,ZSDFTX,) with .D at N1.S
  ground(at N2.S)

ING: P1.G+(-elen/2,0)

  line from P1.B right_ elen * 2/5 then down_ elen * 4/5
  line to (ING, Here)
  dot

  line from N1.B right_ elen * 2/5 then up_ elen * 4/5
  line to (ING, Here)
  dot

  line from N1.G to (ING,N1.G)
  dot

  line from N2.G to (ING,N2.G) then to ING then to P1.G

  dot(at (ING,MIDDLE))
  line left_ elen*2
  "svg_it(V`'svg_sub(in))" above

  dot(at MIDDLE)
  line right_ elen*2
  { "svg_it(V`'svg_sub(out))" wid 0.25 above }

  line from N2.B to (Here,N2.B)
  "svg_it(V`'svg_sub(BB))" above

PUNT:dot(at 0.5 between N1.S and N2.D)

  "svg_it(V`'svg_sub(x))" ljust

  "svg_it(P`'svg_sub(1))" at P1.Channel.end above rjust
  "svg_it(N`'svg_sub(1))" at N1.Channel.start below rjust
  "svg_it(N`'svg_sub(2))" at N2.Channel.start below rjust

] with .sw at last [].se

.PE
