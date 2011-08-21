.PS
# `Drive.m4'
cct_init

define(`synchmachine',`[ r = ifelse(`$1',,`dimen_*3/4',(`$1')/2)
  C: circle rad r
    Point_(120)
    line from C to C+vec_(r*2/9,0); reversed(`inductor',to rvec_(r*2/3,0))
  W2: Here
    Point_(240)
    line from C to C+vec_(r*2/9,0); reversed(`inductor',to rvec_(r*2/3,0))
  W3: Here
    line from C right_ r*2/9; inductor(right_ r*2/3)
  W1: Here ]')

  bht = sourcerad_*10
  dlen = dimen_*2/3
define(`Diodepair',`[ diode(up_ dlen); line up bht-2*dlen; diode(up_ dlen) ]')

define(`gbt',`bi_trans(`$1',`$2',CBUHdE`$3')')
define(`dotrad_',dotrad_*0.5/linewid)
define(`GBTpair',`[linewid = 0.5*3/4
  Q1: gbt(up_ bht/2,,D)
  Q2: gbt(up_ bht/2,,D) with .C at Q1.E
  ]')

SW: ground
  line up
W: Here
  line right dimen_/3; dot
  ssep = sourcerad_*2.5
  { line up   ssep; S1: source(right_ dimen_,AC) }
  { S2: source(right_ dimen_,AC) }
  { line down ssep; S3: source(right_ dimen_,AC) }

  dsep = dimen_/2
D1: Diodepair at S2.e
D2: Diodepair at D1+(dsep,0); dot(at D2.n); dot(at D2.s)
D3: Diodepair at D2+(dsep,0); dot(at D3.n); dot(at D3.s)
  dot(at (D1,S1))
  dot(at (D2,S2)); line to S2.e
  dot(at (D3,S3)); line to S3.e

  line from D1.n to D3.n
  inductor(right_ dimen_)
T: dot
  capacitor(to (Here,D1.s),C)
  dot

  gsep = dimen_*1.25
G1: GBTpair with .Q1.C at T+(dimen_,0)
G2: GBTpair at G1+(gsep,0)
G3: GBTpair at G2+(gsep,0)

  line from G3.Q1.C to T
  dot(at (G1.Q1.C,T))
  dot(at (G2.Q1.C,T))
  line from G3.Q2.E to D1.s
  dot(at (G1.Q1.E,D1.s))
  dot(at (G2.Q1.E,D1.s))

M: synchmachine with .C at G3.e+(dimen_*2,0)
  sg = dimen_/8
L: (0.5<G3.e,M.w>,D1.s)-(sg,0)
  line from M.W1 to (M.W1,L) then to L \
    then up M.C.y-L.y-sg then to G3.Q2.C+(0,-sg); dot
  line from M.W3 down M.W3.y-L.y-sg*2 then to L+(sg*2,sg*2)\
    then up M.C.y-L.y-sg*2 then to G2.Q2.C; dot
  line from M.W2 to (L,M.W2) \
    then down M.W2.y-G1.Q2.C.y-sg then to G1.Q2.C+(0,sg); dot

  line right dimen_/3 from M.e then down M.e.y-SW.y
  ground
.PE
