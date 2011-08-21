.PS
# Windings.m4
cct_init

define(`vlight',`rgb(230,230,230)')
define(`lightgray',`rgb(179,179,179)')

[ winding ]
  `"winding"' at last [].s below

[ winding(R) ] with .n at last "".s+(0,-0.1)
  `"winding(R)"' wid 1 at last [].s below

[ sk = 0.75
  d = 1.5*sk
  p = 0.4*sk
  w = 1*sk

W: winding(L,d,p,4,w,vlight)
  
thinlines_
  dimension_(from W.nw+(p/4,0) right p,0.1,"pitch" above,0.3)
  dimension_(from W.sw to W.nw,0.2,diam,0.3)
  dimension_(from W.e+(-p/2,-w/2) up w,-0.3,,0.2)
  "core wid" wid 0.75 at W.e ljust
  arrow <- up d-w+0.1 left d/10 from W.ne+(-p/2,-(d-w))
  "core color" at Here+(-0.2,0) ljust above
thicklines_
] with .sw at last [].se+(0.25,0)
  "T1" at last [].W.T1 below
  "T2" at last [].W.T2 below

[
#`winding(L|R, diam, pitch, nturns, core wid, core color )'
boxwid = 0.3
boxht = 0.4
 down_
B1: box
 winding(,boxwid*1.3,boxht/5,3,boxwid) at B1
  "Left pins" "cw" at B1.s+(0,-0.15)
  for_(1,2,1,`"svg_small(T`'m4x)" at last [].T`'m4x rjust')
B2: box at B1.e + (0.5,0)
 winding(R,boxwid*1.3,boxht/5,3,boxwid) at B2
  "Left pins" "ccw" at B2.s+(0,-0.15)
  for_(1,2,1,`"svg_small(T`'m4x)" at last [].T`'m4x rjust')
 up_
B3: box at B1.s + (0,-0.55)
 winding(,boxwid*1.3,boxht/5,3,boxwid) at B3
  "Right pins" "cw" at B3.s+(0,-0.15)
  for_(1,2,1,`"svg_small(T`'m4x)" at last [].T`'m4x ljust')
B4: box at (B2,B3)
 winding(R,boxwid*1.3,boxht/5,3,boxwid) at B4
  "Right pins" "ccw" at B4.s+(0,-0.15)
  for_(1,2,1,`"svg_small(T`'m4x)" at last [].T`'m4x ljust')
] with .sw at last [].se+(0,-0.15)

[
  Q: box invis ht 1 wid 5/4
  g = Q.wid/12
  ironwid = Q.wid/4.5

  line thick ironwid/(1bp__)+linethick from Q.n+(g/2-linethick/2 bp__,0) \
    to Q.ne then to Q.se then to Q.sw then to Q.nw \
    then to Q.n+(-g/2+linethick/2 bp__,0)
  line thick ironwid/(1bp__)-linethick outlined "lightgray" \
    from Q.n+(g/2+linethick/2 bp__,0) \
    to Q.ne then to Q.se then to Q.sw then to Q.nw \
    then to Q.n+(-g/2-linethick/2 bp__,0)

thinlines_
  dimension_(from Q.n+(-g/2,-ironwid/2) right g,-ironwid/4,
    "svg_it(g)" below,1,2pt__)
thicklines_

  down_
  P: winding(R,ironwid*5/4,ironwid/2,4,ironwid,lightgray) at Q.w
    line left ironwid*2/3 from P.T1
    arrow right to P.T1 "svg_it(i`'svg_sub(1))" above
    line left ironwid*2/3 from P.T2
    gap(up_ to 2nd last line.end)
    llabel(-,svg_it(v`'svg_sub(1)),+)
    "svg_it(N`'svg_sub(1))" at P.e ljust

  up_
  S: winding(L,ironwid*5/4,ironwid/2,4,ironwid,lightgray) at Q.e
    line right ironwid*2/3 from S.T2
    arrow left to S.T2 "svg_it(i`'svg_sub(2))" above
    line right ironwid*2/3 from S.T1
    gap(up_ to 2nd last line.end)
    rlabel(-,"svg_it(v`'svg_sub(2))" wid 0.2,+)
    "svg_it(N`'svg_sub(2))" at S.w rjust

  box dashed rad ironwid/4 wid Q.wid ht Q.ht at Q
  arrow right arrowht from 0.5 between Q.n and Q.ne
# "\phi" ljust at Here+(0,5pt__)

  move up ironwid/2 from Q.n
  move down ironwid/2 from Q.s
] with .sw at last [].se+(0.25,0)

.PE
