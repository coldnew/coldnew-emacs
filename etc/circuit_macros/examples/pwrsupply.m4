% Pwrsupply.m4
.PS
cct_init
ifdef(`m4pco',`resetrgb')

Singlephase:[
    linewid = linewid*1.2
    down_
  T:transformer
    line left_ elen_/8 from T.P1
    rgbdraw(0,0,1,fuse(left_ elen_/3,D))
    reversed(`switch',left_ elen_*2/3)
    gap(down_ to (Here,T.P2))
    { fuse(right_ 2*dimen_/5 at last []) }
    line to T.P2
    blen = dimen_/2
  W: T.TS+(dimen_/2,0)
  N: W+(blen,blen)
  S: W+(blen,-blen)
  E: S+(blen,blen)
    diode(from W to N)
    diode(from S to E)
  G:gap(from E+(dimen_,0) down_ (E.y-S.y)*5/4); llabel(+,,-)
  C:capacitor(down_ G.start.y-G.end.y from 0.5 between E and G.start,C)
  
  setrgb(1,0,0)
    line from T.S1 to (T.S1,N) then to N; dot
    diode(to E); dot
    line from E to G.start; dot
    dot(at C.start)
  resetrgb
  
  setrgb(0,1,0)
    dot(at C.end)
    dot(at G.end)
    ground
    line to (W,Here) then to W; dot
    diode(to S); dot
    line to (T.S2,Here) then to T.S2
  resetrgb
  ]

Threephase: [
L:[
  Load: ebox(down_ 2*elen_,0.4,0.25); llabel(+,,-)
    hsep = dimen_*3/4
    for_(1,3,1,
     `line left_ hsep; ifelse(m4x,3,,dot)
      { diode(up_ Load.ht/3) ; line up_ Load.ht/3; diode(up_ Load.ht/3)
      T`'m4x: ifelse(m4x,3,Here,dot)
      line right hsep } ')
  ]

T:[
  X1: transformer(down_ dimen_*2/3,,,,4)
  X2: transformer(down_ dimen_*2/3,,,,4) with .P1 at X1.P2
  X3: transformer(down_ dimen_*2/3,,,,4) with .P1 at X2.P2
      line from X1.M4Core1.end to X3.M4Core1.start
      line from X1.M4Core2.end to X3.M4Core2.start
  for_(1,3,1,
   `move to X`'m4x.P2 ; ifelse(m4x,3,,dot)
    line left_ dimen_
    P`'m4x: dot(,,1)')
    line left_ dimen_/2 from X1.P1 then down X1.P1.y-P3.y; dot
  B: X1.S2+(dimen_/2,0)
    line from X1.S1 right B.x-X1.S2.x then down X1.S1.y-X3.S1.y then to X3.S1 
    line from X2.S1 to (B,X2.S1); dot
  ] with .X2.S2 at L.w-(dimen_,0)

  line from T.X1.S2 to (L.T3,T.X1.S2); dot 
  line from T.X2.S2 to (L.T2,T.X2.S2); dot 
  line from T.X3.S2 to (L.T1,T.X3.S2); dot 
] with .sw at Singlephase.se+(0.3,0)

.PE
