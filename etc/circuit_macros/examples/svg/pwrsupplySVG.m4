.PS
# Pwrsupply.m4
cct_init
ifdef(`m4pco',`resetrgb')
  linewid = linewid*1.2

  down_
T:transformer
  line left_ elen_/4 from T.P1
  rgbdraw(0,0,255,fuse(left_ elen_/3,D))
  reversed(`switch')
  gap(down_ to (Here,T.P2))
  { fuse(right_ 2*dimen_/5 at last []) }
  line to T.P2
  blen = dimen_/2
W: T.TS+(dimen_,0)
N: W+(blen,blen)
S: W+(blen,-blen)
E: S+(blen,blen)
  diode(from W to N)
  diode(from S to E)
G:gap(from E+(dimen_*4/3,0) down_ (E.y-S.y)*5/4); llabel("+" wid 0.1*scale,,-)
C:capacitor(down_ G.start.y-G.end.y from 0.5 between E and G.start,C)

rgbdraw(255,0,0,
  line from T.S1 to (T.S1,N) then to N; dot
  diode(to E); dot
  line from E to G.start; dot
  dot(at C.start)
)

rgbdraw(0,255,0,
  dot(at C.end)
  dot(at G.end)
  ground
  line to (W,Here) then to W; dot
  diode(to S); dot
  line to (T.S2,Here) then to T.S2
)

.PE
