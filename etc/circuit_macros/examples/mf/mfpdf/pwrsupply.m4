% Pwrsupply.m4
.PS
cct_init
  linewid = linewid*1.2

  down_
T1:transformer
  line left_ elen_/4 from T1.P1
  dot
  fuse(left_ elen_/3)
  dot
  reversed(`switch')
  gap(down_ to (Here,T1.P2))
  { fuse(right_ 2*dimen_/5 at last []) }
  line to T1.P2
Bridge: [
  blen = dimen_/2
  T: dot
  E: dot(at T+(blen,-blen))
  W: dot(at T-(blen,blen))
  B: dot(at T+(0,-blen*2))
    diode(from T to E)
    diode(from B to E)
    diode(from W to T)
    diode(from W to B)
  ] with .W at .5<T1.S1,T1.S2>+ (dimen_,0)
  line from Bridge.T to (T1.S1,Bridge.T) then to T1.S1
  line from Bridge.B to (T1.S2,Bridge.B) then to T1.S2
  line right_ dimen_*4/3 from Bridge.E
  gap(down_ Bridge.ht*5/8); llabel(+,,-)
  ground
  line left_ (Here.x-Bridge.E.x)/2
  { dot
    reversed(`capacitor',to (Here,Bridge.E),C)
    dot }
  line to (Bridge.W,Here) then to Bridge.W

.PE
