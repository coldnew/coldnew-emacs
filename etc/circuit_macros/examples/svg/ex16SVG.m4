.PS
# ex16.m4
cct_init
[
define(`elen_',linewid*1.2)
  circlerad = delay_rad_
  {"input" above ljust}
  line right_
J: Here
  delay
K: Here
  delay
L: Here
S1: circle "+" at (K.x,K.y+linewid)
S2: circle "+" at (L,S1)
  arrow up_ S1.y-J.y from J then to S1.w
  arrow from K to S1.s
  arrow from S1.e to S2.w
  arrow from L to S2.s
S3: circle "+" at (L.x,L.y-linewid)
  arrow down_ J.y-S3.y from J then to S3.w
  arrow from L to S3.n
  arrow from S2.e right_ linewid/2 then down_ (S2.y-S3.y)/2-circlerad \
    then right_ linewid/2
  arrow from S3.e right_ linewid/2 then up_   (S2.y-S3.y)/2-circlerad \
   then right_ linewid/2
  "output" at 0.5<last arrow.end, 2nd last arrow.end>
  ]
[
  circlerad = 0.35/2
  d = linewid*1.75
  S00: circle "00"
  S10: circle "10" at S00+(d,d)
  S01: circle "01" at S00+(d,-d)
  S11: circle "11" at S01+(d,d)
  dx = circlerad*cosd(60)
  dy = circlerad*sind(60)
  brad = circlerad*1.3
  qrad = circlerad*6

  arc -> cw from S00+(-dx,-dy) to S00+(-dx,dy) rad brad \
    with .c at S00-(dx+sqrt(brad^2-dy^2),0)
  "0/00" at last arc.w rjust
  right
  arc -> cw from S00+(dx,dy) to S10+(-dy,-dx) rad qrad
  "1/11" at last arc.nw rjust above
  right
  arc -> cw from S10+(dy,-dx) to S11+(-dx,dy) rad qrad
  "1/01" at last arc.ne ljust above
  up_
  arc -> cw from S11+(dx,dy) to S11+(dx,-dy) rad brad \
    with .c at S11+(dx+sqrt(brad^2-dy^2),0)
  "1/10" wid 0.3 at last arc.e ljust
  down_
  arc -> cw from S11+(-dx,-dy) to S01+(dy,dx) rad qrad
  "0/01" at last arc.se ljust below
  left_
  arc -> cw from S01+(-dy,dx) to S00+(dx,-dy) rad qrad
  "0/11" at last arc.sw rjust below
  up_
  arc -> cw from S01+(-dx,dy) to S10+(-dx,-dy) rad qrad*2
  "1/00" at last arc.w rjust
  down_
  arc -> cw from S10+(dx,-dy) to S01+(dx,dy) rad qrad*2
  "0/10" at last arc.e ljust
  ] with .sw at last [].se+(0.4,0)
.PE
