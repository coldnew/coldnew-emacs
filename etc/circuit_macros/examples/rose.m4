% rose.m4
divert(-1)

define(`rose',`point_(`$2'); CC: (vec_(`$1',0)); rrad = `$3'
  move to CC
  tmp =  rrad-stripwid
  {circle shaded "gold" rad rrad-sthick at CC}
  {circle shaded "gold" rad tmp-sthick at CC}
  {line to rvec_(rrad,0) chop tmp chop 0 }
  {line to rvec_(-rrad,0) chop tmp chop 0 }
  {line to rvec_(0,rrad) chop tmp chop 0 }
  {line to rvec_(0,-rrad) chop tmp chop 0 }
  {line from rvec_(-tmp/sqrt(2),-tmp/sqrt(2)) to rvec_(tmp/sqrt(2),tmp/sqrt(2))}
  {line from rvec_(-tmp/sqrt(2),tmp/sqrt(2)) to rvec_(tmp/sqrt(2),-tmp/sqrt(2))}
  {circle shaded "gold" rad tmp-stripwid-sthick at CC}
  tmp = last circle.rad/sqrt(2)
  rgbfill(gold_,line thick stripthick from rvec_(-tmp,0) to rvec_(0,-tmp) \
    then to rvec_(tmp,0) then to rvec_(0,tmp) then to rvec_(-tmp,0))
  ')

define(`spt_',`/72.27')
define(`gold_',`1,0.84,0')

divert(0)dnl
ifelse(ifpstricks(T)`'ifmpost(T)`'ifpostscript(T)`'ifpgf(T),,
`This diagram must be produced as PSTricks, postscript, or MetaPost output',
`.PS
scale = 1.2

[ lsk = 1.2
  sector = 360*dtor_/16
  exterior = 2.49
  outer_circle = exterior-0.55
  inner_circle = 1.1
  spoke_tail = 0.41
  spokethick = 0.09/(1spt_) # points
  stripwid = 0.05
  stripthick = 1.6/1.2
  midroserad = inner_circle*sin(sector/2)-stripwid/2
  outerroserad = outer_circle*sin(sector/4)-stripwid/2
  del = 0.05

ifpstricks(`command "\definecolor{`gold'}{rgb}{gold_}"')
ifpgf(`command "\definecolor{`gold'}{rgb}{gold_}"')
ifmpost(`define(`gold',`(gold_)')')
ifpostscript(`define(`gold',`1 0.84 0')')

C: (0,0)
  circle rad exterior fill_(0) at C
  circle shaded "gold" rad outer_circle at C
  circle rad spoke_tail fill_(0) at C
  linethick = stripthick
  sthick = linethick/2 spt_
  for i=0 to 15 do {
    move to rect_(spoke_tail+1.5*stripwid,sector*i);
    for j=-1 to 1 by 2 do {
      {point_(sector*(i+j*0.5))
       line to rvec_(outer_circle-spoke_tail,0) }
      { point_(sector*i)
        move to rvec_(0,j*spokethick spt_)
        line to rvec_(outer_circle-spoke_tail,0) \
          chop outer_circle-inner_circle chop 0
        }
      }
    crad =  0.25 +1.5*stripwid
    cthick =  1.5*stripwid/(1spt_)
    X: rect_(inner_circle+midroserad,sector*i)
    circle outlined "gold" rad crad-(cthick/2 spt_) thick cthick at X
    circle rad crad - sthick at X
    }
  for i=0 to 15 do {
    X: rect_(inner_circle+midroserad,sector*i)
    circle fill_(0) rad crad-1.5*stripwid-sthick at X

    for j = -1 to 1 by 2 do { move to X+vec_(0.20,j*0.14)
      point_(sector*(i+j*0.25))
      line to rvec_(1.5*stripwid,0) }

    circle shaded "gold" rad midroserad*2/3-sthick at X
    circle rad midroserad*2/3-stripwid-sthick at X
    line from X to -(midroserad*2/3/(inner_circle+midroserad))<X,C>
    }

  crad = spoke_tail+7*stripwid
  cthick = 2*stripwid/(1spt_)
  circle outlined "gold" rad crad-cthick/2 spt_ thick cthick at C
  circle rad spoke_tail+1.5*stripwid-sthick at C
  for i=5 to 7 do { circle rad spoke_tail+i*stripwid-sthick at C }
                               # Spokes and roses
  for i=0 to 15 do {
    rose(inner_circle,sector*i,midroserad)
    line thick spokethick/lsk from C to rect_(outer_circle,sector*(i-0.5))
    line thick spokethick/lsk from C to rect_(outer_circle,sector*i) \
      chop inner_circle+midroserad*2 chop 0
    for j=-1 to 1 by 2 do {
      X: rect_(outer_circle+outerroserad-linethick spt_,sector*(i+j*0.25))
      linethick = 1/lsk
      circle colored "gold" rad outerroserad*1/2-1/lsk/2 spt_ at X
      linethick = stripthick
      circle fill_(0) rad outerroserad*1/3-sthick at X
      point_(sector*(i+j*0.25))
      line from X to X + vec_(outerroserad,0)
      rose(outer_circle,sector*(i+j*0.25),outerroserad)
      }
    }
                               # Outer decorations
  for i=0 to 31 do {
    move to rect_(exterior-0.25,sector*i/2); point_(sector*i/2)
    rgbfill(gold_,
      { line thick stripthick from rvec_(del,3.5*del) to rvec_(-del,0) \
        then to rvec_(del,-3.5*del) then to rvec_(del,3.5*del) })
    circle rad stripwid-sthick at Here
    }
  smrad = 0.08
  for i=0 to 7 do {
    point_(sector*2*i)
    move to C + vec_(midroserad+smrad/4,0)
    { circle shaded "gold" rad smrad-sthick at Here}
    { circle shaded "gold" rad smrad/2-sthick at Here}
    line from rvec_(smrad/2,0) to rvec_(smrad,0) 
    }
 
  tmp =  midroserad-stripwid
  circle shaded "gold" rad midroserad-sthick at C
  circle shaded "gold" rad tmp-sthick at C
  CQ: circle invis rad tmp-linethick*4/2 spt_ at C
  line from CQ.e to CQ.n to CQ.w to CQ.s to CQ.e to CQ.n
  CQ: 0.5<CQ.e,CQ.n>
  tmp = (distance(CQ,C) - linethick*3/2 spt_)*sqrt(2)
  box wid tmp ht tmp at C
  ] with .sw at 1,1
.PE')
