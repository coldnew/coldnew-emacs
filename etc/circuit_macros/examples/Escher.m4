.PS
include(HOMELIB_`'lib3D.m4)
  setview(-45,20)
  bwid = 2.8
  bh = 1.5
  bdp = 1.8
  bth = 0.3
  A: project(0,0,0)
  AA: A+(project(bth,bth,-bth))
  B: project(0,bwid,0)
  BB: B+(project(bth,-bth,-bth))
  C: project(bdp,bwid,0)
  D: project(bdp,0,0)
  E: project(0,0,-bh)
  F: project(bdp,0,-bh)
  G: project(bdp,bwid,-bh)
  H: B + (project(0,0,-bh))

  line from A to B then to C then to D then to A then to E then to F \
    then to G then to H then to E
  line from C to G
  line from A+(project(bth,bth,0)) to B+(project(bth,-bth,0)) \
    then to C+(project(-bth,-bth,0))
  L1: line to D+(project(-bth,bth,0))
  L2: line to A+(project(bth,bth,0))
  L4: line from E+(project(bth,bth,0)) to H+(project(bth,-bth,0))
  L3: line to C+(project(-bth,-bth,-bh))
  line to F+(project(-bth,bth,0)) then to A+(project(bth,bth,-bh))

  L5: line from D+(project(-bth,0,-bth)) to A+(project(bth,0,-bth))
  line to E+(project(bth,0,bth))
  L6: line to Here+(project(0,bwid-2*bth,0))
  L7: line from D+(project(0,bth,-bth)) to C+(project(0,-bth,-bth))
    line to G+(project(0,-bth,bth))
  L8: line to Here+(project(-bdp+2*bth,0,0))
  Tmp: line invis from L8.start+(project(-bth,0,0)) up bh
  line from Tmp.start to Intersect_(Tmp,L7)
  Tmp: line invis from L8.end up bh
  line from L8.end to Intersect_(Tmp,L7)
  line from Intersect_(Tmp,L1) to BB+(project(bth,0,0))
  Tmp: line invis to Here+(project(bdp,0,0))
  line from Tmp.start to Intersect_(Tmp,L1)

  Tmp: line invis from H up bh
  line from H to Intersect_(Tmp,L7)
  line from Intersect_(Tmp,L1) to B+(project(bth,-bth,0))

  Tmp: line invis from AA to AA+(project(0,bwid,0))
  Tmp2: line invis up bh from L6.end
  line from Intersect_(Tmp,L2) to Intersect_(Tmp2,Tmp) \
    then to Intersect_(Tmp2,L1)
  line from L6.end to Intersect_(Tmp2,L7)

  Tmp: line invis down bh from AA
  line from Intersect_(Tmp,L6) to Intersect_(Tmp,L5)

  Tmp: line invis down bh from D
  line from D to Intersect_(Tmp,L6)
  line from Intersect_(Tmp,L4) to F+(project(-bth,bth,0))

  Tmp: line invis from L5.start to L5.start+(project(0,0,-bh))
  move to E+(project(0,2*bth,0))
  Tmp2: line invis to Here+(project(bdp,0,0))
  line from L5.start to Intersect_(Tmp,L6)
  line from Intersect_(Tmp,L4) to Intersect_(Tmp,Tmp2) \
    then to Intersect_(Tmp2,L4)

  Tmp: line invis from L7.start to L7.start+(project(0,0,-(bh-bth)))
  line from L7.start to Intersect_(Tmp,L6)

  move to G+(project(-2*bth,0,0))
  Tmp2: line invis to Here+(project(0,-bwid,0))
  line from Intersect_(Tmp2,L3) to Intersect_(Tmp,Tmp2)
  line from Intersect_(Tmp,L4) to Intersect_(Tmp2,Tmp)


.PE
