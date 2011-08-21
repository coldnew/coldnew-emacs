% fbfilter.m4
.PS
cct_init
#                                   `fbfilter( U|D|L|R|degrees, [R|L],
#                                              amp label, C label, R label )'
define(`fbfilter',
`[
  direction_(ifelse(`$1',,0,`$1'))  # Process arg 1
  eleminit_                         # Assign rp_ang, rp_len
  tmpang = rp_ang                   # Save rp_ang 
  hunit = elen_                     # Dimension parameters
  vunit = ifinstr(`$2',R,-)dimen_
 Q: opamp(,,,,`$2')
  move to Q.In`'ifinstr(`$2',R,2,1); line to rvec_(-hunit/4,0)
 J: dot
 R: resistor(to rvec_(-elen_,0))    # Two-term elements change rp_ang
  point_(tmpang)                    # so reset it.
 In: Here
  move to Q.In`'ifinstr(`$2',R,1,2); line to rvec_(-hunit/4,0)
 G: Here
  dot(at Q.Out)
  { line to rvec_(hunit/4,0)
 Out: Here } 
  line to rvec_(0,vunit)
 C: capacitor(to rvec_(-distance(Q.Out,0.5 between J and G),0)); point_(tmpang)
  line to J
  ifelse(`$3',,,"$`$3'$" at Q.C)
  ifelse(`$4',,,"$`$4'$" at C+vec_(0,-vunit/3))
  ifelse(`$5',,,"$`$5'$" at R+vec_(0,-vunit/4))
  ]')

F1: fbfilter(,,K_3,C_{24},R_4)
  ground(at F1.G)
  dot(at F1.In); line up_ elen_/4
F2: fbfilter(L,R,K_2,C_{23},R_3) with .In at F1.In
  ground(at F2.G)

.PE
