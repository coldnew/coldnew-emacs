% Dac.m4
.PS
cct_init
s_init(Dacx)

  ar = 0.15

Q: dac
thinlines_
  arrow <- left ar from last [].In1; { move left 0.15 }
  "In1" above rjust at Here+(0.1,0)
  "\sl NW" at Q.NW above rjust
  "\sl SW" at Q.SW below rjust
  "\sl SE" at Q.SE below ljust
  "\sl NE" at Q.NE above ljust
  arrow <- up ar from Q.N1; "\sl N1" above
  arrow <- down ar from Q.S1; "\sl S1" below
  arrow <- right ar from Q.Out1; "\sl Out1" above ljust at Here-(0.1,0)
  arrow <- right ar down ar/2 from Q.C; "\sl C" ljust
  s_box(`\tt d`'ac') at Q.s+(0,-0.3) below
thicklines_

Q:dac(,,2,2,3,3) with .w at last [].e+(0.9,0)
thinlines_
  "DAC" "2" at Q.C
  arrow <- left ar from Q.In1; "\sl In1" rjust
  arrow <- left ar from Q.In2; "\sl In2" rjust
  arrow <- up ar from Q.N1; "\sl N1" above rjust
  arrow <- up ar from Q.N2; "\sl N2" above ljust
  arrow <- right ar from Q.Out1; "\sl Out1" ljust
  arrow <- right ar from Q.Out2; "\sl Out2" ljust
  arrow <- right ar from Q.Out3; "\sl Out3" ljust
  arrow <- down ar left ar/2 from Q.S1; "\sl S1" below rjust
  arrow <- down ar from Q.S2; "\sl S2" below
  arrow <- down ar right ar/2 from Q.S3; "\sl S3" below ljust
  s_box(
   `\tt Q: d`'ac(,,2,2,3,3); \"DAC\" \"2\" at Q.C') at Q.s+(0,-0.3) below
thicklines_

Q: adc with .nw at last [].ne + (0.9,0)
thinlines_
  "\sl NW" at Q.NW above rjust
  "\sl SW" at Q.SW below rjust
  "\sl SE" at Q.SE below ljust
  "\sl NE" at Q.NE above ljust
  arrow <- left ar from Q.In1; "\sl In1" rjust above at Here+(0.1,0)
  arrow <- up ar from Q.N1; "\sl N1" above
  arrow <- down ar from Q.S1; "\sl S1" below
  arrow <- right ar from Q.Out1; "\sl Out1" ljust above at Here-(0.1,0)
  arrow <- left ar down ar/2 from Q.C; "\sl C" rjust
  s_box(`\tt a`'dc') at Q.s+(0,-0.3) below
thicklines_

Q:adc(,,2,2,3,3) with .w at last [].e+(0.8,0)
thinlines_
  arrow <- left ar from Q.In1; "\sl In1" rjust
  arrow <- left ar from Q.In2; "\sl In2" rjust
  arrow <- up ar from Q.N1; "\sl N1" above rjust
  arrow <- up ar from Q.N2; "\sl N2" above ljust
  arrow <- right ar from Q.Out1; "\sl Out1" ljust
  arrow <- right ar from Q.Out2; "\sl Out2" ljust
  arrow <- right ar from Q.Out3; "\sl Out3" ljust
  arrow <- down ar left ar/2 from Q.S1; "\sl S1" below rjust
  arrow <- down ar from Q.S2; "\sl S2" below
  arrow <- down ar right ar/2 from Q.S3; "\sl S3" below ljust
  s_box(`\tt a`'dc(,,2,2,3,3)') at Q.s+(0,-0.3) below

.PE
