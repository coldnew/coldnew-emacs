% blight.m4
% The background for the web pages.
.PS
define(`density',0.95)
cct_init
  B:box invis wid 0.5 ht 0.5
  linethick_(1.5)
setrgb(density, density, density)
  capacitor(from B.n to B.w ,C)
  inductor(from B.n to B.e ,W,2)
  define(`m4fill',density)
  diode(from B.e to B.s )
  resistor(from B.s to B.w chop linethick/2 pt__)
resetrgb
.PE
