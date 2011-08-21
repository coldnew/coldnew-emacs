% `Multiplexor.m4'
ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')
.PS
log_init
s_init(Multiplexor)
sinclude(CMman.dim)
right_
  Mux(4,Mx1)
  `"\tt Mux(4,Mx1)"' at last [].Chip.s+(0,-0.3)
  `"\sl In0"' at last [].In0.end rjust
  `"\sl In1"' at last [].In1.end rjust
  `"\sl In2"' at last [].In2.end rjust
  `"\sl In3"' at last [].In3.end rjust
  `"\sl Out"' at last [].Out.end above
  `"\sl Sel"' at last [].Sel.end ljust above
  Mux(4,,L) with .Chip.c at last [].Chip.c+(1.25,0)
  `"\tt Mux(4,,L)"' at last [].Chip.s+(0,-0.3)
  Mux(4,,T) with .Chip.c at last [].Chip.c+(1.25,0)
  `"\tt Mux(4,,T)"' at last [].Chip.s+(0,-0.3)
  Mux(4,,LT) with .Chip.c at last [].Chip.c+(1.25,0)
  `"\tt Mux(4,,LT)"' at last [].Chip.s+(0,-0.3)

.PE
