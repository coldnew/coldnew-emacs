% `FlipFlop.m4'
ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')
.PS
log_init
s_init(Flipflop)
sinclude(CMman.dim)
right_
  { Q1: FlipFlop(D,Q1)
    `"\tt FlipFlop(D,Q1)"' at last [].Chip.s+(0,-0.15)
    h1 = L_unit*20
    w1 = L_unit*12
    FlipFlop(T,Q2,ht h1 wid w1 fill_(0.9)) with .w at last [].e+(0.25,0)
    `"\tt FlipFlop(T,Q2,ht h1 wid w1 fill\_(0.9))"' \
      at (last [].Chip,2nd last [].Chip.s)+(0,-0.35)
    FlipFlop(RS) with .w at last [].e+(0.25,0)
    `"\tt FlipFlop(RS)"' at last [].Chip.s+(0,-0.15)
    FlipFlop(JK) with .w at last [].e+(0.25,0)
    `"\tt FlipFlop(JK)"' at last [].Chip.s+(0,-0.35)
    }
  move down 1.5; right_
  { FlipFlop6(,DnCKQNQlb)
    `"\tt FlipFlop6(,DnCKQNQlb)"' at last [].Chip.s+(0,-0.3)
    FlipFlop6(,TCKQlb) with .w at last [].e+(0.5,0)
    `"\tt $\ldots$(,TCKQlb)"' at last [].Chip.s+(0,-0.3)
    FlipFlopJK(,JCKKQnCLRlb) with .Chip.c at last [].e+(1.0,0)
    `"\tt FlipFlopJK(,JCKKQnCLRlb)"' at last [].Chip.s+(0,-0.3)
    }

  move down 1.6 right 0.3; right_
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
