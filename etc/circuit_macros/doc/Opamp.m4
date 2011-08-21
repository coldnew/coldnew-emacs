% Opamp.m4
.PS
sinclude(FOpamp.dim)
sinclude(CMman.dim)
s_init(Opamp)
cct_init

  A: opamp
     thinlines_
       s_box(`\tt opam{}p') at A.s-(0,0.35)
       s_box(\tt Out) below at A.Out
       spline <- from A.In1 left 0.1 then up 0.1 left 0.1
         s_box(\tt In1) rjust above
       spline <- from A.In2 left 0.1 then down 0.1 left 0.1
         s_box(\tt In2) rjust below
       arrow <- up 0.1 right 0.1 from A.N
         s_box(\tt N) ljust above
       arrow <- up 0.1 right 0.1 from A.E1
         s_box(\tt E1) ljust above
       arrow <- up 0.1 right 0.1 from A.E
         s_box(\tt E) ljust above
       arrow <- down 0.1 right 0.1 from A.E2
         s_box(\tt E2) ljust below
       arrow <- down 0.1 right 0.1 from A.S
         s_box(\tt S) ljust below
       arrow <- left 0.2 from A.W
         s_box(\tt W) rjust
     thicklines_
     Point_(15)
  B: opamp(,,,,PR) with .sw at A.se+(0.65,0)
     thinlines_
       s_box(`\tt Point\_(15); opam{}p(,,,,PR)') at B.s-(0,0.20)
       spline <- right arrowht up arrowht from B.V1 then right 0.1
       s_box(\tt V1) ljust
       spline <- right arrowht down arrowht from B.V2 then right 0.1
       s_box(\tt V2) ljust
     thicklines_
     Point_(90)
  C: opamp with .sw at B.se+(1.05,0)
       s_box(`\tt Point\_(90); opam{}p') at C.s-(0,0.35)
     right_
  D: opamp(,,,,T) with .sw at C.se+(0.65,0)
       s_box(`\tt opam{}p(,,,,T)') at D.s-(0,0.20)

.PE
