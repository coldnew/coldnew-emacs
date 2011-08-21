.PS
# Opamp.m4
sinclude(FOpamp.dim)
sinclude(CMman.dim)
s_init(Opamp)
cct_init

  A: opamp
     thinlines_
       "`opamp'" at A.s-(0,0.25)
       "Out" below at A.Out
       spline <- from A.In1 left 0.1 then up 0.1 left 0.1
         "In1" wid 0.25 rjust above
       spline <- from A.In2 left 0.1 then down 0.1 left 0.1
         "In2" rjust below
       arrow <- up 0.1 right 0.1 from A.E1
         "E1" ljust above
       arrow <- down 0.1 right 0.1 from A.E2
         "E2" ljust below
     thicklines_
     Point_(15)
  B: opamp(,,,,PR) with .sw at A.se+(0.65,0)
     thinlines_
       "`Point_(15); opamp(,,,,PR)'" at B.s-(0,0.25)
       spline <- right arrowht up arrowht from B.V1 then right 0.1
       "V1" ljust
       spline <- right arrowht down arrowht from B.V2 then right 0.1
       "V2" ljust
     thicklines_
     Point_(90)
  C: opamp with .sw at B.se+(1.05,0)
       "`Point_(90); opamp'" at C.s-(0,0.25)
     right_
  D: opamp(,,,,T) with .sw at C.se+(0.65,0)
       "`opamp(,,,,T)'" wid 0.75 at D.s-(0,0.25)

.PE
