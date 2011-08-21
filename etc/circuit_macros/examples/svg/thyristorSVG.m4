.PS
# `thyristor.m4'
cct_init

down_
B: [

R1: [
  {Q: thyristor
   "A" at Q.A rjust above
   "K" at Q.K rjust below
   "G" at Q.G ljust
   }
  {`"thyristor"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q1: thyristor(,B)
   "T1" at Q1.T1 rjust above
   "T2" at Q1.T2 rjust below
   "G" at Q1.G below ljust
   }
  {`B: "...(,B)"' at Q1.s+(0,-0.25) }

  move right 0.75 ; down_
  { scr(down_ dimen_*0.8,BR,Q)
   "T1" at Q.T1 ljust above
   "T2" at Q.T2 ljust below
   "G" at Q.G below rjust
   }
  {`"...(,BR)"' at Q.s+(0,-0.21) }

  move right 1 ; down_
  {Q: thyristor(,BGE)
   "T1" at Q.T1 rjust above
   "T2" at Q.T2 rjust below
   "G" at Q.G below ljust
   }
  {`"...(,BGE)"' at Q.s+(0,-0.21) }

  move right 1 ; down_
  {Q: thyristor(,BRGE)
   "T1" at Q.T1 rjust above
   "T2" at Q.T2 rjust below
   "G" at Q.G rjust
   }
  {`BR:"...(,BRGE)"' at Q.s+(0,-0.21) }

  move right 0.75 ; down_
  {Q: thyristor(,C) at (Here,last [])
   "A" at Q.A ljust above
   "K" at Q.K ljust below
  thinlines_
   arrow <- right 0.2 from Q.G chop 0.01
   "G" ljust
  thicklines_
   }
  {`"...(,C)"' at (Q,BR) }
  ]
R2: [
  down_
  {Q: thyristor(,ARE) at (Here,last [])
   "A" at Q.A rjust above
   "K" at Q.K below
   "G" at Q.G rjust below
   }
  {`"...(,ARE)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UA) at (Here,last [])
   "A" at Q.A rjust above
   "K" at Q.K rjust below
   "G" at Q.G ljust below
   }
  {`"...(,UA)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UAV) at (Here,last [])
   "A" at Q.A rjust above
   "K" at Q.K rjust below
   thinlines_
   spline <- from Q.G+(1pt__,0) up 0.1 right 0.05 then right 0.05
   "G" ljust
   thicklines_
   }
  {`"...(,UAV)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UAH) at (Here,last [])
   "A" at Q.A rjust above
   "K" at Q.K rjust below
   "G" at Q.G ljust
   }
  {`"...(,UAH)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UAN) at (Here,last [])
   "A" at Q.A rjust above
   "K" at Q.K rjust below
   "G" at Q.G ljust
   }
  {`"...(,UAN)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UANRE) at (Here,last [])
   "A" at Q.A above
   "K" at Q.K rjust below
   "G" at Q.G rjust above
   }
  {`"...(,UANRE)"' at Q.s+(0,-0.30) }

  ] with .nw at R1.sw + (0,-0.35)
]
move left 0.3 from B.w
move right 0.3 from B.e

.PE
