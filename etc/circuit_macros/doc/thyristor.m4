% `thyristor.m4'
.PS
cct_init

down_

R1: [
  {Q: thyristor
   "\sl A" at Q.A rjust above
   "\sl K" at Q.K rjust below
   "\sl G" at Q.G ljust
   }
  {`"\tt thyristor"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q1: thyristor(,B)
   "\sl T1" at Q1.T1 rjust above
   "\sl T2" at Q1.T2 rjust below
   "\sl G" at Q1.G below ljust
   }
  {`B: "\tt ...(,B)"' at Q1.s+(0,-0.25) }

  move right 0.75 ; down_
  { scr(down_ dimen_*0.8,BR,Q)
   "\sl T1" at Q.T1 ljust above
   "\sl T2" at Q.T2 ljust below
   "\sl G" at Q.G below rjust
   }
  {`"\tt ...(,BR)"' at Q.s+(0,-0.21) }

  move right 1 ; down_
  {Q: thyristor(,BGE)
   "\sl T1" at Q.T1 rjust above
   "\sl T2" at Q.T2 rjust below
   "\sl G" at Q.G below ljust
   }
  {`"\tt ...(,BGE)"' at Q.s+(0,-0.21) }

  move right 1 ; down_
  {Q: thyristor(,BRGE)
   "\sl T1" at Q.T1 rjust above
   "\sl T2" at Q.T2 rjust below
   "\sl G" at Q.G rjust
   }
  {`BR:"\tt ...(,BRGE)"' at Q.s+(0,-0.21) }

  move right 0.75 ; down_
  {Q: thyristor(,C) at (Here,last [])
   "\sl A" at Q.A ljust above
   "\sl K" at Q.K ljust below
  thinlines_
   arrow <- right 0.2 from Q.G chop 0.01
   "\sl G" ljust
  thicklines_
   }
  {`"\tt ...(,C)"' at (Q,BR) }
  ]
R2: [
  down_
  {Q: thyristor(,ARE) at (Here,last [])
   "\sl A" at Q.A rjust above
   "\sl K" at Q.K below
   "\sl G" at Q.G rjust below
   }
  {`"\tt ...(,ARE)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UA) at (Here,last [])
   "\sl A" at Q.A rjust above
   "\sl K" at Q.K rjust below
   "\sl G" at Q.G ljust below
   }
  {`"\tt ...(,UA)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UAV) at (Here,last [])
   "\sl A" at Q.A rjust above
   "\sl K" at Q.K rjust below
   thinlines_
   spline <- from Q.G+(1pt__,0) up 0.1 right 0.05 then right 0.05
   "\sl G" ljust
   thicklines_
   }
  {`"\tt ...(,UAV)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UAH) at (Here,last [])
   "\sl A" at Q.A rjust above
   "\sl K" at Q.K rjust below
   "\sl G" at Q.G ljust
   }
  {`"\tt ...(,UAH)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UAN) at (Here,last [])
   "\sl A" at Q.A rjust above
   "\sl K" at Q.K rjust below
   "\sl G" at Q.G ljust
   }
  {`"\tt ...(,UAN)"' at Q.s+(0,-0.25) }

  move right 0.75 ; down_
  {Q: thyristor(,UANRE) at (Here,last [])
   "\sl A" at Q.A above
   "\sl K" at Q.K rjust below
   "\sl G" at Q.G rjust above
   }
  {`"\tt ...(,UANRE)"' at Q.s+(0,-0.30) }

  ] with .nw at R1.sw + (0,-0.35)

.PE
