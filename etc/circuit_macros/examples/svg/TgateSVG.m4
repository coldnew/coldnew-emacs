.PS
# Tgate.m4
cct_init
right_

[ { Q: tgate
    "A" at Q.A rjust
    "B" at Q.B ljust
    "G" at Q.G ljust below
    "Gb" at Q.Gb ljust above
   `"tgate"' wid 0.5 at Q.A-(0.25,0) rjust
    }

  move down_ linewid; right_
  { Q: tgate(,L)
    "A" at Q.A rjust
    "B" at Q.B ljust
    "G" at Q.G rjust above
    "Gb" at Q.Gb ljust below
   `"tgate(,L)"' at Q.A-(0.25,0) rjust
    move left 0.75; right
    }
  ]

[ { Q: tgate(,B)
    "A" at Q.A rjust
    "B" at Q.B ljust
    "G" at Q.G ljust
   `"tgate(,B)"' at Q.G-(0,10bp__) below
    }
  ] with .Q.A at last [].Q.B+(0.5,linewid/2)

[ { Q: ptrans
    "A" at Q.A rjust
    "B" at Q.B ljust
    "G" at Q.G rjust below
    "Gb" at Q.Gb rjust
   `"ptrans"' at Q.B+(0.25,0) ljust
    }

  move down_ linewid; right_
  { Q: ptrans(,L)
    "A" at Q.A rjust
    "B" at Q.B ljust
    "G" at Q.G ljust above
    "Gb" at Q.Gb ljust below
   `"ptrans(,L)"' wid 0.75 at Q.B+(0.25,0) ljust
    }
  ] with .Q.Gb at (last [].e.x+elen_/2+0.5,1st [].Q.Gb.y)

.PE
