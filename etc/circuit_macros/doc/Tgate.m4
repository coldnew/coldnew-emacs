% Tgate.m4
.PS
cct_init
right_

[ { Q: tgate
    "$A$" at Q.A rjust
    "$B$" at Q.B ljust
    "$G$" at Q.G ljust below
    "$Gb$" at Q.Gb ljust above
   `"\tt tgate"' at Q.A-(0.25,0) rjust
    }

  move down_ linewid; right_
  { Q: tgate(,L)
    "$A$" at Q.A rjust
    "$B$" at Q.B ljust
    "$G$" at Q.G ljust above
    "$Gb$" at Q.Gb ljust below
   `"\tt tgate(,L)"' at Q.A-(0.25,0) rjust
    }
  ]

[ { Q: tgate(,B)
    "$A$" at Q.A rjust
    "$B$" at Q.B ljust
    "$G$" at Q.G ljust
   `"\tt tgate(,B)"' at Q.G-(0,10bp__) below
    }
  ] with .Q.A at last [].Q.B+(0.5,linewid/2)

[ { Q: ptrans
    "$A$" at Q.A rjust
    "$B$" at Q.B ljust
    "$G$" at Q.G rjust below
    "$Gb$" at Q.Gb rjust
   `"\tt ptrans"' at Q.B+(0.25,0) ljust
    }

  move down_ linewid; right_
  { Q: ptrans(,L)
    "$A$" at Q.A rjust
    "$B$" at Q.B ljust
    "$G$" at Q.G ljust above
    "$Gb$" at Q.Gb ljust below
   `"\tt ptrans(,L)"' at Q.B+(0.25,0) ljust
    }
  ] with .Q.Gb at (last [].e.x+elen_/2+0.5,1st [].Q.Gb.y)

.PE
