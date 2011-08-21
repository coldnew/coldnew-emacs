% `Potentiometers.m4'
.PS
cct_init

  down_
  P: potentiometer(down_ dimen_)
    "\tt `potentiometer'(down\_ dimen\_)" at P.End +(0,-0.25)
    "\sl Start" ljust at P.Start
    "\sl End" ljust at P.End
    "\sl T1" below ljust at P.T1

  R: potentiometer(down_ dimen_,,0.5,-0.2) \
      with .Start at P.Start + (1.7,0)
    "\tt ...(down\_ dimen\_,,0.5,-0.2)" at R.End +(0,-0.12)
    "\sl Start" ljust at R.Start
    "\sl End" ljust at R.End
    "\sl T1" below rjust at R.T1

  Q: potentiometer(down_ dimen_,,0.25,-0.2,0.75,0.2) \
      with .Start at R.Start + (1.7,0)
    "\tt ...(down\_ dimen\_,,0.25,-0.2,0.75,0.2)" at Q.End+(0,-.25)
    "\sl Start" ljust at Q.Start
    "\sl End" ljust at Q.End
    "\sl T1" below rjust at Q.T1
    "\sl T2" below ljust at Q.T2

.PE
