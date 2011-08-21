.PS
# `Antennas.m4'
cct_init
s_init(Antennas)
sinclude(CMman.dim)

  define(`elen_',elen_*0.9)
  B: Here
  A: antenna
       "T" below at A.T
       "`antenna'" wid 0.6 at A.n above
  A: antenna(,T) at A+(elen_,0)
       "T" below at A.T
       "`(,T)'" at A.n above
  A: antenna(,,L) at A+(elen_,0)
       "T1" below rjust at A.T1
       "T2" below ljust at A.T2
       "`(,,L)'" at A.n above
  A: antenna(,T,L) at A+(elen_,0)
       "T1" below rjust at A.T1
       "T2" below ljust at A.T2
       "`(,T,L)'" at A.n above
  A: antenna(,,T) at A+(elen_,0)
       "T" below at A.T
       "`(,,T)'" at A.n above
  A: antenna(,,S) at A+(elen_,0)
       "T1" below rjust at A.T1
       "T2" below ljust at A.T2
       "`(,,S)'" at A.n above
  A: antenna(,,D) at A+(elen_,0)
       "T1" below rjust at A.T1
       "T2" below ljust at A.T2
       "`(,,D)'" at A.n above
  A: antenna(,,P) at A+(elen_,0)
       "T" below at A.T
       "`(,,P)'" at A.n above
  A: antenna(,,F) at A+(elen_,0)
       "T" below at A.T
       "`(,,F)'" at A.n above
.PE
