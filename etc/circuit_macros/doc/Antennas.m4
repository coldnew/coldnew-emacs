% `Antennas.m4'
.PS
cct_init
s_init(Antennas)
sinclude(CMman.dim)

  define(`elen_',elen_*0.9)
  B: Here
  A: antenna
       s_box(T) below at A.T
       s_box(`\tt an`'tenna') at A.n above
  A: antenna(,T) at A+(elen_,0)
       s_box(T) below at A.T
       s_box(`\tt (,T)') at A.n above
  A: antenna(,,L) at A+(elen_,0)
       s_box(T1) below rjust at A.T1
       s_box(T2) below ljust at A.T2
       s_box(`\tt (,,L)') at A.n above
  A: antenna(,T,L) at A+(elen_,0)
       s_box(T1) below rjust at A.T1
       s_box(T2) below ljust at A.T2
       s_box(`\tt (,T,L)') at A.n above
  A: antenna(,,T) at A+(elen_,0)
       s_box(T) below at A.T
       s_box(`\tt (,,T)') at A.n above
  A: antenna(,,S) at A+(elen_,0)
       s_box(T1) below rjust at A.T1
       s_box(T2) below ljust at A.T2
       s_box(`\tt (,,S)') at A.n above
  A: antenna(,,D) at A+(elen_,0)
       s_box(T1) below rjust at A.T1
       s_box(T2) below ljust at A.T2
       s_box(`\tt (,,D)') at A.n above
  A: antenna(,,P) at A+(elen_,0)
       s_box(T) below at A.T
       s_box(`\tt (,,P)') at A.n above
  A: antenna(,,F) at A+(elen_,0)
       s_box(T) below at A.T
       s_box(`\tt (,,F)') at A.n above
.PE
