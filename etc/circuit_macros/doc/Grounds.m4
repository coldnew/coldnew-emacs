% `Grounds.m4'
.PS
cct_init

movewid = 1.15
Ground: ground
   move right movewid*3/4
G1: ground(,T)
   move right movewid*3/4
G2: ground(,,F)
   move right movewid*3/4
Gx: ground(,,E)
   move right movewid*3/4

B: Here+(0,-0.5) ; {
   "{\tt `ground'}" at (Ground,B)
   "{\tt `ground'(,T)}" at (G1,B)
   "{\tt (,,F)}" at (G2,B)
   "{\tt (,,E)}" at (Gx,B)}

G3: [ground(,,S,90) ] with .n at Here
   move right movewid*3/4 from G3.n
G4: ground(,,L)
   move right movewid*3/4
G5: ground(,,P)

C: Here+(0,-0.5)
   "{\tt (,,S,90)}" at (G3,C)
   "{\tt (,,L)}" at (G4,C)
   "{\tt (,,P)}" at (G5,C)

.PE
