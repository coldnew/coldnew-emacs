.PS
# `Grounds.m4'
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
   "`ground'" wid 0.65 at (Ground,B)
   "`ground'(,T)" at (G1,B)
   "(,,F)" at (G2,B)
   "(,,E)" at (Gx,B)}

G3: [ground(,,S,90) ] with .n at Here
   move right movewid*3/4 from G3.n
G4: ground(,,L)
   move right movewid*3/4
G5: ground(,,P)

C: Here+(0,-0.5)
   "(,,S,90)" at (G3,C)
   "(,,L)" at (G4,C)
   "(,,P)" at (G5,C)

.PE
