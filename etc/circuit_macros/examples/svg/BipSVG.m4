.PS
# `Bip.m4'
cct_init

   up_
Q1: bi_tr(up_ dimen_) 
   thinlines_
   box dotted ht last [].ht wid last [].wid at last []
   thicklines_
   "E" at Q1.E-(0,0.05) below    
   "B" at Q1.B rjust    
   "C" at Q1.C above    
   "`bi_tr(up_ dimen_)'" wid 1.2 at Q1.s + (0,-0.35) below

Q2: bi_tr(,R) with .E at Q1.E+(0.25,0) 
   thinlines_
   box dotted ht last [].ht wid last [].wid at last []
   thicklines_
   "E" at Q2.E-(0,0.05) below    
   "B" at Q2.B ljust    
   "C" at Q2.C above    
   "`bi_tr(,R)'" at Q2.s + (0,-0.2) below

Q3: bi_tr(,,P) with .C at Q2.C+(1.15,0) 
   thinlines_
   box dotted ht last [].ht wid last [].wid at last []
   thicklines_
   "E" at Q3.E-(0,0.05) below    
   "B" at Q3.B rjust    
   "C" at Q3.C above    
   "`bi_tr(,,P)'" at Q3.s + (0,-0.2) below

Q4: bi_tr(,,,E) with .C at Q3.C+(0.9,0) 
   thinlines_
   box dotted ht last [].ht wid last [].wid at last []
   thicklines_
   "E" at Q4.E-(0,0.05) below    
   "B" at Q4.B rjust    
   "C" at Q4.C above    
   "`bi_tr(,,,E)'" at (Q4.s,Q4.E) + (0,-0.2) below

Q5: igbt(,,) with .E at Q4.E+(0.65,0) 
   thinlines_
   box dotted ht last [].ht wid last [].wid at last []
   thicklines_
   "E" at Q5.E-(0,0.05) below    
   "G" at Q5.G rjust    
   "C" at Q5.C above    
   "`igbt'" at Q5.s + (0,-0.2) below

Q6: igbt(,,LD) with .E at Q5.E+(0.55,0) 
   thinlines_
   box dotted ht last [].ht wid last [].wid at last []
   thicklines_
   "E" at Q6.E-(0,0.05) below    
   "G" at Q6.G rjust    
   "C" at Q6.C above    
   "`igbt(,,LD)'" at (Q6.s,Q6.E) + (0,-0.2) below

Q7: Darlington with .E at Q6.E+(1.2,0) 
   thicklines_
   "E" wid 0.15 at Q7.E-(0,0.05) below    
   "B" at Q7.B rjust    
   "C" at Q7.C above    
   "`Darlington'" at (Q7.s,Q7.E) + (0,-0.2) below

.PE
