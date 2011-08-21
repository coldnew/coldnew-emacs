% `bistable.m4'
.PS
cct_init                                 # Default initialization

   up_
rgbdraw(1,0,0,
Q1: bi_tr(,R)                            # Q1 upward with base to the right
   llabel(,Q_1) )                        # Label to left of last []
rgbdraw(0,0,1,
Q2: bi_tr with .C at Q1.C+(elen_*7/4,0)  # Q2 with base to left
   rlabel(,Q_2) )

   ground(at Q1.E)
   ground(at Q2.E)

   line right_ dimen_/3 from Q1.B        # Extend the base conductors
B1: Here
   line left_ dimen_/3 from Q2.B
B2: Here

   resistor(up_ elen_*0.75 from Q1.C); llabel(,R_L) # Load resistors
   line to (Q2.C,Here) "$V_{\hbox{cc}}$" below
   resistor(down_ to Q2.C); llabel(,R_L)

   move to Q1.C+(0,dimen_/5)
   { resistor(right_ to (B1,Here)); llabel(,R_1)
X: line to B2 }                          # Label the line to be jumped
   resistor(from (Q2.C,Here) left_ to (B2,Here)); rlabel(,R_1)
   crossover(to B1,R,X)

   resistor(down_ elen_*0.75); rlabel(,R_2)   # Bias resistors
   {"$-V_{\hbox{cc}}$" rjust}
   line to (B2,Here)
   resistor(up_ to B2); rlabel(,R_2)
.PE
