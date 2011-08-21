% `Oblique.m4'
.PS
cct_init

Ct:dot; Point_(-60); capacitor(,C); dlabel(0.12,0.12,,,C_3)
Cr:dot; left_; capacitor(,C); dlabel(0.12,0.12,C_2,,)
Cl:dot; down_; capacitor(from Ct to Cl,C); dlabel(0.12,-0.12,,,C_1)

T:dot(at Ct+(0,elen_))
   inductor(from T to Ct); dlabel(0.12,-0.1,,,L_1)

   Point_(-30); inductor(from Cr to Cr+vec_(elen_,0))
      dlabel(0,-0.07,,L_3,)
R:dot
L:dot( at Cl-(R.x-Cr.x,Cr.y-R.y) )

   inductor(from L to Cl); dlabel(0,-0.12,,L_2,)
   right_; resistor(from L to R); rlabel(,R_2,)
   resistor(from T to R); dlabel(0,0.15,,R_3,) ; b_current(y,ljust)
   line from L to 0.2<L,T>
   source(to 0.5 between L and T); dlabel(sourcerad_+0.07,0.1,-,,+)
      dlabel(0,sourcerad_+0.07,,u,)
   resistor(to 0.8 between L and T); dlabel(0,0.15,,R_1,)
   line to T
.PE
