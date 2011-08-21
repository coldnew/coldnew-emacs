% ex04.m4
.PS
cct_init

[
pushdef(`elen_',`dimen_')          # short elements
pushdef(`sourcerad_',`dimen_*0.2')
linewid = 0.85

Ct:dot
   Point_(-60); capacitor(,C); dlabel(0.14,0.14,,,C_3)
Cr:dot
   left_; capacitor(,C); dlabel(0.14,0.14,C_2,,)
Cl:dot
   down_; capacitor(from Ct to Cl,C); dlabel(0.14,0.14,C_1,,)

T:dot(at Ct+(0,elen_))
   inductor(from T to Ct); dlabel(0.12,-0.1,,,L_1)

   Point_(-30); inductor(from Cr to Cr+vec_(elen_,0))
      dlabel(0,-0.07,,L_3,)
R:dot
L:dot( at (Cl-(Cos(30)*(elen_),0),R) )

   inductor(from L to Cl); dlabel(0,-0.12,,L_2,)

   right_; resistor(from L to R); rlabel(,R_2,)
   move down 0.3

   resistor(from T to R); dlabel(0,0.15,,R_3,) ; b_current(y,ljust)

   line from L to 0.2<L,T>
   source(to 0.5 between L and T); dlabel(sourcerad_+0.07,0.1,-,,+)
      dlabel(0,sourcerad_+0.07,,u,)
   resistor(to 0.8 between L and T); dlabel(0,0.15,,R_1,)
   line to T
  ]
popdef(`elen_')
popdef(`sourcerad_')

[
   Point_(225)
{B1: ebox ; b_current; dlabel(dimen_*0.4,-0.1,i_1)
   dot
 B4: ebox(to rvec_(rp_len,0)); b_current(,,,E); dlabel(dimen_*0.4,-0.1,,,i_4) }
   Point_(-45)
 B2: ebox ; b_current; dlabel(dimen_*0.4, 0.1,i_2)
   dot
 B7: ebox ; b_current; dlabel(dimen_*0.4, 0.1,i_7)
 B3: ebox(from B1.end to B2.end); b_current; dlabel(dimen_*0.4,0.1,i_3)
   dot(at (B1.start,B7.end))
{B5: ebox(to B1.end); b_current; dlabel(dimen_*0.4,-0.09,i_5)}
{B6: ebox(to B2.end); b_current; dlabel(dimen_*0.4, 0.09,i_6)}
{B8: ebox(right_ to B4.end); b_current; dlabel(dimen_*0.4, 0.1,i_8)}
 B9: ebox(left_ to B7.end); b_current; dlabel(dimen_*0.4,-0.1,i_9)
 ] with .w at last [].e+(0.5,0)

.PE
