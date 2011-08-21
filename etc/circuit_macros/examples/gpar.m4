% gpar.m4
% an exercise of the `gpar_' macro
.PS
cct_init
define(`elen_',dimen_)        # shorter default element lengths
  rpoint_(right_ 0.10 down_ 1) # skew current direction: a tough test

  gpar_(
        gpar_(capacitor(,C); rlabel(,,C_2),
              resistor; llabel(,R_2),
              ),
        gpar_(resistor; rlabel(,R_1); move to rvec_(-dimen_/5,0)
                inductor(,W); rlabel(,L_1),
              reversed(`source',to rvec_(elen_,0),V,sourcerad_*2.5)
              rlabel(,v_s),
              ),
        1.75*dimen_)
.PE
