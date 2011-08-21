.PS
# gpar.m4
# an exercise of the `gpar_' macro
cct_init
define(`elen_',dimen_)        # shorter default element lengths
  rpoint_(right_ 0.10 down_ 1) # skew current direction: a tough test

  gpar_(
        gpar_(capacitor(,C); rlabel(,,C`'svg_sub(2)),
              resistor; llabel(,R`'svg_sub(2)),
              ),
        gpar_(resistor; rlabel(,R`'svg_sub(1)); move to rvec_(-dimen_/5,0)
                inductor(,W); rlabel(,L`'svg_sub(1)),
              reversed(`source',to rvec_(elen_,0),V,sourcerad_*2.5)
              rlabel(,v`'svg_sub(s)),
              ),
        1.75*dimen_)
  move left 0.2 from last [].w
.PE
