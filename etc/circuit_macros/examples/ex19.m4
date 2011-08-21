% ex19.m4
.PS
cct_init
define(`dimen_',dimen_*1.25)

B: battery(up_ dimen_*2/3); rlabel(,V^o)
   move down dimen_/4
   resistor(up_ dimen_); rlabel(,R,)
   inductor(right_ elen_); b_current(i); rlabel(,L)
   { capacitor(down_ to (Here,B.start)); rlabel(,C); llabel(+,v,-) }
   line right_ dimen_
   diode(down_ to (Here,B.start),T); b_current(h(v))
   line to B.start
.PE
