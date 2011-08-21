% ex01.m4
.PS
cct_init
linewid = linewid*1.5     # let's make the circuit elements bigger than usual
define(`elen_',linewid)   # but not longer

  del = elen_/4           # a spacing parameter
Vin: source(up_ elen_ + 2*del, V); llabel(,v_s,)

  switch(right_ elen_,,C); rlabel(,t = 0,)
  resistor; llabel(,47\`,'\Omega,); b_current(i_R\strut,above rjust)

  line right_ elen_/2 then down_ del
  gpar_(inductor(,W); llabel(,L,); b_current(i_L),
        capacitor(,C); rlabel(+,v_C,-); llabel(,,\;C),
        elen_)
  line to (Here,Vin.start) then to Vin.start

  define(`meshcurrent',`[arc -> cw with .c at (0,0) \
    from (Rect_(`$1',-30)) to (Rect_(`$1',30)) "$`$2'$"]')

  right_
  meshcurrent(del,i_2) at last[] + (0.05,0)
  meshcurrent(2*del,i_1) at Vin.start + (4*del,3*del)

.PE
