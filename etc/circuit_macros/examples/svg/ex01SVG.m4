.PS
# ex01.m4
cct_init
linewid = linewid*1.5     # let's make the circuit elements bigger than usual
define(`elen_',linewid)   # but not longer

  del = elen_/4           # a spacing parameter
  move right 0.4
Vin: source(up_ elen_ + 2*del, V); llabel(,v`'svg_sub(s),)

  switch(right_ elen_,,C); rlabel(,t = svg_norm(0),)
  resistor; llabel(,svg_norm(47 ohm),); b_current(i`'svg_sub(R),above rjust)

  line right_ elen_/2 then down_ del
  gpar_(inductor(,W); llabel(,"L" wid 0.15,); b_current(i`'svg_sub(L)),
        capacitor(,C); rlabel(+,v`'svg_sub(C),-); llabel(,,C),
        elen_)
  line to (Here,Vin.start) then to Vin.start

  define(`meshcurrent',`[arc -> cw with .c at (0,0) \
    from (Rect_(`$1',-30)) to (Rect_(`$1',30)) "svg_it(`$2')"]')

  right_
  meshcurrent(del,i`'svg_sub(2)) at last[].C
  meshcurrent(2*del,i`'svg_sub(1)) at Vin.start + (4*del,3*del)

.PE
