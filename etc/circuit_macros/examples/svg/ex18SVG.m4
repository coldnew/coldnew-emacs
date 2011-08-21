.PS
# ex18.m4
cct_init
   "svg_it(v`'svg_sub(i))" wid 0.1 rjust; dot
   resistor(right_ dimen_) ; llabel(,R`'svg_sub(i))
T: dot
   line right_ linewid/4
A: opamp with .In1 at Here

   line from A.In2 to (T,A.In2)
   resistor(down_ dimen_) ; rlabel(,R`'svg_sub(g))
   ground(,T)

   line right_ dimen_ from A.Out
   diode(up_ dimen_); dot
   {line right_ linewid/2; dot; "svg_it(v`'svg_sub(o))" wid 0.2 ljust }
   resistor(left_ to (A.Out,Here)) ; rlabel(,R`'svg_sub(o)); dot
   { diode(down_ to A.Out); dot }
   line to (T,Here) then to T
.PE
