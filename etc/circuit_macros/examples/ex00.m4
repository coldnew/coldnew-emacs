% ex00.m4
.PS

thicklines_
ewid = 2
eht = 0.5
E: ellipse wid ewid ht eht

   [  narrows = 5
      de = ewid/narrows
      for i=0 to narrows do {
         arrow down from i*de,0 }
      ] with .s at E.n+(0,0.25)

   "{\bf B$(t)$}" at last [].c

thinlines_
   arrow from E.c to E.ne
   box invis fill_(1) ht 0.12 wid 0.12 with .c at last arrow.c "$r$"

.PE
