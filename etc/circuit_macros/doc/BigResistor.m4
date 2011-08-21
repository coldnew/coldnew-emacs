.PS
   cct_init
   linewid = 2.0
   linethick_(2.0)

R1: resistor

   thinlines_
   box dotted wid last [].wid ht last [].ht at last []

   move to 0.85<last [].sw,last [].se>
   spline <- down arrowht*2 right arrowht/2 then right 0.15; "\tt last []" ljust

   arrow <- down 0.3 from R1.start chop 0.05; "\tt R1.start" below
   arrow <- down 0.3 from R1.end chop 0.05; "\tt R1.end" below
   arrow <- down last [].c.y-last arrow.end.y from R1.c; "\tt R1.centre" below

   dimension_(from R1.start to R1.end,0.45,\tt elen\_,0.4)
   dimension_(right_ dimen_ from R1.c-(dimen_/2,0),0.3,\tt dimen\_,0.5)
.PE
