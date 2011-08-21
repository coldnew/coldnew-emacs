.PS
# ex03.m4
cct_init
circlerad = 0.35/2
define(`elen_',`dimen_')
del = linewid*0.6

   "svg_it(u`'svg_sub(k))" wid 0.15 above
   line
{
   arrow right_ del
B0: circle "svg_it(b`'svg_sub(0))"
   arrow right linewid/2
S: circle #"$\sum$"
   line right 2*circlerad + del + S.w.x-B0.e.x
   {arrow ; "svg_it(y`'svg_sub(k))" wid 0.2 above }
   down_
   delay
   {line left_ del
   circle "svg_it(-a`'svg_sub(1))" ; arrow from last circle to S chop}
   line dashed down_ del
   delay
   {line left_ del
   circle "svg_it(-a`'svg_sub(n))" ; arrow from last circle to S chop}
}
   down_
   delay
   {line right_ del
   circle "svg_it(b`'svg_sub(1))" ; arrow from last circle to S chop}
   line dashed down_ del
   delay
   {line right_ del
   circle "svg_it(b`'svg_sub(n))" ; arrow from last circle to S chop}
.PE
