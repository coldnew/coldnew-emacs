% ex03.m4
.PS
cct_init
circlerad = 0.35/2
define(`elen_',`dimen_')
del = linewid*0.6

   "$u_k$" above
   line
{
   arrow right_ del
B0: circle "$b_0$"
   arrow right linewid/2
S: circle "$\sum$"
   line right 2*circlerad + del + S.w.x-B0.e.x
   {arrow ; "$y_k$" above }
   down_
   delay
   {line left_ del ; circle "$-a_1$" ; arrow from last circle to S chop}
   line dashed down_ del
   delay
   {line left_ del ; circle "$-a_n$" ; arrow from last circle to S chop}
}
   down_
   delay
   {line right_ del ; circle "$b_1$" ; arrow from last circle to S chop}
   line dashed down_ del
   delay
   {line right_ del ; circle "$b_n$" ; arrow from last circle to S chop}
.PE
