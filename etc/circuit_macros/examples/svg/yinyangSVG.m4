.PS
# Yinyang.m4
gen_init
circlerad = 0.5
shade(0,
A: arc ccw rad circlerad*2 to Here+(4*circlerad,0)
  )
B: circle colored "white" with .c at 1/4<A.w,A.e>
  arc cw rad circlerad from B.e to B.w with .c at B.c
  circle colored "black" with .c at 3/4<A.w,A.e>
  circle rad circlerad*2 with .c at A.c
.PE
