% `Corners.m4'
.PS
cct_init
s_init(Corners)
sinclude(CMman.dim)

linethick = 4
[ line up 0.2
      line from Here+(0.2,0) left 0.2 ]
s_box("``\tt\shortstack[l]{line up 0.2\\ line right 0.2}''") \
   with .n at last [].s +(0,-0.1)
[ line up 0.2 then right 0.2 ] with .w at last [].e+(1.5,0)
s_box("``\tt line up 0.2 then right 0.2''") \
   with .n at last [].s +(0,-0.1)
[ line up 0.2
      round
      line right 0.2 ] with .w at last [].e+(1.5,0)
s_box("``\tt\shortstack[l]{line up 0.2\\ round\\ line right 0.2}''") \
   with .n at last [].s +(0,-0.1)
[ line up 0.2
      corner
      line right 0.2 ] with .w at last [].e+(1.25,0)
s_box("``\tt\shortstack[l]{line up 0.2\\ corner\\ line right 0.2}''") \
   with .n at last [].s +(0,-0.1)

.PE
