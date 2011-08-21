% snake.m4
.PS
scale = 2
rmax = 0.8
rmin = 0.2
fact = 0.75
wd = 0.15
[
shade(0.8,
  for r = rmax to rmin by *fact do {
  arc cw  rad r to Here+(r+r,0)
  arc ccw rad r-wd to Here+(2*(r-wd),0) }
  line right wd
  for r = r to rmax by *1/fact do {
  arc cw  rad r to Here-(r+r,0)
  arc ccw rad r-wd to Here-(2*(r-wd),0) }
  line to (0,0)
  )
] with .sw at 2,2
.PE
