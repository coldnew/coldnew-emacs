% chaos.m4
% Example simulation of chaos equations.
.PS
scale = 10
define(`FF',`dnl
  `$4' = `$1'+dt*10*(-`$1'+`$2')
  `$5' = `$2'+dt*(28*`$1'-`$2'-`$1'*`$3')
  `$6' = `$3'+dt*(-8*`$3'/3+`$1'*`$2') ')
[
x0 = 1
y0 = 0
z0 = 0
dt = 0.005
nN = 100
for i = 1 to nN do {
  for j = 1 to 8 do {
    FF(x0,y0,z0,x1,y1,z1)
    FF(x1,y1,z1,x2,y2,z2)
    FF(x2,y2,z2,x3,y3,z3)
    FF(x3,y3,z3,x4,y4,z4)
    spline from x0,y0 to x1,y1 then to x2,y2 then to x3,y3 then to x4,y4
    x0 = x4; y0 = y4; z0 = z4
    }
  }
arrow from (-15,0) to (15,0); "$x$" ljust
line up 1 from (10,0); "$ 10$" at last line.start below
line up 1 from (-10,0); "$-10$" at last line.start below
arrow from (0,-15) to (0,15); "$y$" above
line right 1 from (0,10); "$ 10$" at last line.start rjust
line right 1 from (0,-10); "$-10$" at last line.start rjust
"$ 0$" at (0,0) below rjust
]
`"\parbox{2.5in}{\begin{eqnarray*}
 \dot{x}_1 & = & 10(-x_1+x_2)\\
 \dot{x}_2 & = & 28x_1-x_2-x_1x_3\\
 \dot{x}_3 & = & -8x_3/3+x_1x_2
 .\end{eqnarray*}}"' at last [].s below 
.PE
