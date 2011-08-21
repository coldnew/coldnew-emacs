% Sierpinski.m4
.PS
`define Sierpinski' {
  if $1 > 0.04 then {
    d = $1/2-lh
    shade(1,line from $2+(d/2,d/2*s3+lw*2) right d/2 up d/2*s3 then left d*2 \
        then right d down d*s3 then right d/2 up d/2*s3 )
    Sierpinski($1/2,$2-($1/2,0))
    Sierpinski($1/2,$2+($1/2,0))
    Sierpinski($1/2,$2+(0,$1*s3/2))
    }
  }

  r = 4
  linethick = 0.2
  #linethick = 1/2 /(1pt__)
  lw = linethick pt__ /2
  s3 = sqrt(3)
  lh = lw*s3
  A: 3,3
  d = r/2-lh
  shade(0,line from A+(0,lw) right d then up d*s3 left d then down d*s3 left d\
    then right d )
  psset_(linecolor=white)
  Sierpinski(r/2,A)
.PE
