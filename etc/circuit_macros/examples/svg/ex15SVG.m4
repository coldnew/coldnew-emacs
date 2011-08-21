.PS
# ex15.m4
gen_init
[
  n = 5                                   # number of languages and machines
  boxwid = 0.2
  boxht = boxwid
  h = 0.3; w = 0.35                       # spacing height and width
  I: shadebox(box at (w*(n+1)/2,0))       # the intermediate language box
  for i = 1 to n do {
     shadebox(box fill_(0.9) with .s at (i*w,h))          # language box
     line from last box.s to I.n
     shadebox(box fill_(0.5) with .n at (i*w,-h))         # machine box
     line from last box.n to I.s
     }
  move to 2nd box.w-(1,0)
  "1 intermediate language  " at I.w rjust
  "5 languages  " at 2nd box .w rjust
  "5 machines  " at 3rd box .w rjust
  ]

[
n = 10; r = 1.4
for i = 1 to n-1 do {
   for j = i+1 to n do {
      line from rect_(r,i*twopi_/n) to rect_(r,j*twopi_/n)
      }
   }
  ] with .w at last [].e+(0.2,0)
.PE
