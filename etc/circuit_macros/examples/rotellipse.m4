divert(-1)
                                `ellipse oriented in current direction;
                                 type= eg dotted fill_(0.9)'
define(`rotellipse',
 `[define(`m4ehw',(ifelse(`$1',,ellipsewid,`($1)')/2))dnl
  define(`m4ehh',(ifelse(`$2',,ellipseht,`($2)')/2))dnl
  spline ifdpic(
   `0.551784 `$3' from vec_(0,-m4ehh) to vec_(m4ehw,-m4ehh) \
      then to vec_(m4ehw,m4ehh) then to vec_(-m4ehw,m4ehh) \
      then to vec_(-m4ehw,-m4ehh) then to vec_(0,-m4ehh)',
   ``$3' from vec_(0,-m4ehh) to vec_(m4ehw/64,-m4ehh) \
      for_(1,31,1,
       `then to vec_(m4ehw*sin(twopi_*m4x/32),-m4ehh*cos(twopi_*m4x/32))\')\
      then to vec_(-m4ehw/64,-m4ehh) then to vec_(0,-m4ehh)')
    ] wid 2*sqrt((m4a_*m4ehw)^2+(m4b_*m4ehh)^2) \
      ht  2*sqrt((m4c_*m4ehw)^2+(m4d_*m4ehh)^2)')

divert(0)dnl
% rotellipse.m4
.PS
gen_init

ifpostscript(`define(`red',1 0 0)define(`blue',0 0 1)define(`green',0 1 0)')

[
E: ellipse shaded "red" outlined "blue"

A: rotellipse(1,1,outlined "red" shaded "blue") with .w at E.e
  Point_(45)
B: rotellipse(,,dotted fill_(0.9))
  Point_(-60)
C: rotellipse(,,colored "red") with .s at last [].n
thinlines_
showbox_(A)
showbox_(B)
showbox_(C)
thicklines_
]

[
for_(0,9,1,
 `Point_(m4x/9*360)
  B: lbox(boxwid/2,boxht/2,colored "red")
  thinlines_
  showbox_(B)
  thicklines_ ')

arc fill
arc to Here-(2*arcrad,0) outlined "red" shaded "green"
] with .nw at last [].sw + (0,-0.5)

.PE
