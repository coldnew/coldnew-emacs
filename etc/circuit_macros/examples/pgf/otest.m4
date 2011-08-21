.PS
# Fills.m4
gen_init

define(`tline',`[
$1(0,1,0,B:box)$2
ifelse(`$3',,`{ `"$1(0,1,0,box)$2"' at B.s below }')
move
$1(0,1,0,E:ellipse)$2
ifelse(`$3',,`{ `"$1(0,1,0,E:ellipse)$2"' at E.s+(0,-0.15) below }')
move
$1(0,1,0,C:circle)$2
ifelse(`$3',,`{ `"$1(0,1,0,C:circle)$2"' at C.s below }')
move down circlerad then right
$1(0,1,0,K:arc)$2
ifelse(`$3',,`{ `"$1(0,1,0,K:arc)$2"' at K.s+(0,-0.15) below }')
move down arcrad then right
$1(0,1,0,L:line right then up left)$2
ifelse(`$3',,`{ `"$1(0,1,0,L:line)$2"' at L.s+(linewid/2,0) below }')
 move right from last line.e
#$1(0,1,0,A:arrow right then up left)$2
#ifelse(`$3',,`{ `"$1(0,1,0,A:arrow)$2"' at A.s+(0,-0.15) below }')
#move right from last arrow.e
$1(0,1,0,S:spline right then up then left)$2
ifelse(`$3',,`{ `"$1(0,1,0,S:spline)$2"' at S.s below }')
]')
A: tline(`rgbfill')
#B: tline(`rgbfill',,.) with .nw at A.sw+(0,-0.5)
#C: tline(`rgbfill(1,0,0,rgbdraw',`)',.) with .nw at B.sw+(0,-0.5)
#setrgb(0,0,1)
#D: tline(`rgbfill(1,0,0,rgbdraw',`)',.) with .nw at C.sw+(0,-0.5)
.PE
