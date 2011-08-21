% rotatetext.m4
{\Huge
.PS
#		This is a test of the \rput function to write a string around the
#		periphery of a circle of radius r, accounting for inter-letter kerns
twopi = twopi_
rtod = rtod_

define(`outputstring',`University of Waterloo')
r = 0.75
divert(-1)

sinclude(rotate.dim)             # Letter and letter-pair width definitions
define(`arctot',0)               # Total arc used by the string
sinclude(rotatetext.dim)

define(`cname',`ifelse(`$1',` ',,`$1')')

define(`cprint',`ifelse(len(`$3'),0,,`define(`cc',`substr(`$3',0,1)')dnl
  dt = atan2(boxdim(cname(cc),w)/2,`$2')
  te = te + ifdef(`kern',`atan2(kern/2,`$2')',0) + dt

#		The kernel of the print mechanism.  All else is to calculate angles
  sprintf("\rput[B]{%8.3f}(0,0){\boxdims{cname(cc)}{cc}}",(arctot/2-te)*rtod) \
    with .bottom at `$1'+(rect_(`$2',arctot/2-te+twopi/4))

  define(`kern',0) ifelse(len(`$3'),1,,cc,` ',,substr(`$3',1,1),` ',,
  `define(`kern',`(boxdim(substr(`$3',0,2),w)-boxdim(
    cname(substr(`$3',0,1)),w)-boxdim(cname(substr(`$3',1,1)),w))')dnl
  \defboxdim{substr(`$3',0,2)}{substr(`$3',0,2)}')
if abs(kern/(1 pt__))>0.05 then {
  print sprintf("k`'ern(substr(`$3',0,2))=%5.2gpt",kern/(1 pt__)) }
  te = te + dt + atan2(kern/2,`$2')
  cprint(`$1',`$2',substr(`$3',1))')')
divert

print sprintf("`arctot'=%g",arctot)

C: circle rad r
te = 0
cprint(C,r,outputstring)

#		Write the total angle to rotatetext.dim
sh sprintf("echo \"`define'(`\`arctot'',%f)\" > rotatetext.dim",te)

.PE
}
