% Buttons.m4 (requires PSTricks)
.PS
ifpstricks(`
`define cmyk2rgb {
  $5 = (1-($4)/100)*(1-($1)/100)
  $6 = (1-($4)/100)*(1-($2)/100)
  $7 = (1-($4)/100)*(1-($3)/100) }'

define(`cmykrgb',
`(1-($4)/100)*(1-($1)/100),(1-($4)/100)*(1-($2)/100),(1-($4)/100)*(1-($3)/100)')

`define rgb2cmyk {
  $7 = min(1-$1,min(1-$2,1-$3))
  $4 = (1-$7-$1)/(1-$7)
  $5 = (1-$7-$2)/(1-$7)
  $6 = (1-$7-$3)/(1-$7) }'

\font\Buttonfont=phvb at 0.5in
linedensity = 150/scale

[
cdiam = 2
cmyk2rgb(0,0,100,0,r0,g0,b0)
cmyk2rgb(0,0,100,0,r1,g1,b1)
cmyk2rgb(0,5,100,10,r2,g2,b2)
cmyk2rgb(40,50,100,10,r3,g3,b3)

command sprintf("\psset{slopesteps=%g}",cdiam/2*linedensity)
command sprintf(\
"\psset{slopecolors=0 %g %g %g",r0,g0,b0) \
  + sprintf(" 20 %g %g %g",r1,g1,b1) \
  + sprintf(" 80 %g %g %g",r2,g2,b2) \
  + sprintf(" 100 %g %g %g",r3,g3,b3) \
  + " 4}"
\pscustom[linestyle=none,fillstyle=ccslopes]{%
C: circle path diam cdiam
\relax}

command sprintf("\psset{slopesteps=%g}",cdiam*3/4*linedensity)
command sprintf(\
"\psset{slopecolors=0 %g %g %g",1,1,1) \
  + sprintf(" 10 %g %g %g",1,1,1) \
  + sprintf(" 33 %g %g %g",(r1+r2)/2,(g1+g2)/2,(b1+b2)/2) \
  + sprintf(" 67 %g %g %g",r1,g1,b1) \
  + sprintf(" 100 %g %g %g",(r1+r2)/2,(g1+g2)/2,(b1+b2)/2) \
  + " 5}"
\pscustom[linestyle=none,slopeangle=-90,fillstyle=slopes]{
  circle diameter cdiam*3/4 with .n at last circle.n+(0,-0.01*cdiam)
\relax}

"\Buttonfont Button" "\Buttonfont 1" at C

]

[
cmyk2rgb(100,20,0,80,r0,g0,b0)
cmyk2rgb(100,20,0,20,r2,g2,b2)
cmyk2rgb(10,0,0,0,r3,g3,b3)
command sprintf("\definecolor{outer}{rgb}{%g,%g,%g}",r2,g2,b2)

buttonwd = 4.5
buttonht = 1

steps=10
wth = linethick pt__
for i=1 to steps-1 do {
  command sprintf("\definecolor{outer}{rgb}{%g,%g,%g}",\
    cmykrgb(100*i/steps,20*i/steps,0,20*i/steps))
\pscustom[linestyle=solid,linecolor=outer]{
B: box ht buttonht wid buttonwd rad buttonht/2 at (0,0)+(0,-wth*(steps-i))
\relax}
}

command sprintf("\psset{slopesteps=%g}",buttonht*linedensity)
command sprintf(\
"\psset{slopecolors=0 %g %g %g",r0,g0,b0) \
  + sprintf(" 40 %g %g %g",r2,g2,b2) \
  + sprintf(" 100 %g %g %g",r3,g3,b3) \
  + " 3}"
\pscustom[linestyle=solid,linecolor=outer,slopeangle=-90,fillstyle=slopes]{
B: box ht buttonht wid buttonwd rad buttonht/2 at 0,0
\relax}

smbf = 0.4
a = 0.8
b = 0.2
define(`intrp',`(a*(`$1')+b*(`$2'))/(a+b)')

command sprintf("\psset{slopesteps=%g}",buttonht*smbf*linedensity)
command "\psset{slopecolors=0 1 1 1 8 1 1 1"\
  + sprintf(" 100 %g %g %g 3}",intrp(r2,r3),intrp(g2,g3),intrp(b2,b3))
\pscustom[linestyle=none,slopeangle=-90,fillstyle=slopes]{
  box ht buttonht*smbf wid buttonwd*4/4.5 rad buttonht/2*smbf \
    with .n at last box.n+(0,-0.05*buttonht)
\relax}

"\Buttonfont Button 2" at B

] with .nw at last [].sw+(0,-0.5)

',
`box "PSTricks" "required"')
.PE
