.PS
# exp.m4
# `Test of project (), setview ()'
gen_init

ifdef(`rot3Dx',,`include(HOMELIB_`'lib3D.m4)')

define(`axlen',1.2)
define(`O3',`0,0,0')
define(`X0',`axlen,0,0')
define(`Y0',`0,axlen,0')
define(`Z0',`0,0,axlen')

# diagram viewing angle
dazim = 54
delev = 15
setview(dazim,delev)

# projection azimuth, elevation
pazim = 20*dtor_
pelev = 35*dtor_
adia = 0.6

# object displacement
define(`D3',`0.3,0.6,0.9')

# fixed coordinate frame
O: project(O3)
X: project(X0)
Y: project(Y0)
 Fector(X0,Z0) with .Origin at O ; "svg_it(x`'svg_sub(0))" wid 0.2 at X rjust
 Fector(Y0,Z0) with .Origin at O ; "svg_it(y`'svg_sub(0))" at Y ljust below
 Fector(Z0,Y0) with .Origin at O ; "svg_it(z`'svg_sub(0))" at project(Z0) above

define(`R01',`rot3Dz(pazim,rot3Dy(-pelev,$1,$2,$3))')
define(`R10',`rot3Dy(pelev,rot3Dz(-pazim,$1,$2,$3))')

thinlines_ ; psset_(linecolor=gray)
  line from O to project(rot3Dz(pazim,axlen,0,0))
psset_(linecolor=black)
  arc -> ccw from project(adia,0,0) to project(rot3Dz(pazim,adia,0,0)) rad adia
    "svg_it(t)" below
  up_
  arc -> cw from project(rot3Dz(pazim,adia,0,0)) \
    to project(R01(adia,0,0)) rad adia
    "svg_it(r)" rjust at project(rot3Dz(pazim,rot3Dy(-pelev/2,adia,0,0)))
  arc -> from project(0,adia,0) to project(R01(0,adia,0)) rad adia
    "svg_it(t)" above
  right_
  arc -> cw from project(0,0,adia/2) to project(R01(0,0,adia/2)) rad adia/2
    "svg_it(r)" ljust at Here+(-1pt__,-3pt__)

 Fector(R01(X0),R01(Z0)) with .Origin at O ;
    "svg_it(x`'svg_sub(1))" above rjust at project(R01(X0))
 Fector(R01(Y0),R01(Z0)) with .Origin at O ;
    "svg_it(y`'svg_sub(1))" ljust at project(R01(Y0))
 { move right 0.2}
 Fector(R01(Z0),R01(Y0)) with .Origin at O ;
    "svg_it(z`'svg_sub(1))" above at project(R01(Z0))
thicklines_

# box object dimension
b = 0.6
d = 0.4
h = 0.20

# object corners
define(`B0',`D3')
define(`B1',`sum3D(D3,d,0,0)') PB1: project(B1)
define(`B2',`sum3D(D3,d,b,0)') PB2: project(B2)
define(`B3',`sum3D(D3,0,b,0)') PB3: project(B3)
define(`B4',`sum3D(D3,0,0,h)') PB4: project(B4)
define(`B5',`sum3D(D3,d,0,h)') PB5: project(B5)
define(`B6',`sum3D(D3,d,b,h)') PB6: project(B6)
define(`B7',`sum3D(D3,0,b,h)') PB7: project(B7)

P0:project(R01(0,dcosine3D(2,R10(B0)),dcosine3D(3,R10(B0))))
P1:project(R01(0,dcosine3D(2,R10(B1)),dcosine3D(3,R10(B1))))
P2:project(R01(0,dcosine3D(2,R10(B2)),dcosine3D(3,R10(B2))))
P3:project(R01(0,dcosine3D(2,R10(B3)),dcosine3D(3,R10(B3))))
P4:project(R01(0,dcosine3D(2,R10(B4)),dcosine3D(3,R10(B4))))
P5:project(R01(0,dcosine3D(2,R10(B5)),dcosine3D(3,R10(B5))))
P6:project(R01(0,dcosine3D(2,R10(B6)),dcosine3D(3,R10(B6))))
P7:project(R01(0,dcosine3D(2,R10(B7)),dcosine3D(3,R10(B7))))
thinlines_
  line from PB1 to P1
  line from PB2 to P2
  line from PB7 to P7
  line from PB4 to P4
thicklines_

# draw the object
  ifpstricks(
   `\pscustom[fillstyle=solid,fillcolor=white,linewidth=0]{
    line from PB4 to PB7; line to PB6; line to PB5 ; line to PB4
    \relax}',
   `gshade(1,PB4,PB7,PB6,PB5,PB4,PB7)')
  ifpstricks(
   `\pscustom[fillstyle=solid,fillcolor=gray,linewidth=0]{
    line from PB5 to PB6; line to PB2; line to PB1 ; line to PB5
    \relax}',
   `gshade(0.5,PB5,PB6,PB2,PB1,PB5,PB6)')
  ifpstricks(
   `\pscustom[fillstyle=solid,fillcolor=lightgray,linewidth=0]{
    line from PB6 to PB7; line to PB3; line to PB2 ; line to PB6
    \relax}',
   `gshade(0.85,PB6,PB7,PB3,PB2,PB6,PB7)')
  line from PB4 to PB5 then to PB1 then to PB2 then to PB3 then to PB7 \
    then to PB4
  line from PB5 to PB6 then to PB7
  line from PB6 to PB2

  arrow from O to PB1 chop linethick pt__; "svg_it(X)" rjust

  line from P4 to P5 then to P1 then to P2 then to P3 then to P7 then to P4
  line from P5 to P6 then to P7
  line from P6 to P2
  line dashed from P4 to P0 then to P3
  line dashed from P0 to P1
.PE
