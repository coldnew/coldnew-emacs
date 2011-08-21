divert(-1)
lib3D.m4                        Macros for rotation, projection, and other
                                operations on argument triples representing
                                3D vectors.

* Circuit_macros Version 7.0, copyright (c) 2011 J. D. Aplevich, under    *
* the LaTeX Project Public License. The files of this distribution may    *
* be redistributed or modified, provided that this copyright notice is    *
* included and provided that modifications are clearly marked to          *
* distinguish them from this distribution.  There is no warranty          *
* whatsoever for these files.                                             *

                                Installation directory.  You can set this to
                                the null string if you use an environment
                                variable to tell m4 where to search:
ifdef(`HOMELIB_',,
`define(`HOMELIB_',`/home/coldnew/.kde4/share/apps/cirkuit/circuit_macros/')')

                                Default pic processor: gpic.  To make dpic -p
                                the default, change gpic to pstricks in the
                                line below. To make dpic -g and TikZ-PGF the
                                default, change gpic to pgf:
define(`m4defaultprocessor',pstricks)

ifdef(`m4picprocessor',,`include(HOMELIB_`'m4defaultprocessor.m4)divert(-1)')

=============================================================================

                               `setview (azimuth, elevation)
                                Set view angles (degrees) for projection onto
                                a plane, where the projection matrix P is
                                P =(       -sin(az),        cos(az),   0   )
                                   (-sin(el)cos(az),-sin(az)sin(el),cos(el))'
define(`setview',`dnl
m4azim=prod_($1,dtor_); m4elev=prod_($2,dtor_)
m4caz=cos(m4azim); m4saz=sin(m4azim); m4cel=cos(m4elev); m4sel=sin(m4elev)')

                                The resulting view vector
define(`View3D',`(m4caz*m4cel),(m4saz*m4cel),m4sel')
                                Transformation projection coords to base coords
define(`PtoBase3D',`rot3Dz(m4azim,rot3Dy(-m4elev,`$1',`$2',`$3'))')

                                This does the 3D to 2D projection
                                i.e. project(x,y,z) produces u,v that
                                are the coordinates on the 2D plane defined
                                by the view angles.
define(`project',`diff_(`prod_(m4caz,$2)',`prod_(m4saz,$1)'),dnl
diff_(`prod_(m4cel,$3)',`sum_(`prod_(m4sel*m4caz,$1)',dnl
`prod_(m4sel*m4saz,$2)')')')

                               `Rotation about x axis rot3Dx(angle,x1,x2,x3)'
define(`rot3Dx',``$2',diff_(prod_(cos(`$1'),`$3'),prod_(sin(`$1'),`$4')),dnl
        sum_(prod_(sin(`$1'),`$3'),prod_(cos(`$1'),`$4'))')

                               `Rotation about y axis rot3Dy(angle,x1,x2,x3)'
define(`rot3Dy',`sum_(prod_(cos(`$1'),`$2'),prod_(sin(`$1'),`$4')),`$3',dnl
  diff_(prod_(cos(`$1'),`$4'),prod_(sin(`$1'),`$2'))')

                               `Rotation about z axis rot3Dz(angle,x1,x2,x3)'
define(`rot3Dz',`diff_(prod_(cos(`$1'),`$2'),prod_(sin(`$1'),`$3')),dnl
  sum_(prod_(sin(`$1'),`$2'),prod_(cos(`$1'),`$3')),`$4'')

                               `Cross product cross3D(x1,y1,z1,x2,y2,z2)'
define(`cross3D',`diff_(prod_(`$2',`$6'),prod_(`$3',`$5')),dnl
  diff_(prod_(`$3',`$4'),prod_(`$1',`$6')),dnl
  diff_(prod_(`$1',`$5'),prod_(`$2',`$4'))')
  
                               `Dot product dot3D(x1,y1,z1,x2,y2,z2)'
define(`dot3D',`(sum_(
  sum_(prod_(`$1',`$4'),prod_(`$2',`$5')),prod_(`$3',`$6')))')
                                Vector addition, subtraction, scalar product
define(`sum3D',`sum_(`$1',`$4'),sum_(`$2',`$5'),sum_(`$3',`$6')')
define(`diff3D',`diff_(`$1',`$4'),diff_(`$2',`$5'),diff_(`$3',`$6')')
define(`sprod3D',`prod_(`$1',`$2'),prod_(`$1',`$3'),prod_(`$1',`$4')')

                                Extract direction cosine
define(`dcosine3D',`(ifelse(`$1',1,`$2',`$1',2,`$3',`$4'))')
                                Euclidian length
define(`length3D',`sqrt((`$1')^2+(`$2')^2+(`$3')^2)')
                                Unit vector
define(`unit3D',`sprod3D(1/length3D(`$1',`$2',`$3'),`$1',`$2',`$3')')
                                Write out the 3 arguments for debug
define(`print3D',`print sprintf("`$1'(%g,%g,%g)",`$2',`$3',`$4')')

                                `Fector(x,y,z,nx,ny,nz) with .Origin at pos
                                Arrow with flat 3D head.  The second vector,
                                (i.e. args nx,ny,nz) is the normal to the
                                head flat surface'
define(`Fector',
 `[ Origin: 0,0
  define(`M4F_V',``$1',`$2',`$3'')dnl          the whole vector V
  lV = length3D(M4F_V)
  define(`M4F_T',``$4',`$5',`$6'')dnl          normal to the top surface
  lT = length3D(M4F_T)
  define(`M4F_Vn',`sprod3D(1/lV,M4F_V)')dnl  unit vector Vn
  aln = 0.15*scale ;dnl             arrowhead length
  awd = 0.09*scale ;dnl                 "     width
  adp = 0.0375*scale ;dnl               "     depth (thickness)
  define(`M4F_Vt',`sprod3D((lV-aln),M4F_Vn)')dnl head base vector

Start: Origin
End: project(M4F_V)
  rpoint_(from Origin to End)
  lTdp = adp/2/lT
  vtx = dcosine3D(1,M4F_Vt); vty = dcosine3D(2,M4F_Vt) # Vt coords
  vtz = dcosine3D(3,M4F_Vt)
dnl                             half-thickness vector in direction of T
  tx = prod_(lTdp,`$4'); ty = prod_(lTdp,`$5')
  tz = prod_(lTdp,`$6')
dnl                             half-width vector right
  rf = awd/2/lT/lV
  rx = rf*dcosine3D(1,cross3D(M4F_V,M4F_T))
  ry = rf*dcosine3D(2,cross3D(M4F_V,M4F_T))
  rz = rf*dcosine3D(3,cross3D(M4F_V,M4F_T))
dnl                             top and bottom points of V
TV: project(sum3D(M4F_V, tx,ty,tz))
BV: project(diff3D(M4F_V, tx,ty,tz))
dnl                             top, bottom right, left of base
TR: project(sum3D(vtx,vty,vtz, sum3D(tx,ty,tz,rx,ry,rz)))
BR: project(sum3D(vtx,vty,vtz, diff3D(rx,ry,rz,tx,ty,tz)))
BL: project(diff3D(vtx,vty,vtz, sum3D(rx,ry,rz,tx,ty,tz)))
TL: project(diff3D(vtx,vty,vtz, diff3D(rx,ry,rz,tx,ty,tz)))
  lthickness = linethick
dnl                             base
  if dot3D(M4F_V,View3D) < 0 then {
    thinlines_
    ifpstricks(
     `\pscustom[linewidth=0pt,fillstyle=solid,fillcolor=gray]{
      line from BR to BL then to TL then to TR then to BR
      \relax}',
     `gshade(0.5,BR,BL,TL,TR,BR,BL)')
    line from BR to BL ; line to TL ; line to TR ; line to BR
    linethick_(lthickness)
    }
dnl                             shaft
  linethick_(1.2)
  psset_(arrows=c-c)
  line from Origin to project(vtx,vty,vtz)
  psset_(arrows=-)
  thinlines_
dnl                             top or bottom
  if dot3D(M4F_T,View3D) > 0 then {
    ifpstricks(
     `\pscustom[linewidth=0pt,fillstyle=solid,fillcolor=white]{
      line from TV to TR then to TL then to TV
      \relax}',
     `gshade(1,TR,TL,TV,TR,TL)')
    line from TV to TR ; line to TL ; line to TV
  } else {
    ifpstricks(
     `\pscustom[linewidth=0pt,fillstyle=solid,fillcolor=black]{
      line from BV to BR then to BL then to BV
      \relax}',
     `gshade(0,BR,BL,BV,BR,BL)')
    line from BV to BR ; line to BL ; line to BV
    }
dnl                             starboard normal; draw right face
define(`M4F_S',
  `cross3D(diff3D(sprod3D(aln,M4F_Vn),rx,ry,rz),M4F_T)')dnl
  if dot3D(M4F_S,View3D) > 0 then {
    ifpstricks(
     `\pscustom[linewidth=0pt,fillstyle=solid,fillcolor=white]{
      line from TV to BV then to BR then to TR then to TV
      \relax}',
     `gshade(1,TV,BV,BR,TR,TV,BV)')
    line from TV to BV ; line to BR ; line to TR ; line to TV
    }
dnl                             port normal; draw left face
define(`M4F_P',
  `cross3D(M4F_T,sum3D(sprod3D(aln,M4F_Vn),rx,ry,rz))')dnl
  if dot3D(M4F_P,View3D) > 0 then {
    ifpstricks(
     `\pscustom[linewidth=0pt,fillstyle=solid,fillcolor=white]{
      line from TV to BV then to BL then to TL then to TV
      \relax}',
     `gshade(1,TV,BV,BL,TL,TV,BV)')
    line from TV to BV ; line to BL ; line to TL ; line to TV
    }
  linethick_(lthickness)
  `$7'] ')
divert(0)dnl
