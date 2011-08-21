.PS
# graysurf.m4
ifdef(`rot3Dx',,`include(HOMELIB_`'lib3D.m4)')
gen_init
setview(20,30)
dimen = 1.5

xmax = 2*dimen
ymax = 1.5*dimen
zmax = 1*dimen

define(`fn',
`expe(-(`$1')/tt)*cos((`$2')/xmax*1.75*twopi_)*cos((`$1')/ymax*twopi_)')

nx = 24; dx = xmax/nx
ny = 30; dy = ymax/ny
tt = 1.5
fnmax = fn(0,0,0)

define(`Orig',`(project(0,0,0))')
linethick_(1.2)
arrow from Orig to 1.2 <Orig,(project(xmax,0,0))> ; "svg_it(x)" wid 0.1 below
arrow from Orig to 1.2 <Orig,(project(0,ymax,0))> ; "svg_it(y)" wid 0.1 ljust
arrow from Orig to 1.2 <Orig,(project(0,0,zmax))> ; "svg_it(z)" rjust

linethick_(0.1)
for i = 0 to nx-1 do {
  x = xmax*i/nx
  for j = 0 to ny-1 do {
    y = ymax*j/ny
    fnxy = fn(x,y)
    shade((fnmax+fnxy)/fnmax/2,
      line from (project(x,y,fnxy)) to (project(x,(y+dy),`fn(x,(y+dy))'))\
        then to (project((x+dx),(y+dy),`fn((x+dx),(y+dy))'))\
        then to (project((x+dx),y,`fn((x+dx),y)'))\
        then to (project(x,y,fnxy))
      ) } }
.PE
