.PS
# Globe.m4
include(HOMELIB_`'lib3D.m4)
gen_init
#ommand "ifmpost(verbatimtex) \small{ ifmpost(etex)"
def_bisect                         # Bring in the equation solver

  azimuth = 15                     # Set view angles in degrees
  elevation = 35
  setview(azimuth,elevation)

  rectwid = 3.2                    # basic dimensions
  rectht = 2
  alpha = rectht/3

  command "<defs>"
  command "<linearGradient id=\"base_gray\"
   x1=\"100%\" y1=\"100%\" x2=\"0%\" y2=\"0%\">
   <stop offset=\"0%\" style=\"stop-color:rgb(255,255,255); stop-opacity:1\"/>
   <stop offset=\"100%\" style=\"stop-color:rgb(120,120,120); stop-opacity:1\"/>
   </linearGradient>"
  command "<radialGradient id=\"globe_shade\"
   cx=\"50%\" cy=\"50%\" r=\"50%\" fx=\"30%\" fy=\"30%\">
   <stop offset=\"0%\" style=\"stop-color:rgb(255,255,255); stop-opacity:1\"/>
   <stop offset=\"100%\" style=\"stop-color:rgb(120,120,120); stop-opacity:1\"/>
   </radialGradient>"
  command "</defs>"

                                   # Rectangle
  command "<g style=\"fill:url(`#'base_gray)\">"
  line from project(-rectht/2,-rectwid*0.25,0) \
         to project( rectht/2,-rectwid*0.25,0) \
    then to project( rectht/2, rectwid*0.75,0) \
    then to project(-rectht/2, rectwid*0.75,0) \
    then to project(-rectht/2,-rectwid*0.25,0)
  command "</g>"

  define(`C3D',`0,0,alpha')        # Centre of the sphere
  C: project(C3D)

  command "<g style=\"fill:url(`#'globe_shade)\">"
  circle rad alpha at C
  command "</g>"

  S: 0,0                           # The sphere bottom touch point
  "svg_it(S)" at S+(0,-2pt__) rjust
  "svg_it(a)" at 0.5<S,C> ljust

  define(`N3D',`0,0,2*alpha')      # North pole
  N: project(N3D)
  "svg_it(N)" at N+(0,3pt__) ljust

  phi = 65*dtor_
  define(`Phat3D',`rot3Dz(phi,alpha*3,0,0)')
  Phat: "svg_it(Q)" at project(Phat3D) ljust

  X: project(rectht/2*0.8,0,0)
  Y: project(0,rectwid/2*0.8,0)

`define' linevis { # ratio         # Visibility function for lines fom S to Tmp
  $2 = distance(($1 between S and Tmp),C)-alpha }

`define' invisline { # name        # Draw dashed invisible part of line in
  Tmp: $1                          # the plane
  bisect( linevis, 0, 1, 1e-8, x )
  line dashed from S to x between S and Tmp chop 0 chop 0.05 }

thinlines_                         # axes
  invisline(X)
  arrow to X chop 0.05 chop 0; "svg_it(x)" ljust
  invisline(Y)
  arrow to Y chop 0.05 chop 0; "svg_it(y)" ljust
  line dashed from S to N chop 0 chop 0.05
  arrow up alpha*0.5 chop 0.05 chop 0 ; "svg_it(z)" above
  invisline(Phat)
  line to Phat chop 0.05 chop 0
  arc ccw -> rad alpha from project(alpha/2,0,0) to \
                  project(rot3Dz(phi,alpha/2,0,0))
  "svg_it(r)" below at 0.5 between last arc.start and last arc.end

                                   # vector (ratio along (N to Phat))
define(`ray',`sum3D(N3D,sprod3D($1,diff3D(Phat3D,N3D)))')
`define' rayvis { # ratio
  $2 = length3D(diff3D(ray($1),C3D))-alpha }

  bisect( rayvis, 1e-3, 1, 1e-8, p )  # Find P
  P: "svg_it(P)" at project(ray(p)) ljust above

thicklines_
  line dashed from N to P chop 0 chop 0.05
  line to Phat chop 0.05 chop 0

define(`meridian',`rot3Dz(phi,rot3Dy(-($1),alpha,0,0))')
`define' meridianvis { # angle     # Visibility function on the meridian
  $2 = dot3D(meridian($1),View3D) }

thinlines_                         # Draw the meridian
  bisect( meridianvis, 0, pi_, 1e-8, y )
  n = 0
  for ang = y-pi_ to y by pi_/20 do {
    Q[n]: project(sum3D(C3D,meridian(ang))); n+=1 }
  fitcurve(Q,n-1)
  n = 0
  for ang = y to y+pi_ by pi_/20 do {
    Q[n]: project(sum3D(C3D,meridian(ang))); n+=1 }
  fitcurve(Q,n-1,dashed)

define(`equator',`rot3Dz($1,alpha,0,0)')
`define' equatorvis { # angle      # Visibility function on the equator
  $2 = dot3D(View3D,equator($1)) }

  bisect( equatorvis, 0, pi_, 1e-8, y )
  n = 0
  for ang = y-pi_ to y by pi_/20 do {
    Q[n]: project(sum3D(C3D,equator(ang))); n+=1 }
  fitcurve(Q,n-1)
  n = 0
  for ang = y to y+pi_ by pi_/20 do {
    Q[n]: project(sum3D(C3D,equator(ang))); n+=1 }
  fitcurve(Q,n-1,dashed)

  line dashed from C to P          # beta
  line dashed from C to project(sum3D(C3D,equator(phi)))
  arc ccw -> from 0.6 along_(last line) to 0.6 between C and P
  "svg_it(s)" ljust at last arc.e+(0,2pt__)

#ommand "ifmpost(verbatimtex) } ifmpost(etex)"
.PE
