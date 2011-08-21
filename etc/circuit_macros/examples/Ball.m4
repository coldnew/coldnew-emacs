% Ball.m4 stand-alone (PDF)LaTeX example
%
% Usage: type
%     m4 <path>pgf.m4 Ball.m4 | dpic -g > Ball.tex; pdflatex Ball
% or: m4 <path>pstricks.m4 Ball.m4 | dpic -p > Ball.tex; latex Ball; dvips Ball
%
\documentclass{article}
ifpgf(`\usepackage{tikz}',`\usepackage{pstricks,pst-grad}')
\pagestyle{empty}
\begin{document}
%
.PS
include(HOMELIB_`'lib3D.m4)
gen_init
command "\small{"

  viewazimuth = 15                 # Set view angles in degrees
  viewelevation = 35
  setview(viewazimuth,viewelevation)

def_bisect                         # Bring in the equation solver

  rectwid = 3.2                    # basic dimensions
  rectht = 2
  alpha = rectht/3

                                   # Rectangle
  ifpstricks(
   `command "\pscustom[fillstyle=gradient,gradmidpoint=1.0,%"
    command sprintf("gradbegin=gray,gradend=white,gradlines=%g]{",rectwid*200)')
  line from project(-rectht/2,-rectwid*1/3,0) \
         to project( rectht/2,-rectwid*1/3,0) \
    then to project( rectht/2, rectwid*2/3,0) \
    then to project(-rectht/2, rectwid*2/3,0) \
    then to project(-rectht/2,-rectwid*1/3,0)
  ifpstricks(command "}%")

  define(`C3D',`0,0,alpha')        # Centre of the sphere
  C: project(C3D)

                                   # Shaded sphere
  ifelse(m4postprocessor,pstricks,
   `Highlight: project(sum3D(C3D,rot3Dz(-15*dtor_,rot3Dy(-60*dtor_,alpha,0,0))))
    command "\pscustom[fillstyle=gradient,gradmidpoint=0.0,%"
    command sprintf("gradbegin=gray,gradend=white,gradlines=%g,%%",alpha*200)
    command "GradientCircle=true,GradientScale=1.5,%"
    command sprintf("GradientPos={(%g,%g)}]{",Highlight.x,Highlight.y)
     circle rad alpha at C
     command "}%"',
  m4postprocessor,pgf,
   `command sprintf(\#             A little too dark, maybe
      "\dpicdraw[ball color=white](%g,%g) circle (%gin)\dpicstop",\
       C.x,C.y,alpha/2.54)',
   `circle rad alpha at C fill_(1) ')

  S: "$S$" at project(0,0,0) rjust # The sphere bottom touch point
  "$\alpha$" at 0.5<S,C> rjust

  define(`N3D',`0,0,2*alpha')      # North pole
  N: "N" at project(N3D) ljust above

  phi = 65*dtor_
  define(`Phat3D',`rot3Dz(phi,alpha*2.7,0,0)')
  Phat: "$\hat{P}$" at project(Phat3D) ljust

  X: project(rectht/2*0.8,0,0)
  Y: project(0,rectwid/2*0.8,0)

`define' linevis { # ratio         # Visibility function for lines fom S to Xb
  $2 = distance(($1 between S and Xb),C)-alpha }

`define' invisline { # name        # Draw dashed invisible part of line in
  Xb: $1                           # the plane
  bisect( linevis, 0, 1, 1e-8, x )
  line dashed from S to x between S and Xb chop 0 chop 0.05 }

thinlines_                         # axes
  invisline(X)
  arrow to X chop 0.05 chop 0; "$x,\:\xi$" at Here+(0,3pt__) below
  invisline(Y)
  arrow to Y chop 0.05 chop 0; "$y,\:\eta$" ljust
  line dashed from S to N chop 0 chop 0.05
  arrow up alpha*0.5 chop 0.05 chop 0 ; "$z,\:\zeta$" above ljust
  invisline(Phat)
  line to Phat chop 0.05 chop 0
  arc ccw -> rad alpha from project(alpha/2,0,0) to \
                  project(rot3Dz(phi,alpha/2,0,0))
  "$\phi$" below at 0.5 between last arc.start and last arc.end

                                   # vector (ratio along (N to Phat))
define(`ray',`sum3D(N3D,sprod3D($1,diff3D(Phat3D,N3D)))')
`define' rayvis { # ratio
  $2 = length3D(diff3D(ray($1),C3D))-alpha }

  bisect( rayvis, 1e-3, 1, 1e-8, p )  # Find P
  P: "$P$" at project(ray(p)) ljust above

thicklines_
  line dashed from N to P chop 0 chop 0.05
  line to Phat chop 0.05 chop 0

define(`meridian',`rot3Dz(phi,rot3Dy(-($1),alpha,0,0))')
`define' meridianvis { # angle       # Visibility function on the meridian
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
  "$\beta$" above

command "}"
.PE
%
\end{document}
