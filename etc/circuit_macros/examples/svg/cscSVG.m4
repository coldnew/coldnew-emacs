.PS 3.5
# csc.m4
gen_init

circlerad=1.91*scale
hubrad=0.10
s=0.5

define(`midnight',`26,26,112')
define(`white',`255,255,255')
define(`spokethick',0.1)
define(`spoke',`dnl
  {line from rvec_(0,hubrad-spokethick/2) to rvec_(circlerad-0.05,0)}
  {line from rvec_(0,-(hubrad-spokethick/2)) to rvec_(circlerad-0.05,0)}')
define(`lwid',1)
define(`coord',`(s*(`$1'),s*(`$2'))')

#                               Circle and spokes
# [
rgbdraw(midnight,
  linethick=`0.'eval(lwid*15)/(1pt__)
  {circle with .c at Here
   circle invis diam last circle.diam + linethick pt__ at last circle }
  linethick=spokethick/(1pt__)
  for angle = 0 to 330 by 30 do {
    Point_(angle)
    {spoke}
    }
  )

#                               Whiten where the hull will be
  linethick=`0.'eval(lwid*20)/(1pt__)
  rgbdraw(white,
    Point_(-60){line to rvec_(circlerad*0.72,0)} 
    Point_(-90){line to rvec_(circlerad*0.72,0)} 
    Point_(-120){line to rvec_(circlerad*0.72,0)} 
  
    linethick=`0.'eval(lwid*15)/(1pt__)
    {move to coord(1.8,-2.3)+(0,0.025)
    spline to Here+coord(-0.8,-0.3) then to Here+coord(-1.8,-0.3) \
      then to Here+coord(-2.18,-0.27) \
      then to Here+coord(-3.93,-0.1)}
    )

#                               Sail
  thinlines_
  setrgb(midnight)
    line from coord(0.44,3.38) to coord(0.44,2.6) \
      then to coord(2.1,-1.75) \
      then to coord(1.22,-1.53) \
      then to coord(0.62,-1.4) \
      then to coord(0.55,-1.6) \
      then to coord(-2.2,-1.6) \
      then to coord(-2.35,-1.5)
    spline to coord(-2.14,-0.78) \
      then to coord(-1.74,0.22) \
      then to coord(-1.42,0.89) \
      then to coord(-0.92,1.73) \
      then to coord(0,2.9) \
      then to coord(0.44,3.38)

#                               Hull
     spline from coord(2.1,-1.75) to coord(1.9,-2.2) \
       then to coord(1.85,-2.3) then to coord(1.8,-2.33) \
       then to coord(1,-2.5) then to coord(0,-2.6) then to coord(-0.38,-2.57) \
       then to coord(-2.13,-2.4)
     line to coord(-2.2,-2.05) then to coord(2.1,-1.75)
#    spline to coord(-1,-2.08) then to coord(0.55,-1.77)
     line to coord(2.1,-1.75)
resetrgb
  setrgb(white)
     spline from coord(-2.2,-2.05) to coord(-1,-2.08) then to coord(0.55,-1.77)
     line to coord(-2.2,-2.05)
resetrgb
  linethick=`0.'eval(lwid*15)/(1pt__)

# ] with .sw at 1,1

.PE
