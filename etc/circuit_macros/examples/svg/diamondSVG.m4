.PS
# diamond.m4
# See LaTeX Graphics Companion pp 57-58
dimen = 1.5*scale
ratio = 0.9
n = 30

North: (0,dimen)
East: (dimen,0)
South: (0,-dimen)
West: (-dimen,0)

define(`square',
 `shade(`$1',
    line from North to East then to South then to West then to North)
 ')

define(`diamond',`[
for i = 2 to n do { 
  square(`$1')
  Tmp: North
  North: ratio between East and North
  East: ratio between South and East
  South: ratio between West and South
  West: ratio between Tmp and West
  }]')

linethick = 0
diamond( i % 2 )
linethick = 0.4
diamond( (n-i)/(n-2) ) with .w at last [].e+(0.25,0)

.PE
