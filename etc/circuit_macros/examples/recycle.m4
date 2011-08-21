% recycle.m4
.PS
gen_init
#                                   `recycle(width)'
define(`point',` line from P1+vec_(0,-stripwd/2) to P1+vec_(0,-stripwd*3/4) \
    then to rvec_(space,0) then to P1+vec_(0,stripwd*3/4) \
    then to P1+vec_(0,stripwd/2) then to P3+vec_(0,-stripwd/2)
  arc ccw to P3+vec_(Rect_(stripwd/2,-30)) with .c at P3
  line to P1+vec_(Rect_(stripwd/2,-30))
  arc cw to P1+vec_(0,-stripwd/2) with .c at P1')

define(`tail',`
  Point_(angle+120)
  line from P2 to P2+vec_(stripwd,0) then to P2+vec_(stripwd,-stripwd) \
    then to P5
  Point_(angle)
  arc cw to P3+vec_(0,-stripwd/2) with .c at P3 
  line to P2')

define(`recycle',`[ space = 2*linethick pt__
  stripwd = ($1-space)/3.765544
  ltthick = linethick
  for angle = 0 to 240 by 120 do {
    Point_(angle)

    P1: rvec_(stripwd*3/4+space,0)
    P2: P1+vec_(stripwd/2/sqrt(3),stripwd/2)
    P3: 2<P1,P2>
    Point_(angle+120)
    P4: P2+vec_(stripwd,-stripwd/2)
    P5: P3+vec_(0,-stripwd/2)
    Point_(angle)

#                                   `PSTricks has a convenient gradient fill'
ifpstricks(
 `{ linethick = 0
    command sprintf("\psset{gradbegin=white,gradend=gray,gradlines=%g}",\
      int(stripwd*2*300/16)*16)
    command sprintf(\
      "\pscustom[fillstyle=gradient,gradmidpoint=0.999,gradangle=%g]{",angle+90)
    point
    command "}%"
    command sprintf("\psset{gradbegin=white,gradend=gray,gradlines=%g}",\
      int(stripwd*2*300/16)*16)
    command sprintf(\
      "\pscustom[fillstyle=gradient,gradmidpoint=0.999,gradangle=%g]{",angle+30)
    tail
    command "}%"
    linethick_(ltthick) }',
 `{ shade(0.95, tail)   }')
  
    point
    tail
    move to P4
    } ]')

recycle(2)
# print last [].wid,last [].ht
.PE
