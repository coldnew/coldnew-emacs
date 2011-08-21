% ex02.m4
.PS
cct_init

[
  [ for angle = 0 to 315 by 45 do { Point_(angle)
     opamp(to rvec_(linewid*3/2,0)) with .Out at 0,0 } ]
  resistor(down_ dimen_ from last [].s)
  diode(down_ dimen_*2/3,LE) 
  diode(down_ dimen_*2/3,LER) 
  ground(,T)
]

[
 define(`elen_',`dimen_*3/2')          # restore defaults
 define(`sourcerad_',`dimen_*0.25')
 leng = elen_
 define(`fromcentre',`move to C+(-elen_/2,-elen_*cosd(15)/(2*sind(15)))')
 C: dot
  { fromcentre()
     for i = 0 to 359 by 30 do {
        Point_(i); inductor 
        }
     }
  define(`elen_',leng*0.8)
  { fromcentre()
     for i=0 to 359 by 30 do {
        Point_(i); inductor(,W) 
        }
     }
  define(`elen_',leng*0.6)
  { fromcentre()
     for i=0 to 359 by 30 do {
        Point_(i); resistor 
        }
     }
  define(`elen_',leng*0.4)
  { fromcentre()
     for i=0 to 359 by 30 do {
        Point_(i); capacitor(,C)
        }
     }
  define(`elen_',leng*0.2)
  { fromcentre()
     for i=0 to 359 by 30 do {
        Point_(i); diode 
        }
     }
  ] with .sw at last [].se+(linewid,0)

.PE
