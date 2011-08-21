% clock.m4
.PS
gen_init

  linethick = 1.5
  arrowwid = 5 pt__

  rgbfill(255/255,250/255,205/255,
    Clock: circle rad 0.75 at (0,0) )
   
  command "\newcounter{hour}"
  for time=1 to 12 do {
    sprintf("\setcounter{hour}{%g}\Roman{hour}",time) \
      at Rect_(Clock.rad*0.85,90-time*30)
    }
  line <-> from Rect_(Clock.rad*0.95,90-8.5*30) to Clock \
    then to Rect_(Clock.rad*0.7,90-3.5*30)
  line thick 1 outline "red" from Clock to Rect_(Clock.rad*0.95,90-0.75*30)

.PE
