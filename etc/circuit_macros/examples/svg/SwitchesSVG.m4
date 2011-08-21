.PS
# Switches.m4
cct_init
movewid = 0.25
moveht = moveht*1.25
  right_; {
    switch; rlabel(,`"`switch'"')
    move right_
    switch(,,D); rlabel(,`"(,,D)"')
    move right_
    switch(,,OD); rlabel(,`"(,,OD)"')
    move right_
    switch(,,C); rlabel(,`"(,,C)"')
    move right_
    switch(,,,B); rlabel(,`"(,,,B)"')
    move right_
    switch(,,C,B); rlabel(,`"(,,C,B)"')
    }
  move down; right_; {
    S: dswitch(,,); {`"dswitch=" at last line.c+(0,-0.2) "switch(,,,D)"'}
       thinlines_
       { spline <- from 0.25 along_(S) up 0.1 then up 0.05 left 0.1
         "W" wid 0.15 rjust
         spline <- from S.c+(0,m4sc*2) up 0.1 then up 0.05 left 0.1
         "B" rjust }
       thicklines_
    move right_
    S: dswitch(,,WdBK); rlabel(,`"(,,WdBK)"')
       thinlines_
       { spline <- from tr_xy(0,-2) up 0.1 then up 0.10 left 0.1
         "dB" rjust
         spline <- from tr_xy(4,-2) right 0.1 then right 0.05 up 0.15
         "K" above }
       thicklines_
    move right_
    dswitch(,,WBuD); rlabel(,`"(,,WBuD)"')
    move right_
    dswitch(,,WBF); rlabel(,`"(,,WBF)"')
    move right_
    dswitch(,,WdBKF); rlabel(,`"(,,WdBKF)"')
    move right_
    dswitch(,,WBL); rlabel(,`"(,,WBL)"')
    }
  move down; right_; {
    dswitch(,,WdBKL); rlabel(,`"(,,WdBKL)"')
    move right_
    dswitch(,,WBT); rlabel(,`"(,,WBT)"')
    move right_
    dswitch(,,WdBKC); rlabel(,`"(,,WdBKC)"')
    move right_
    dswitch(,,WBM); rlabel(,`"(,,WBM)"')
    move right_
    dswitch(,,WBCO); rlabel(,`"(,,WBCO)"')
    move right_
    dswitch(,,WBMP); rlabel(,`"(,,WBMP)"')
    }
  move down; right_; {
    dswitch(,,WBCY); rlabel(,`"(,,WBCY)"')
    move right_
    dswitch(,,WBCZ); rlabel(,`"(,,WBCZ)"')
    move right_
    dswitch(,,WBCE); rlabel(,`"(,,WBCE)"')
    move right_
    dswitch(,,WBRH); rlabel(,`"(,,WBRH)"')
    move right_
    dswitch(,,WBRdH); rlabel(,`"(,,WBRdH)"')
    move right_
    dswitch(,,WBRHH); rlabel(,`"(,,WBRHH)"')
    }
  move down; right_; {
    dswitch(,,WBMMR); rlabel(,`"(,,WBMMR)"')
    move right_
    dswitch(,,WBMM); rlabel(,`"(,,WBMM)"')
    move right_
    dswitch(,,WBMR); rlabel(,`"(,,WBMR)"')
    move right_
    dswitch(,,WBEL); rlabel(,`"(,,WBEL)"')
    move right_
    dswitch(,,WBLE); rlabel(,`"(,,WBLE)"')
    move right_
    dswitch(,,WdBKEL); rlabel(,`"(,,WdBKEL)"')
    }

.PE
