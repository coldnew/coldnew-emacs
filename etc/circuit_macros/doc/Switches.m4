% Switches.m4
.PS
cct_init
movewid = 0.25
moveht = moveht*1.25
  right_; {
    switch; rlabel(,`\hbox{\tt `switch'}')
    move right_
    switch(,,D); rlabel(,`\hbox{\tt (,,D)}')
    move right_
    switch(,,OD); rlabel(,`\hbox{\tt (,,OD)}')
    move right_
    switch(,,C); rlabel(,`\hbox{\tt (,,C)}')
    move right_
    switch(,,,B); rlabel(,`\hbox{\tt (,,,B)}')
    move right_
    switch(,,C,B); rlabel(,`\hbox{\tt (,,C,B)}')
    }
  move down; right_; {
    S: dswitch(,,);rlabel(,`\hbox{\tt \shortstack{`dswitch'=\\`switch'(,,,D)}}')
       thinlines_
       { spline <- from 0.25 along_(S) up 0.1 then up 0.05 left 0.1
         "\tt W" rjust
         spline <- from S.c+(0,m4sc*2) up 0.1 then up 0.05 left 0.1
         "\tt B" rjust }
       thicklines_
    move right_
    S: dswitch(,,WdBK); rlabel(,`\hbox{\tt (,,WdBK)}')
       thinlines_
       { spline <- from tr_xy(0,-2) up 0.1 then up 0.10 left 0.1
         "\tt dB" rjust
         spline <- from tr_xy(4,-2) right 0.1 then right 0.05 up 0.15
         "\tt K" above }
       thicklines_
    move right_
    dswitch(,,WBuD); rlabel(,`\hbox{\tt (,,WBuD)}')
    move right_
    dswitch(,,WBF); rlabel(,`\hbox{\tt (,,WBF)}')
    move right_
    dswitch(,,WdBKF); rlabel(,`\hbox{\tt (,,WdBKF)}')
    move right_
    dswitch(,,WBL); rlabel(,`\hbox{\tt (,,WBL)}')
    }
  move down; right_; {
    dswitch(,,WdBKL); rlabel(,`\hbox{\tt (,,WdBKL)}')
    move right_
    dswitch(,,WBT); rlabel(,`\hbox{\tt (,,WBT)}')
    move right_
    dswitch(,,WdBKC); rlabel(,`\hbox{\tt (,,WdBKC)}')
    move right_
    dswitch(,,WBM); rlabel(,`\hbox{\tt (,,WBM)}')
    move right_
    dswitch(,,WBCO); rlabel(,`\hbox{\tt (,,WBCO)}')
    move right_
    dswitch(,,WBMP); rlabel(,`\hbox{\tt (,,WBMP)}')
    }
  move down; right_; {
    dswitch(,,WBCY); rlabel(,`\hbox{\tt (,,WBCY)}')
    move right_
    dswitch(,,WBCZ); rlabel(,`\hbox{\tt (,,WBCZ)}')
    move right_
    dswitch(,,WBCE); rlabel(,`\hbox{\tt (,,WBCE)}')
    move right_
    dswitch(,,WBRH); rlabel(,`\hbox{\tt (,,WBRH)}')
    move right_
    dswitch(,,WBRdH); rlabel(,`\hbox{\tt (,,WBRdH)}')
    move right_
    dswitch(,,WBRHH); rlabel(,`\hbox{\tt (,,WBRHH)}')
    }
  move down; right_; {
    dswitch(,,WBMMR); rlabel(,`\hbox{\tt (,,WBMMR)}')
    move right_
    dswitch(,,WBMM); rlabel(,`\hbox{\tt (,,WBMM)}')
    move right_
    dswitch(,,WBMR); rlabel(,`\hbox{\tt (,,WBMR)}')
    move right_
    dswitch(,,WBEL); rlabel(,`\hbox{\tt (,,WBEL)}')
    move right_
    dswitch(,,WBLE); rlabel(,`\hbox{\tt (,,WBLE)}')
    move right_
    dswitch(,,WdBKEL); rlabel(,`\hbox{\tt (,,WdBKEL)}')
    }

.PE
