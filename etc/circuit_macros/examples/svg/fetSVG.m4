.PS
# `fet.m4'
cct_init
dwn = 0.8
rgt = 0.9

dnl                               `IRF4905(linespec,R)'
define(`IRF4905',
 `[ ifelse(`$1',,,`eleminit_(`$1')')
   Q: mosfet(,`$2',LdPzEDSQdB,) ifelse(`$1',,`
     S: Q.tr_xy(-4,-2); line from Q.tr_xy(-2,-2) to S
     D: Q.tr_xy( 4,-2); line from Q.tr_xy(2,-2) to D',
    `with .Diode.c at last line.c
     S: last line.start; D: last line.end; line from S to D ')
   G: Q.G
   circle rad 5*dimen_/10 at Q.tr_xy(0,1) ]')

define(`rmove',0.75)
define(`dir_',`right_')

dir_
  {j_fet(right_ dimen_,,,E)
   `"j_fet(right_ dimen_,,,E)"' wid 1.5 at last [].s+(0.0,-0.05) below
    "G" at last [].G rjust above
    "S" at last [].S rjust
    "D" at last [].D ljust
    }

  {move down_ dwn; dir_
    j_fet(,,P,)
   `"j_fet(,,P,)"' at last [].s+(0,-0.05) below
    "G" at last [].G rjust
    "S" at last [].S rjust
    "D" at last [].D ljust
    }

  move right_ 1.35; dir_
    {e_fet(,R,,)
    `"e_fet(,R,,)"' at last [].s+(0,-0.2) below
    "G" at last [].G rjust
    "S" at last [].S rjust
    "D" at last [].D ljust
     }

  {move down_ dwn; dir_
    e_fet(,,P,)
   `"e_fet(,,P,)"' at last [].s+(0,-0.05) below}

  move right_ 1; dir_
    {d_fet(,,,)
    `"d_fet(,,,)"' at last [].s+(0,-0.05) below}

  {move down_ dwn; dir_
    d_fet(,,P,)
   `"d_fet(,,P,)"' at last [].s+(0,-0.05) below}

  move right_ 1; dir_
    {e_fet(,,,S)
    `"e_fet(,,,S)"' at last [].s+(0,-0.05) below}

  {move down_ dwn; dir_
    e_fet(,,P,S)
   `"e_fet(,,P,S)"' at last [].s+(0,-0.05) below}

  move right_ 1; dir_
    {d_fet(,,,S)
    `"d_fet(,,,S)"' at last [].s+(0,-0.05) below}

  {move down_ dwn; dir_
    d_fet(,,P,S)
   `"d_fet(,,P,S)"' at last [].s+(0,-0.05) below}
right_
{ [
  c_fet(,,,)
 `"c_fet(,,,)"' at last [].s+(0,-0.05) below
  move right rgt from last [].e
  c_fet(,,P)
 `"c_fet(,,P)"' at last [].s+(0,-0.05) below
  ] with .n at 5th last [].s+(0,-0.3)
  }
move down 1; right_
[
  linewid = linewid*1.2

  Q1: mosfet(,,dGSDF,)
  { "`mosfet'(,,dGSDF,)" at last [].s+(0,-0.05) below wid 1.3
    thinlines_
    arrow <- down .05 left .15 from (Q1.G.x,Q1.G.y-0.05)
    "dG" rjust
    arrow <- down .10 left .30 from Q1.Channel.start+(.15,0)
    "F" rjust
    arrow <- down .05 left .15 from (Q1.S.x,Q1.S.y+0.05)
    "S" rjust
    arrow <- down .05 right .15 from (Q1.D.x,Q1.D.y+0.05)
    "D" ljust
    thicklines_ }

  move right_ rmove
  Q2: mosfet(,,uHSDF,)
  {"`...'(,,uHSDF,)" at last [].s+(0,-0.15) below
    thinlines_
    arrow <- down .05 left .15 from (Q2.G.x,Q2.G.y-0.05)
    "uH" rjust
    thicklines_ }

  move right_ rmove
  Q3: mosfet(,,LEDSQuB,)
  {"`...'(,,LEDSQuB,)" at last [].s+(0,-0.05) below
    thinlines_
    arrow <- down .05 left .15 from (Q3.G.x,Q3.G.y-0.05)
    "L" rjust
    arrow <- down .13 left .30 from Q3.Channel.start+(.12,0)
    "E" rjust
    arrow <- down .05 left .10 from Q3.S+(.06,0)
    "Q" rjust
    arrow <- down .08 right .24 from (Q3.B.x,Q3.B.y+0.175)
    "uB" ljust
    thicklines_ }

  move right_ rmove
  Q4:  mosfet(,,LEDSuB)
  {`"...(,,LEDSuB)"' at last [].s+(0,-0.15) below
    "G" at last [].G rjust
    "S" at last [].S rjust
    "D" at last [].D ljust
    "B" at last [].B below
    }

  move right_ rmove
  Q5: mosfet(,,ZSDFdT,)
  {"`...'(,,ZSDFdT,)" at last [].s+(0,-0.05) below
    thinlines_
    arrow <- down .08 left .08 from (Q5.S.x,Q5.S.y+0.12)
    "Z" rjust
    arrow from last arrow.end to Q5.Channel.c+(0.05,0)
    arrow from last arrow.start to (Q5.D.x,Q5.D.y+.05)
    arrow <- down .08 right .24 from (Q5.G.x,Q5.G.y-0.02)
    "dT" ljust
    thicklines_ }

  move right_ rmove
  up_
  Q6: IRF4905 with .c at Here
  {`"IRF4905"' at Q6.s+(0,-0.15) below
    "G" at Q6.G rjust
    "D" at Q6.D ljust above
    "S" at Q6.S ljust below
    }
  ] with .n at last [].s+(0,-.2)

.PE
