% `fet.m4'
.PS
cct_init
dwn = 0.8
rgt = 0.9

dnl                               `IRF4905(linespec,R)'
define(`IRF4905',
 `[ ifelse(`$1',,,`eleminit_(`$1')')
   Q: mosfet(,`$2',dMdPzEDSQdB,) ifelse(`$1',,`
     S: Q.tr_xy(-4,-2); line from Q.tr_xy(-2,-2) to S
     D: Q.tr_xy( 4,-2); line from Q.tr_xy(2,-2) to D',
    `with .Diode.c at last line.c
     S: last line.start; D: last line.end; line from S to D ')
   G: Q.G
   circle rad 5*dimen_/10 at Q.tr_xy(0,1) ]')

define(`rmove',0.75)
define(`dir_',`right_')

Orig: Here
dir_
  {move left 0.5}
  {j_fet(right_ dimen_,,,E)
    "{\tt j\_fet(right\_ dimen\_,,,E)}" at last [].s+(0.0,-0.05) below
    "G" at last [].G rjust above
    "S" at last [].S rjust
    "D" at last [].D ljust
    }

  {move down_ dwn; dir_
    j_fet(,,P,)
    "{\tt j\_fet(,,P,)}" at last [].s+(0,-0.05) below
    "G" at last [].G rjust
    "S" at last [].S rjust
    "D" at last [].D ljust
    }

  move right_ 1.35; dir_
    {e_fet(,R,,)
     "{\tt e\_fet(,R,,)}" at last [].s+(0,-0.2) below
    "G" at last [].G rjust
    "S" at last [].S rjust
    "D" at last [].D ljust
     }

  {move down_ dwn; dir_
    e_fet(,,P,)
    "{\tt e\_fet(,,P,)}" at last [].s+(0,-0.05) below}

  move right_ 0.9; dir_
    {d_fet(,,,)
     "{\tt d\_fet(,,,)}" at last [].s+(0,-0.05) below}

  {move down_ dwn; dir_
    d_fet(,,P,)
    "{\tt d\_fet(,,P,)}" at last [].s+(0,-0.05) below}

  move right_ 0.9; dir_
    {e_fet(,,,S)
     "{\tt e\_fet(,,,S)}" at last [].s+(0,-0.05) below}

  {move down_ dwn; dir_
    e_fet(,,P,S)
    "{\tt e\_fet(,,P,S)}" at last [].s+(0,-0.05) below}

  move right_ 0.9; dir_
    {d_fet(,,,S)
     "{\tt d\_fet(,,,S)}" at last [].s+(0,-0.05) below}

  {move down_ dwn; dir_
    d_fet(,,P,S)
    "{\tt d\_fet(,,P,S)}" at last [].s+(0,-0.05) below}

  move right_ 0.9; dir_
    {c_fet(,,,)
    "{\tt `c\_fet'(,,,)}" at last [].s+(0,-0.05) below}

  {move down_ dwn; dir_
    c_fet(,,P)
    "{\tt `c\_fet'(,,P)}" at last [].s+(0,-0.05) below}

move down 1; right_
 [
  linewid = linewid*1.2

  Q1: mosfet(,,dGSDF,)
  {"\tt `mosfet'(,,dGSDF,)" at last [].s+(0,-0.05) below
    thinlines_
    arrow <- down .05 left .15 from (Q1.G.x,Q1.G.y-0.05)
    "\tt dG" rjust
    arrow <- down .10 left .30 from Q1.Channel.start+(.15,0)
    "\tt F" rjust
    arrow <- down .05 left .15 from (Q1.S.x,Q1.S.y+0.05)
    "\tt S" rjust
    arrow <- down .05 right .15 from (Q1.D.x,Q1.D.y+0.05)
    "\tt D" ljust
    thicklines_ }

  move right_ rmove
  Q2: mosfet(,,uHSDF,)
  {"\tt `$\ldots$'(,,uHSDF,)" at last [].s+(0,-0.15) below
    thinlines_
    arrow <- down .05 left .15 from (Q2.G.x,Q2.G.y-0.05)
    "\tt uH" rjust
    thicklines_ }

  move right_ rmove
  Q3: mosfet(,,dMEDSQuB,)
  {"\tt `$\ldots$'(,,dMEDSQuB,)" at last [].s+(0,-0.05) below
    thinlines_
    arrow <- down .05 left .15 from (Q3.G.x,Q3.G.y-0.05)
    "\tt dM" rjust
    arrow <- down .13 left .30 from Q3.Channel.start+(.12,0)
    "\tt E" rjust
    arrow <- down .05 left .10 from Q3.S+(.06,0)
    "\tt Q" rjust
    arrow <- down .08 right .24 from (Q3.B.x,Q3.B.y+0.175)
    "\tt uB" ljust
    thicklines_ }

  move right_ rmove
  Q4:  mosfet(,,uMEDSuB)
  {`"{\tt $\ldots$(,,uMEDSuB)}"' at last [].s+(0,-0.15) below
    "G" at last [].G rjust
    "S" at last [].S rjust
    "D" at last [].D ljust
    "B" at last [].B below
    }

  move right_ rmove
  Q5: mosfet(,,ZSDFdT,)
  {"\tt `$\ldots$'(,,ZSDFdT,)" at last [].s+(0,-0.05) below
    thinlines_
    arrow <- down .08 left .08 from (Q5.S.x,Q5.S.y+0.12)
    "\tt Z" rjust
    arrow from last arrow.end to Q5.Channel.c+(0.05,0)
    arrow from last arrow.start to (Q5.D.x,Q5.D.y+.05)
    arrow <- down .08 right .24 from (Q5.G.x,Q5.G.y-0.02)
    "\tt dT" ljust
    thicklines_ }

  move right_ rmove
  up_
  Q6: IRF4905 with .c at Here
  {`"\tt IRF4905"' at Q6.s+(0,-0.15) below
    "G" at Q6.G rjust
    "D" at Q6.D ljust above
    "S" at Q6.S ljust below
    }
  ] with .nw at Orig+(-0.26,-1.3)

.PE
