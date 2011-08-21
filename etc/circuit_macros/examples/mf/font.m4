\documentclass[11pt]{article}
\usepackage{mfpic}
\pagestyle{empty}
\begin{document}
\thispagestyle{empty}
\opengraphsfile{cct}

ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')
define(`srad_',`sourcerad_*3/2')
define(`quad_',`srad_/sqrt(2)')
define(`Dim',`dimen_*2/3')
.PS
  resistor(right_ Dim)
.PE
.PS
  resistor(up_ Dim)
.PE
.PS
  inductor(right_ Dim,W)
.PE
.PS
  inductor(down_ Dim,W)
.PE
.PS
  inductor(left_ Dim,W)
.PE
.PS
  inductor(up_ Dim,W)
.PE
.PS
  {eleminit_(right Dim)}
  m4m_core(Here,m4wd,m4ht+dimen_/24,dimen_/16)
.PE
.PS
  {eleminit_(up_ Dim)}
  m4m_core(Here,m4wd,m4ht+dimen_/24,dimen_/16)
.PE
.PS
  inductor(right_ Dim)
.PE
.PS
  inductor(down_ Dim)
.PE
.PS
  inductor(left_ Dim)
.PE
.PS
  inductor(up_ Dim)
.PE
.PS
  inductor(up_ Dim)
.PE
.PS
  {eleminit_(right Dim)}
  m4m_core(Here,m4wd,m4ht+dimen_/24,dimen_/16)
.PE
.PS
  {eleminit_(up_ Dim)}
  m4m_core(Here,m4wd,m4ht+dimen_/24,dimen_/16)
.PE
.PS
  capacitor(right_ Dim,C)
.PE
.PS
  capacitor(down_ Dim,C)
.PE
.PS
  capacitor(left_ Dim,C)
.PE
.PS
  capacitor(up_ Dim,C)
.PE
.PS
  capacitor(right_ Dim)
.PE
.PS
  capacitor(up_ Dim)
.PE
.PS
  diode(right_ Dim)
.PE
.PS
  diode(down_ Dim)
.PE
.PS
  diode(left_ Dim)
.PE
.PS
  diode(up_ Dim)
.PE
.PS
  diode(right_ Dim,Z)
.PE
.PS
  diode(down_ Dim,Z)
.PE
.PS
  diode(left_ Dim,Z)
.PE
.PS
  diode(up_ Dim,Z)
.PE
.PS
  diode(right_ Dim,BD)
.PE
.PS
  diode(down_ Dim,BD)
.PE
.PS
  xtal(right_ Dim)
.PE
.PS
  xtal(down_ Dim)
.PE
.PS
  source(right_ Dim)
.PE
.PS
  source(up_ Dim)
.PE
.PS
  arc rad sourcerad_/3 cw from Here-(sourcerad_*2/3,0) to Here\
     with .c at Here-(sourcerad_/3,0)
  arc rad sourcerad_/3 ccw from Here to Here+(sourcerad_*2/3,0) \
     with .c at Here+(sourcerad_/3,0)
.PE
.PS
  consource(right_ Dim)
.PE
.PS
  consource(up_ Dim)
.PE
.PS
  {move down 1 bp__ then up_ 2 bp__}; arrow right_ srad_
.PE
.PS
  {move right 1 bp__ then left 2 bp__}; arrow down_ srad_
.PE
.PS
  {move down 1 bp__ then up_ 2 bp__}; arrow left_ srad_
.PE
.PS
  {move right 1 bp__ then left 2 bp__}; arrow up_ srad_
.PE
.PS
  arrow right quad_ up quad_
.PE
.PS
  arrow left quad_ up quad_
.PE
.PS
  arrow left quad_ down quad_
.PE
.PS
  arrow right quad_ down quad_
.PE
.PS
  battery(right_ Dim)
.PE
.PS
  battery(down_ Dim)
.PE
.PS
  battery(left_ Dim)
.PE
.PS
  battery(up_ Dim)
.PE
.PS
  battery(right_ Dim,3)
.PE
.PS
  battery(down_ Dim,3)
.PE
.PS
  battery(left_ Dim,3)
.PE
.PS
  battery(up_ Dim,3)
.PE
.PS
  ebox(right_ Dim)
.PE
.PS
  ebox(up_ Dim)
.PE
.PS
  switch(right_ Dim)
.PE
.PS
  switch(down_ Dim)
.PE
.PS
  switch(left_ Dim)
.PE
.PS
  switch(up_ Dim)
.PE
.PS
  switch(right_ Dim,,O)
.PE
.PS
  switch(down_ Dim,,O)
.PE
.PS
  switch(left_ Dim,,O)
.PE
.PS
  switch(up_ Dim,,O)
.PE
.PS
  switch(right_ Dim,,C)
.PE
.PS
  switch(down_ Dim,,C)
.PE
.PS
  switch(left_ Dim,,C)
.PE
.PS
  switch(up_ Dim,,C)
.PE
.PS
  amp(right_ Dim*3/2*5/4)
.PE
.PS
  amp(down_ Dim*3/2*5/4)
.PE
.PS
  amp(left_ Dim*3/2*5/4)
.PE
.PS
  amp(up_ Dim*3/2*5/4)
.PE
.PS
  delay(right_ Dim)
.PE
.PS
  delay(down_ Dim)
.PE
.PS
  delay(left_ Dim)
.PE
.PS
  delay(up_ Dim)
.PE
.PS
rp_ang = 0
  right_
  ground(,T)
.PE
.PS
rp_ang = 0
  right_
  ground
.PE
.PS
  down_; transformer(,L)
.PE
.PS
  right_; transformer(,L)
.PE
.PS
  up_; bi_tr
.PE
.PS
  left_; bi_tr
.PE
.PS
  up_; bi_tr(,R)
.PE
.PS
  right_; bi_tr(,R)
.PE
.PS
  up_; bi_tr(,,P)
.PE
.PS
  left_; bi_tr(,,P)
.PE
.PS
  up_; bi_tr(,R,P)
.PE
.PS
  right_; bi_tr(,R,P)
.PE
.PS
  circle rad 4*dimen_/10
.PE
.PS
  up_; j_fet
.PE
.PS
  up_; j_fet(,R)
.PE
.PS
  up_; j_fet(,,P,)
.PE
.PS
  up_; j_fet(,R,P,)
.PE
.PS
  up_; e_fet(,,,)
.PE
.PS
  up_; e_fet(,R,,)
.PE
.PS
  up_; e_fet(,,P,)
.PE
.PS
  up_; e_fet(,R,P,)
.PE
.PS
  up_; d_fet(,,,)
.PE
.PS
  up_; d_fet(,R,,)
.PE
.PS
  up_; d_fet(,,P,)
.PE
.PS
  up_; d_fet(,R,P,)
.PE
.PS
  right_; AND_gate
.PE
.PS
  down_; AND_gate
.PE
.PS
  left_; AND_gate
.PE
.PS
  up_; AND_gate
.PE
.PS
  right_; OR_gate
.PE
.PS
  down_; OR_gate
.PE
.PS
  left_; OR_gate
.PE
.PS
  up_; OR_gate
.PE
.PS
  right_; BUFFER_gate
.PE
.PS
  down_; BUFFER_gate
.PE
.PS
  left_; BUFFER_gate
.PE
.PS
  up_; BUFFER_gate
.PE
.PS
  right_; NAND_gate
.PE
.PS
  down_; NAND_gate
.PE
.PS
  left_; NAND_gate
.PE
.PS
  up_; NAND_gate
.PE
.PS
  right_; NOR_gate
.PE
.PS
  down_; NOR_gate
.PE
.PS
  left_; NOR_gate
.PE
.PS
  up_; NOR_gate
.PE
.PS
  right_; XOR_gate
.PE
.PS
  down_; XOR_gate
.PE
.PS
  left_; XOR_gate
.PE
.PS
  up_; XOR_gate
.PE
.PS
  right_; NXOR_gate
.PE
.PS
  down_; NXOR_gate
.PE
.PS
  left_; NXOR_gate
.PE
.PS
  up_; NXOR_gate
.PE
.PS
  right_; NOT_gate
.PE
.PS
  down_; NOT_gate
.PE
.PS
  left_; NOT_gate
.PE
.PS
  up_; NOT_gate
.PE

\closegraphsfile
\end{document}
