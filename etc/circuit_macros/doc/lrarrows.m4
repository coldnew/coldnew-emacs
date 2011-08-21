% `lrarrows.m4'
.PS
cct_init
 del = 0.55
  inductor; larrow(i)
  {"`\tt larrow(i)'" at last line + (0,-0.25)}
  move right_ del
  inductor; rarrow(i)
  {"`\tt rarrow(i)'" at last line + (0,-0.25)}
  move right_ del
  inductor; larrow(i,<-)
  {"`\tt larrow(i,<-)'" at last line + (0,-0.25)}
  move right_ del
  inductor; rarrow(i,<-)
  {"`\tt rarrow(i,<-)'" at last line + (0,-0.25)}
.PE
