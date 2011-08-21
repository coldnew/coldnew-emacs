% `currents.m4'
.PS
cct_init
  del = 0.75
 {right_
  resistor; b_current(i)
  {"\tt b\_current(i)" at last line + (0,-0.20)}
  move right_ del
  resistor; b_current(i,below_)
  {"\tt $\ldots$(i,below\_)" at last line + (0,-0.20)}
  move right_ del
  resistor; b_current(i,,O)
  {"\tt $\ldots$(i,,O)" at last line + (0,-0.20)}
  move right_ del
  resistor; b_current(i,below_,O)
  {"\tt $\ldots$(i,below\_,O)" at last line + (0,-0.20)}
  }

  move down_ 0.60
 {right_
  resistor; b_current(i,,,E)
  {"\tt b\_current(i,,,E)" at last line + (0,-0.20)}
  move right_ del
  resistor; b_current(i,below_,,E)
  {"\tt $\ldots$(i,below\_,,E)" at last line + (0,-0.20)}
  move right_ del
  resistor; b_current(i,,O,E,0.2)
  {"\tt $\ldots$(i,,O,E,0.2)" at last line + (0,-0.20)}
  move right_ del
  resistor; b_current(i,below_,O,E)
  {"\tt $\ldots$(i,below\_,O,E)" at last line + (0,-0.20)}
  }

  move down_ 0.60
 {right_
  del = 0.55
  inductor; larrow(i)
  {"`\tt larrow(i)'" at last line + (0,-0.20)}
  move right_ del
  inductor; rarrow(i)
  {"`\tt rarrow(i)'" at last line + (0,-0.20)}
  move right_ del
  inductor; larrow(i,<-)
  {"`\tt larrow(i,<-)'" at last line + (0,-0.20)}
  move right_ del
  inductor; rarrow(i,<-)
  {"`\tt rarrow(i,<-)'" at last line + (0,-0.20)}
  }
.PE
