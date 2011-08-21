% `Relay.m4'
.PS
cct_init
{contact
 {`"\tt contact"' at last [].s+(0,-0.2)
  "\sl P" rjust at last [].P
  "\sl O" ljust at last [].O
  "\sl C" ljust at last [].C }
 move right_ 1.0; contact(,R)
 {`"\tt contact(,R)"' at last [].s+(0,-0.2)
  "\sl P" rjust at last [].P
  "\sl O" ljust at last [].O
  "\sl C" ljust at last [].C }
 move right_ 1.0; contact(O,); {`"\tt contact(O,)"' at last [].s+(0,-0.2)}
 move right_ 1.0; contact(C,); {`"\tt contact(C,)"' at last [].s+(0,-0.2)}
}
move down 1.5
right_
relay
 {"\sl V1" rjust at last [].V1
  "\sl V2" ljust at last [].V2
  "\sl P1" rjust at last [].P1
  "\sl O1" ljust at last [].O1
  "\sl C1" ljust at last [].C1 }
`"\tt relay"' at last [].s+(0,-0.2)
relay(2,,) with .sw at last [].sw+(1.2,0)
 {"\sl V1" rjust at last [].V1
  "\sl V2" ljust at last [].V2
  "\sl P1" rjust at last [].P1
  "\sl O1" ljust at last [].O1
  "\sl C1" ljust below at last [].C1
  "\sl P2" rjust at last [].P2
  "\sl O2" ljust at last [].O2
  "\sl C2" ljust at last [].C2 }
`"\tt relay(2,,)"' at last [].s+(0,-0.2)
relay(2,,R) with .sw at last [].sw+(1.2,0)
`"\tt relay(2,,R)"' at last [].s+(0,-0.2)
relay(2,O,) with .sw at last [].sw+(1.2,0)
`"\tt relay(2,O,)"' at last [].s+(0,-0.2)
relay(2,C,) with .sw at last [].sw+(1.2,0)
`"\tt relay(2,C,)"' at last [].s+(0,-0.2)
.PE
