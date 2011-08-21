.PS
# `Relay.m4'
cct_init
move right 0.3
{contact
 {`"contact"' at last [].s+(0,-0.2)
  "P" rjust at last [].P
  "O" ljust at last [].O
  "C" ljust at last [].C }
 move right_ 1.0; contact(,R)
 {`"contact(,R)"' at last [].s+(0,-0.2)
  "P" rjust at last [].P
  "O" ljust at last [].O
  "C" ljust at last [].C }
 move right_ 1.0; contact(O,); {`"contact(O,)"' at last [].s+(0,-0.2)}
 move right_ 1.0; contact(C,); {`"contact(C,)"' at last [].s+(0,-0.2)}
}
move down 1.5
right_
relay
 {"V1" rjust at last [].V1
  "V2" ljust at last [].V2
  "P1" rjust at last [].P1
  "O1" ljust at last [].O1
  "C1" ljust at last [].C1 }
`"relay"' at last [].s+(0,-0.2)
relay(2,,) with .sw at last [].sw+(1.2,0)
 {"V1" rjust at last [].V1
  "V2" ljust at last [].V2
  "P1" rjust at last [].P1
  "O1" ljust at last [].O1
  "C1" ljust below at last [].C1
  "P2" rjust at last [].P2
  "O2" ljust at last [].O2
  "C2" ljust at last [].C2 }
`"relay(2,,)"' at last [].s+(0,-0.2)
relay(2,,R) with .sw at last [].sw+(1.2,0)
`"relay(2,,R)"' at last [].s+(0,-0.2)
relay(2,O,) with .sw at last [].sw+(1.2,0)
`"relay(2,O,)"' at last [].s+(0,-0.2)
relay(2,C,) with .sw at last [].sw+(1.2,0)
`"relay(2,C,)"' wid 1 at last [].s+(0,-0.2)
.PE
