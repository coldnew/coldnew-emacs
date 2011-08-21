.PS
# `Nport.m4'
textoffset = 5bp__
Twoport: nport

 "W1a" wid 0.35 above rjust at Twoport.W1a
 "W1b" below rjust at Twoport.W1b
 "E1a" above ljust at Twoport.E1a
 "E1b" below ljust at Twoport.E1b

#Nport: nport(wid 1 ht 1.5 fill_(0.9) svgLinkString(Np.svg,n-port,B),1,2,3,4) \
#  with .Box.w at Twoport.Box.e+(1.0,0)
Nport: nport(wid 1.75 ht 1 fill_(0.9),1,2,3,4) \
  with .Box.w at Twoport.Box.e+(1.25,0)
  svgLink(Np.svg,"n-port" at Nport,B)

  "W1a" rjust at Nport.W1a
  "W1b" rjust at Nport.W1b

 "E1a" ljust at Nport.E1a
 "E3b" ljust at Nport.E3b

 "N1a" above at Nport.N1a
 "N1b  " above at Nport.N1b
 "  N2a" above at Nport.N2a
 "N2b" above at Nport.N2b

 "S1a" below at Nport.S1a
 "S4b" below at Nport.S4b

 "..." at 0.5<Nport.S1a,Nport.S4b>+(0,-0.10)
 "..." at 0.5<Nport.E1a,Nport.E3b>+(0.15,0.05)

Nterm: nterm with .Box.w at Nport.Box.e+(1.0,0)
  "W1" rjust at Nterm.W1
  "E1" wid 0.2 ljust at Nterm.E1
  "S1" below at Nterm.S1

 `"nport"' at Twoport.s+(0,-0.3)
 `"nport(wid 1.5 ht 1.75 fill_(0.9),1,2,3,4)"' ljust at Nport.sw+(0,-0.3)
  `"svgLink(Np.svg,\"n-port\" at Nport,B)"' ljust at Nport.sw+(0,-0.5)
 `"nterm"' at Nterm.s+(0,-0.3)
.PE
