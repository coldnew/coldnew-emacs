% `Nport.m4'
.PS
textoffset = 5bp__
Twoport: nport

 "\sl W1a" above rjust at Twoport.W1a
 "\sl W1b" below rjust at Twoport.W1b
 "\sl E1a" above ljust at Twoport.E1a
 "\sl E1b" below ljust at Twoport.E1b

Nport: nport(wid 2.0 ht 1 fill_(0.9) "n-port",1,2,3,4) \
  with .Box.w at Twoport.Box.e+(1.0,0)

  "\sl W1a" rjust at Nport.W1a
  "\sl W1b" rjust at Nport.W1b

 "\sl E1a" ljust at Nport.E1a
 "\sl E3b" ljust at Nport.E3b

 "\sl N1a" above at Nport.N1a
 "\sl N1b$\;\;$" above at Nport.N1b
 "\sl $\;\;$N2a" above at Nport.N2a
 "\sl N2b" above at Nport.N2b

 "\sl S1a" below at Nport.S1a
 "\sl S4b" below at Nport.S4b

 "$\cdots$" at 0.5<Nport.S1a,Nport.S4b>+(0,-0.10)
 "$\vdots$" at 0.5<Nport.E1a,Nport.E3b>+(0.15,0.05)

Nterm: nterm with .Box.w at Nport.Box.e+(1.0,0)
  "\sl W1" rjust at Nterm.W1
  "\sl E1" ljust at Nterm.E1
  "\sl S1" below at Nterm.S1

 `"\tt nport"' at Twoport.s+(0,-0.3)
 `"\tt nport(wid 2.0 ht 1 fill\_(0.9) \"n-port\",1,2,3,4)"' \
  at Nport.s+(0,-0.25)
 `"\tt nterm"' at Nterm.s+(0,-0.3)
.PE
