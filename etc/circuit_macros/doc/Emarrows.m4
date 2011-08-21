% `Emarrows.m4'
.PS
cct_init
  hskip = 0.6
[
  { em_arrows(N)
    box dotted wid last [].wid ht last [].ht at last [].c
    thinlines_
    arrow <- left 0.15 from last [].Head; "\sl Head" rjust
    arrow <- down 0.12 from last [].Tail; "\sl Tail" below
    "\sl A1" rjust below at last [].A1.c
    "\sl A2" ljust above at last [].A2.c
    thicklines_
    move down 0.25 from last [].s
    move up 0.2 from last [].n
    "\tt em\_arrows(N)" at last [].s+(0,-0.20) below }
  { em_arrows(ND,45) with .sw at last [].se+(hskip*1.5,0)
    "\tt em\_arrows(ND,45)" at last [].s below }
  { em_arrows(I) with .sw at last [].se+(hskip,0)
    "\tt $\ldots$(I)" at last [].s below }
  { em_arrows(ID) with .sw at last [].se+(hskip,0)
   "\tt $\ldots$(ID)" at last [].s below }
  { em_arrows(E) with .sw at last [].se+(hskip,0)
    "\tt $\ldots$(E)" at last [].s below }
  { em_arrows(ED) with .sw at last [].se+(hskip,0)
   "\tt $\ldots$(ED)" at last [].s below }
  ]
# box wid last [].wid ht last [].ht at last []

.PE
