.PS
# `Emarrows.m4'
cct_init
  hskip = 0.6
[
  { em_arrows(N)
    box dotted wid last [].wid ht last [].ht at last [].c
    thinlines_
    arrow <- up 0.12 from last [].Head;`"Head"' above
    arrow <- down 0.12 from last [].Tail;`"Tail"' below
   `"A1"' rjust below at last [].A1.c
   `"A2"' ljust above at last [].A2.c
    thicklines_
    move down 0.25 from last [].s
    move up 0.2 from last [].n
   `"em_arrows(N)"' wid 1.0 at last [].s+(0,-0.20) below }
  { em_arrows(ND,45) with .sw at last [].se+(hskip*1.5,0)
   `"em_arrows(ND,45)"' at last [].s below }
  { em_arrows(I) with .sw at last [].se+(hskip,0)
   `"...(I)"' at last [].s below }
  { em_arrows(ID) with .sw at last [].se+(hskip,0)
  `"...(ID)"' at last [].s below }
  { em_arrows(E) with .sw at last [].se+(hskip,0)
   `"...(E)"' at last [].s below }
  { em_arrows(ED) with .sw at last [].se+(hskip,0)
  `"...(ED)"' wid .75 at last [].s below }
  ]
# box wid last [].wid ht last [].ht at last []

.PE
