.PS 3.5
# ex05.m4
ifdef(`dline',,`include(HOMELIB_`'darrow.m4)')
darrow_init

  dx = 0.1
  circlerad = boxht*3/8
  extlen=4*dx
  boxwid = 0.75
  indel = boxht/3
  fillval = 0.8

COb: box fill_ "CO"
  { line right textht*3/4 from COb+(0,textht*0.8) }
CO:box fill_ with .nw at last box.se+(dx,-dx) "CO"
CbOb: box fill_ with .nw at last box.se+(dx,-dx) "CO"
  { line right textht*1.3 with .c at CbOb+(0,textht*0.8) }
CbO: box fill_ with .nw at last box.se+(dx,-dx) "CO"
  { line left textht*3/4 from CbO+(0,textht*0.8) }

  dline(from CbO.s down_ dx,,t,,|-)
  dright
{Sum: circle at (CO,Here)}
  darrow(to Sum.e)

SW: (COb.w,Sum.s)+(-3*dx-extlen/2,-dx)
NE: (CbO.e,COb.n)+(dx,dx)
  right_; shadebox(box wid NE.x-SW.x ht NE.y-SW.y with .sw at SW)

U: COb.w-(3*dx+extlen,0)
  {"svg_bf(u)" wid 0.1 rjust at U}
  dline(from U right_ extlen+dx,,t,,|-)
TU: dtee(R)
  {darrow(to (COb.w,Here),t,)}
  dline(to (Here,CO),t,t)
{Dum: circle at (Here,Sum)}
  dtee(L)
  {darrow(to CO.w,t,)}
{D: box "svg_bf(D)" wid boxwid/2 ht boxwid/2 at 0.5<Here,Dum.n>}
  darrow(to D.n,t,)
  darrow(from D.s to Dum.n,,,,,,|)

  dline(from CO.n to (CO,COb)-(0,indel),,t,,|-)
  up_; dleft; darrow(to (COb.e,Here))

  dline(from CbOb.n to (CbOb,COb),,t,,|-)
  up_; dleft; darrow(to COb.e)

  dline(from CbO.n-(indel,0) up_ boxht/2+dx,,t,,|-)
  dleft; darrow(to CbOb.e)

  dline(from CbO.n to (CbO,CO),,t,,|-)
  up_; dleft
  {box invis fill_(1) ht dlinewid wid boxwid+2*dx with .e at Here-(dx,0)}
  darrow(to CO.e)

  dline(from CbO.n+(indel,0) to (CbO,COb)+(indel,indel),,t,,|-)
  up_; dleft; darrow(to (COb.e,Here))

  darrow(from CO.s to Sum.n,,,,,,|)

  eps=0.005
  move to Sum.w-(eps,0); left_
  {{line invis from Dum.w to (U,Dum)}
     box invis fill_(1) ht dlinewid wid last line.start.x-last line.end.x\
     with .e at Here-(dx,0)}
  darrow(to Dum.e)
  {box invis fill_(1) ht dlinewid wid dlinewid at (SW,Here)}
  darrow(from Dum.w-(eps,0) to (U,Dum))
     {"svg_bf(y)" rjust }
  {line invis from Here+(-0.23,0) to Here+(-0.23,0.1)}

.PE
