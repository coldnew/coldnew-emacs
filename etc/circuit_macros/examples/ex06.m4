% ex06.m4
.PS
#scale = 1.2
gen_init

  bight = 1.8i
  bigwd = 1.6i
  lettersp = 0.2
  overlap = 0.1
  qminusl = 0.6
  r = 0.3
  ex = 0.12
  del = 0.1
  ddel = del * sqrt(2)
  eps = 0.01
  widg = 1

F: box invis wid bigwd ht bight
E: box invis wid bigwd ht bight
G: box invis wid widg  ht bight
   box wid G.ne.x-F.nw.x ht F.nw.y-F.sw.y with .sw at F.sw
   box wid last box.wid+2*eps ht last box.ht+2*eps with .c at last box.c

   line from F.ne+(-eps/2,0) to F.se+(-eps/2,0)
   line from F.ne+( eps/2,0) to F.se+( eps/2,0)
   line from E.ne+(-eps/2,0) to E.se+(-eps/2,0)
   line from E.ne+( eps/2,0) to E.se+( eps/2,0)

   box invis ht 0.2 wid 0.2 "\Large $F$" with .c at F.s + (0,-lettersp)
   box invis ht 0.2 wid 0.2 "\Large $E$" with .c at E.s + (0,-lettersp)
   box invis ht 0.2 wid 0.2 "\Large $G$" with .c at G.s + (0,-lettersp)
XF: F.se + (-qminusl,qminusl)
XE: E.se + (-qminusl,qminusl)

   line from (F.w,XF) to XF + (overlap,0)
   line from (XF,F.n) to XF + (0,-overlap)
   line from XF + (-r,0) to (XF.x-r,F.n.y)
   line dashed from XF to F.se

   line from (E.w,XE) to XE + (overlap,0)
   line from (XE,E.n) to XE + (0,-overlap)
   line from XE + (-r,0) to (XE.x-r,E.n.y)
   line dashed from XE to E.se
   line from XE + (0,r) to E.w.x,XE.y+r

   line from (G.w,XE) to (G.e,XE)

   "$q$" at F.sw + (-ex,ex/3)
   "$\ell$" at (F.w,XF) + (-ex,ex/3)
   "$t$" at (XF,F.n) + (-ex/2,ex)
   "$t$" at (XE,E.n) + (-ex/2,ex)
   "$n$$+$$p$$+$$m$" at G.ne + (0,ex)
   "\large $0$" at XF + (-r/2,(bight-qminusl)/2)
   "\large $0$" at XF + (qminusl/2,(bight-qminusl)/2)

   "\large $0$" at XE + (-r/2,(bight-qminusl)/2)
   "\large $0$" at XE + (qminusl/2,(bight-qminusl)/2)

                            # extra left brace to avoid a psfrag problem (bug?)
   "$\vphantom{\{}\left.\strut\right\} r$" at XE + (ex,r/2)

   "$\overbrace{\phantom{\hbox to 0.7in{}}}^{\hbox{$k$}}$" at \
      F.nw + ((bigwd-qminusl-r)/2,ex*1.2)

   "$\overbrace{\phantom{\hbox to 0.7in{}}}^{\hbox{$k$}}$" at \
      E.nw + (bigwd/2-qminusl/2-r/2,ex*1.2)

   line invis up 0.3 from F.nw

   thinlines_

   for y = ddel to widg by ddel do {
      line down y left y from G.e.x,G.s.y+y }
   for y = last line.start.y+ddel to G.n.y by ddel do {
      line down widg left widg from G.e.x,y }
   for y = last line.end.y+ddel to G.n.y by ddel do {
      line up G.n.y-y right G.n.y-y from G.w.x,y }

   for x = del to XF.x-F.w.x-r by del do {
      line down bight-qminusl from E.w.x+x,E.nw.y
      line down bight-qminusl from F.w.x+x,F.nw.y }

   for y = XE.y+del to XE.y+r by del do { line from XE.x,y left r }

   for x = ddel to qminusl by ddel do {
      line left x down x from E.w.x+x,XE.y
      line left x down x from F.w.x+x,XF.y }
   for x = last line.start.x-F.w.x+ddel to XF.x-F.w.x by ddel do {
      line left qminusl down qminusl from E.w.x+x,XE.y
      line left qminusl down qminusl from F.w.x+x,XF.y }
   for x = last line.end.x-F.w.x+ddel to bigwd by ddel do {
      line up (bigwd-x)/2 right (bigwd-x)/2 from E.w.x+x,E.s.y
      line up (bigwd-x)/2 right (bigwd-x)/2 from F.w.x+x,F.s.y }

.PE
