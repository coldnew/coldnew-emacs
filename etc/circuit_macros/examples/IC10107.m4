% IC10107.m4
.PS
   ifdef(`NOR_gate',,`include(HOMELIB_`'liblog.m4)')
   cct_init

right_
   pinlen = 2*NOT_rad
   hunit = AND_wd*L_unit*1.6
B: box invis wid hunit*4 ht hunit*2
   psep = hunit/2
   nrad = B.ht*0.06
   line from B.w+(0,nrad) to B.nw then to B.ne then to B.se then to B.sw \
     then to B.w-(0,nrad)
   arc ccw to Here+(0,2*nrad) with .c at B.w
   for i = 1 to 8 do {
     line down pinlen from B.sw+(psep/2,0)+((i-1)*psep,0)
     sprintf("\small\sf%g",i) below
     exec sprintf("P%g: last line.start",i)
     line up pinlen from B.ne-(psep/2,0)-((i-1)*psep,0)
     sprintf("\small\sf%g",8+i) above
     exec sprintf("P%g: last line.start",8+i)
     }
G1: OR_gen(2,PIBANEONSEC) with .Out at P12.x-pinlen,B.y+B.ht/4
G2: OR_gen(2,PIBANNEOSEC) with .Out at B.e.x-4*pinlen,B.y
   left_
G3: OR_gen(2,PIBANEONSEC) with .NE at P3.x,B.y-B.ht/4-AND_ht/6*L_unit

   line from G3.N_NSE.w to (P2,G3.N_NSE) then to P2
   line from P3 to G3.NE
   line right pinlen from G3.In1 then down 2*pinlen
   contline to (P4,Here) then to P4
   line from G3.In2 to (P5,G3.In2) then to P5

   line from G2.In2 left pinlen then down 2*pinlen
   contline to (P9,Here) then to P9
   line from G2.In1 left pinlen*2 then down G2.In1.y-G2.In2.y+3*pinlen
   contline to (P7,Here) then to P7
   line from G2.SE right NOT_rad+2*pinlen then up B.n.y-pinlen-G2.SE.y
   contline to (P10,Here) then to P10
   line from G2.N_NNE.e right pinlen then up B.n.y-2*pinlen-G2.N_NNE.y
   contline to (P11,Here) then to P11

   line from P12 to (P12,G1.N_NSE) then to G1.N_NSE.e
   line from P13 down pinlen
   contline to (G1.NE,Here) then to G1.NE
   line from G1.In1 to (P14,G1.In1) then to P14
   line from G1.In2 to (P15,G1.In2) then to P15

.PE
