% Logic.m4
.PS
cct_init
ifdef(`AND_gate',,`include(HOMELIB_`'liblog.m4)')
xgap = 0.6
[ dmov = 0.4
{  {AND_gate ;  "{\tt AND\_gate}" ljust at last [].w + (xgap,0)}
   move down dmov; right_
   {OR_gate ;  "{\tt OR\_gate}" ljust at last [].w + (xgap,0)}
   move down dmov; right_
   {BUFFER_gate ;  "{\tt BUFFER\_gate}" ljust at last [].w + (xgap,0)}
   move down dmov; right_
   {XOR_gate ;  "{\tt XOR\_gate}" ljust at last [].w + (xgap,0)}
   }

   move right_ 130pt__
{  { NAND_gate ;  "{\tt NAND\_gate}" ljust at last [].w + (xgap,0) }
   move down dmov; right_
   {  Gate: NOR_gate(3)
   "{\tt NOR\_gate(3)}" ljust at last [].w + (xgap,0)
   thinlines_
   spline <- from Gate.Out right arrowht*2.0 \
    then up 0.15 then right 0.05
     "{\tt Out}" ljust
   spline <- from Gate.N_Out down arrowht*2.0 \
     then right 0.1+arrowht*1.5+NOT_rad down 0.15-arrowht*2.0
     "{\tt N\_Out}" ljust
   for_(1,3,1,
     `arrow <- left 0.2 down 0.05 from Gate.In`'m4x
     "{\tt In`'m4x\/}" rjust at Here+(0,(2-m4x)*0.02)')
   }
   thicklines_
   move down dmov; right_
  {  NOT_gate ;  "{\tt NOT\_gate}" ljust at last [].w + (xgap,0) }
   move down dmov; right_
  {  NXOR_gate ;  "{\tt NXOR\_gate}" ljust at last [].w + (xgap,0)  }
}
   move right_ 130pt__
{ {NAND_gate(,B) ;  "{\tt NAND\_gate(,B)}" ljust at last [].w + (xgap,0)}
   { line invis right 1.6 from last [].se then down 1}
   move down dmov; right_
   move left N_diam*L_unit; right_
  {NOR_gate(3,NB) ;  "{\tt NOR\_gate(3,NB)}" ljust at (last "",last [])  }
   move down dmov; right_
  {BOX_gate(PN,N,,,=1)
   "{\tt BOX\_gate(PN,N,,,=1)}" ljust at (last "",last [])
   line left 0.15 from last [].In1
   line left 0.15-NOT_rad*2 from last [].In2
   line right 0.15 from last [].Out }
   move down dmov; move right_ N_diam*L_unit
  {BOX_gate(PP,N,,,=)
   "{\tt BOX\_gate(PP,N,,,=)}" ljust at (last "",last [])
   line left 0.15 from last [].In1
   line left 0.15 from last [].In2
   line right 0.15 from last [].Out }
}
]
.PE
