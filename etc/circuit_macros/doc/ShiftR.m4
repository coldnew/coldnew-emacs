% ShiftR.m4
.PS

ifdef(`FlipFlopJK',,`include(HOMELIB_`'liblog.m4)')
log_init
define(`lg_plen',3)

define(`customFF',
 `[ Chip: box wid 10*L_unit ht FF_ht*L_unit
    lg_pin(Chip.w,\sf CK,PinCK,wEN)
    lg_pin(Chip.n,\sf PR,PinPR,nN)
    lg_pin(Chip.ne-svec_(0,int(FF_ht/4)),\sf Q,PinQ,e)
    ifelse(`$1',1,
     `lg_pin(Chip.se+svec_(0,int(FF_ht/4)),lg_bartxt(\sf Q),PinNQ,e)')
    lg_pin(Chip.s,\sf CLR,PinCLR,sN)
    lg_pin(Chip.sw+svec_(0,int(FF_ht/4)),\sf R,PinR,w)
    lg_pin(Chip.nw-svec_(0,int(FF_ht/4)),\sf S,PinS,w)
  ]')

  F0: customFF(1)

  BUFFER_gate(,N) with .Out at F0.PinS.end
    line left 2*L_unit from last [].In1
  T: dot
    reversed(`NOT_gate',left 10*L_unit)
  Serial: "\scriptsize\sf\shortstack{SERIAL\\\hfill INPUT}" rjust
    line from T to (T,F0.PinR) then to F0.PinR.end

  NCLR: NOT_gate(right T.x-Serial.x from (Serial,F0.PinCLR.end),N)
    "$\overline{\hbox{\scriptsize\sf CLEAR}}$" at NCLR.start rjust 

  CLK: NOT_gate(right T.x-Serial.x from NCLR.start+(0,-BUF_ht*3/2*L_unit))
    "\scriptsize\sf CLOCK" at CLK.start rjust

for_(1,4,1,`
  F`'m4x: customFF(eval(m4x!=4)) with .PinR.end at F`'eval(m4x-1).PinNQ.end
  ')

  "\scriptsize\sf OUTPUT" at F4.PinQ.end ljust
  line from NCLR.end to F4.PinCLR.end

  down_
for_(0,4,1,`
  line from F`'m4x.PinCK.end down F0.PinCK.y-CLK.y
  ifelse(eval(m4x!=4),1,`dot',`line to CLK.end')
  N`'m4x: NAND_gate with .Out at F`'m4x.PinPR.end 
    line up 2*L_unit from N`'m4x.In2
    { line up 6*L_unit from N`'m4x.In1
      "\scriptsize\sf PR`'eval(4-m4x)" rjust }
    ifelse(eval(m4x!=4),1,
     `dot; dot(at F`'m4x.PinCLR.end)',
     `line to (Serial,Here)
      "\scriptsize\sf\shortstack{\hfill PRESET\\ENABLE}" rjust ')
  ')

.PE
