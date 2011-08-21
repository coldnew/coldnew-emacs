.PS
# ShiftR.m4

ifdef(`FlipFlopJK',,`include(HOMELIB_`'liblog.m4)')
log_init
define(`lg_plen',3)

  textht = textht*0.9

define(`customFF',
 `[ Chip: box wid 10*L_unit ht FF_ht*L_unit
    lg_pin(Chip.w,CK,PinCK,wEN)
    lg_pin(Chip.n,PR,PinPR,nN)
    lg_pin(Chip.ne-svec_(0,int(FF_ht/4)),Q,PinQ,e)
    ifelse(`$1',1,
     `lg_pin(Chip.se+svec_(0,int(FF_ht/4)),lg_bartxt(Q),PinNQ,e)')
    lg_pin(Chip.s,CLR,PinCLR,sN)
    lg_pin(Chip.sw+svec_(0,int(FF_ht/4)),R,PinR,w)
    lg_pin(Chip.nw-svec_(0,int(FF_ht/4)),S,PinS,w)
  ]')

  F0: customFF(1)

  BUFFER_gate(,N) with .Out at F0.PinS.end
    line left 2*L_unit from last [].In1
  T: dot
    reversed(`NOT_gate',left 10*L_unit)
    {move left 0.7}
  Serial: "SERIAL" rjust "INPUT" rjust
    line from T to (T,F0.PinR) then to F0.PinR.end

  NCLR: NOT_gate(right T.x-Serial.x from (Serial,F0.PinCLR.end),N)
    "CLEAR" at NCLR.start rjust 
    line left textht*3.0 from last ""+(-0.3,0.5)*textht

  CLK: NOT_gate(right T.x-Serial.x from NCLR.start+(0,-BUF_ht*3/2*L_unit))
    "CLOCK" at CLK.start rjust

for_(1,4,1,`
  F`'m4x: customFF(eval(m4x!=4)) with .PinR.end at F`'eval(m4x-1).PinNQ.end
  ')

  "OUTPUT" wid 0.7 at F4.PinQ.end ljust
  line from NCLR.end to F4.PinCLR.end

  down_
for_(0,4,1,`
  line from F`'m4x.PinCK.end down F0.PinCK.y-CLK.y
  ifelse(eval(m4x!=4),1,`dot',`line to CLK.end')
  N`'m4x: NAND_gate with .Out at F`'m4x.PinPR.end 
    line up 2*L_unit from N`'m4x.In2
    { line up 6*L_unit from N`'m4x.In1
      "PR`'eval(4-m4x)" rjust }
    ifelse(eval(m4x!=4),1,
     `dot; dot(at F`'m4x.PinCLR.end)',
     `line to (Serial,Here)
      "PRESET" rjust "ENABLE" rjust ')
  ')

.PE
