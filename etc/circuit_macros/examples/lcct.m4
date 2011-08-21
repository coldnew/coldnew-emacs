% lcct.m4
.PS 8.5
ifelse(ifpostscript(T)ifxfig(T)ifroff(T)ifmpost(T),,`command "{\sf"')
include(HOMELIB_`'liblog.m4)
log_init
include(ics.m4)

define(`dimen_',0.5)
define(`elen_',dimen_)
define(`Groundtype',`')

define(`breakout',` for i = 1 to `$1' do {
      {line `$2' ifelse(`$4',,del,`$4')-jog/2 then `$2' jog/2 `$3' jog \
         then `$3' lg_pinsep}
      move `$3' lg_pinsep }
      move `$2' ifelse(`$4',,del,`$4')-jog/2 then `$2' jog/2 `$3' jog ')

del = lg_pinsep
jog = del*2/3

   right_
IC1: ic6502

Vcc: IC1.Pin8.end.x-del-elen_,IC1.Pin4.y+lg_pinsep+3*del
Reset: IC1.Pin8.end.x-del,Vcc.y+del

   resistor(up_ Vcc.y-IC1.Pin4.y from IC1.Pin4.end)

   line left 2*del from IC1.Pin6.end
   line up_ to (Here,Vcc) chop 0 chop elen_
   resistor

   line left del from IC1.Pin40.end
   {line to (Here,Reset)}
   move left elen_
   {NOT_gate(right_ elen_)}
   {reversed(`capacitor',to (Here,Vcc),C)}
   down_; resistor; ground(,T,Groundtype)

   line from IC1.Pin2.end to (IC1.Pin2.end,Vcc)

   move to IC1.Pin26.end; breakout(8,left,down)
   line to (Here,IC1.sw+(0,-del)) 
   line down jog right jog/2
Data: Here

   line from IC1.Pin1.end down_ 2*lg_pinsep; ground(,T,Groundtype)

   line left 2*del from IC1.Pin34.end
   line to (Here,Data+(0,-del))
RWB: Here

   move to IC1.Pin25.end; breakout(16,right,down)
   line to (Here,RWB)+(0,-del+jog) then down jog left jog/2
Addr: Here
   
   right_
IC5: ic74LS138 with .Chip.nw at (IC1.Chip.w.x,Addr.y-2*del)
   line from IC5.Pin5.end down_ 3*lg_pinsep ; ground(,T,Groundtype)
   line from IC5.Pin16.end down_ 1.5*lg_pinsep ; ground(,T,Groundtype)
   move to IC5.Pin1.end; breakout(3,left,up)
   line to Here.x,Addr.y-jog then up jog right jog/2 then to Addr

   line right_ 2*del from IC1.Pin39.end
   { NOT_gate(right_ 3*del)
C1: Here }
   capacitor(up_ elen_*0.6)
   {resistor(right_ 3*del)}
   line up 2*(IC1.Pin37.y-Here.y)
   {line from IC1.Pin37.end to (Here,IC1.Pin37)}
   right_; xtal(right_ 3*del)

IC2: ic6116 with .Chip.nw at Here.x+2*del+lg_plen*L_unit,IC1.Chip.n.y
   line from IC2.Pin24.end up to (IC2.Pin24.end,Vcc)
   right_
Or1: OR_gate with .Out at IC2.Pin21.end
   line from Or1.In2 left del then down del
   line to (C1,Here) then to C1
   line from Or1.In1 left del
   line to (Here,IC2.Chip.nw+(0,del))
RW: Here
   move to IC2.Pin17.end; breakout(8,left,down)
   line to (Here,Data)+(0,jog) then down jog left jog/2
   line down_ lg_pinsep from IC2.Pin12.end ; ground(,T,Groundtype)
   move to IC2.Pin19.end; breakout(11,right,down)
   line to (Here,Addr) chop 0 chop jog
   line down jog left jog/2
   line down_ lg_pinsep from IC2.Pin20.end ; ground(,T,Groundtype)

   right_
IC3: ic6116 with .Chip.nw at IC2.Chip.ne + (IC2.Chip.nw.x - IC1.Chip.ne.x-del,0)
IC4: ic6522 with .Chip.nw at IC3.Chip.ne + (IC2.Chip.nw.x - IC1.Chip.ne.x-del,0)
IC6: ic74LS138 with .Chip.nw at (IC3.Chip.nw,IC5.Chip.nw)

   line from Vcc to (IC4.Pin34.end.x+del,Vcc.y)
   line from Reset to (IC4.Pin34.end,Reset) then to IC4.Pin34.end

   line right_ 2*del from (C1,IC4.Pin25); NOT_gate; line to IC4.Pin25.end
   line right del from IC1.Pin4.end; line to Here.x,Vcc.y-del
   line to (IC4.Pin21.end,Here)-(del,0) then to IC4.Pin21.end-(del,0) \
      then to IC4.Pin21.end

   line from IC3.Pin21.end to (IC3.Pin21.end,Vcc)
   move to IC3.Pin17.end
   breakout(8,left,down); line to (Here,Data)+(0,jog) then down jog left jog/2
   move to IC3.Pin19.end
   breakout(11,right,down); line to (Here,Addr)+(0,jog) then down jog left jog/2
   move to IC4.Pin26.end
   breakout(8,left,down,2*del);
   line to (Here,Data)+(0,jog) then down jog left jog/2 then to Data
   move to IC4.Pin35.end
   breakout(4,left,down); line to (Here,Addr)+(0,jog) then down jog left jog/2 \
      then to Addr
   line from IC4.Pin20.end to (IC4.Pin20.end,Vcc)
   line down_ lg_pinsep from IC4.Pin1.end; ground(,T,Groundtype)
   line left 3*del from IC4.Pin23.end
   line to (Here,IC6.Pin7) then to IC6.Pin7.end
   line down_ 2*lg_pinsep from IC3.Pin18.end; ground(,T,Groundtype)

   line down_ lg_pinsep from IC6.Pin8.end; ground(,T,Groundtype)
   line up_ 2.5*lg_pinsep from IC6.Pin16.end \
     then right_ lg_pinsep then down_ lg_pinsep/2; ground(,T,Groundtype)
   move to IC6.Pin5.end; breakout(1,left,up); line up_ lg_pinsep
   move to IC6.Pin1.end; breakout(3,left,up)
   line to (Here,Addr)-(0,jog) then up jog left jog/2

   line from RW to (IC3.Pin19.end+(2*del,0),RW)
RWE: Here
   line to (RWE,RWB) then to RWB
   line from IC4.Pin22.end to (RWE,IC4.Pin22.end)

   right_
Or3: OR_gate with .Out at IC3.Pin20.end.x-del,IC1.Pin9.y
   line from Or3.Out to (IC3.Pin20.end,Or3.Out) then to IC3.Pin20.end
   left_
Or2: OR_gate with .In2 at (IC2.Chip.se,Or3.In1)
   line from Or2.Out to (IC2.Pin18.end-(3*del,0),Or2.Out)
   line to (Here,IC2.Pin18) then to IC2.Pin18.end
   line from Or2.In1 right del; line to (Here,IC1.Pin37)

   line from Or3.In2 left del; line to (Here,IC1.Pin37)

define(`addrlabel',`dnl
   line invis right 4*del from IC5.Pin`$1'.end "{\tiny (`$2')}" above ')

   addrlabel( 7,E000 - FFFF)
   addrlabel( 9,C000 - DFFF)
   addrlabel(10,A000 - BFFF)
   addrlabel(11,8000 - 9FFF)
   addrlabel(12,6000 - 7FFF)
   addrlabel(13,4000 - 5FFF)
   addrlabel(14,2000 - 3FFF)
   addrlabel(15,0000 - 1FFF)

   line from IC5.Pin7.end right 4*del
K: Here; line to (K,IC5.Pin13)
   line to (IC6.Pin4.end-(3*del,0),Here)
   line to (Here,IC6.Pin4.end) then to IC6.Pin4.end

Ands: [ right_
   And1: AND_gate; line right del/2 then down del*3/2 \
      then left And1.Out.x-And1.In1.x+del then down del then right_ del/2
   And2: AND_gate with .In1 at Here
      line from And2.Out right del/2 then down del then right_ del/2
   And3: AND_gate with .In1 at Here
      line right_ del/2 from And3.Out 
      NOT_gate
      line right_ del/2
   And4: AND_gate with .In1 at Here
   ] with .And2.In1 at (K.x+2*del,IC5.Pin9.y)

#  line from Ands.And4.Out right del*3; line to (Here,Or3.In1) then to Or3.In1
   line from Ands.And4.Out right IC6.Pin5.end.x-3*del-Ands.And4.Out.x
      line to (Here,Or3.In1) then to Or3.In1
   line from Ands.And4.In2 left del
   line to (Here,IC5.Pin14) then to IC5.Pin14.end
   line from IC5.Pin15.end to (IC2.Pin20.end.x+2*del,IC5.Pin15.y)
   line to (Here,Or2.In2) then to Or2.In2
   NOT_gate(right_ from (K,Ands.And3.In2) to Ands.And3.In2)
   line from Ands.And2.In2 to (K+(del+jog/2,0),Ands.And2.In2)
   line left jog/2 up jog; line to (Here.x,Addr.y-jog) then up jog left jog/2
   line from Ands.And1.In1 to (K.x+del+jog/2,Ands.And1.In1.y) \
      then left jog/2 up jog
   line from Ands.And1.In2 to (K.x+del+jog/2,Ands.And1.In2.y) \
      then left jog/2 up jog
   
   line from IC6.Pin6.end left 2*del; line to (Here,IC4.Pin24)
   line from IC4.Pin24.end to (IC2.Chip.se,IC4.Pin24)
   move left dimen_; {NOT_gate(right_ dimen_)}
   line left jog/2 down jog; line to (Here,Data) chop 0 chop jog
   line down jog left jog/2

ifelse(ifpostscript(T)ifxfig(T)ifroff(T)ifmpost(T),,`command "}%"')
.PE
