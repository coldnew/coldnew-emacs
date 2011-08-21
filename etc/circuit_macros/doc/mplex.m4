% mplex.m4
.PS
  ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')
  log_init

  define(`short',elen_*2/3)
  del = BUF_ht*L_unit
down_
P: NOR_gate(8)
B: P.In3+(0,del/2)
  dx = P.In1.x-P.In2.x
  line down del/2 from P.Out then right 2*del; dot
  {line up del*1.5; right_; BUFFER_gate(right_ short); "$\bar{Y}$" ljust }
  BUFFER_gate(right_ short,N) ; "$Y$" ljust
  for_(0,7,1,
   `line from P.In`'eval(8-m4x) \
      up B.y-P.In`'eval(8-m4x).y + (3.5-abs(m4x-3.5)) * dx \
      then left (3.5-m4x)*AND_wd*L_unit then up (0.5+abs(m4x-3.5))*dx; down_
    A`'m4x: AND_gate(5) with .Out at Here ')

Eb: "$\bar{E}$" rjust at (A0.In5.x-2*short-dx/2,A0.n.y+del/2)
  BUFFER_gate(from Eb right_ short,N); line to (A7.In1,Here) then to A7.In1
  for_(0,6,1,
   `line from A`'m4x.In1 to (A`'m4x.In1,Eb) ; dot')

  for_(0,2,1,
   `"`$S_'m4x$" rjust at Eb.x,Eb.y+(1+m4x)*del*2
    NOT_gate(right_ short)
    V1`'m4x: Here+(N_diam*L_unit/2,-del); {dot(at (V1`'m4x,Here))}
    NOT_gate(right_ short)
    V0`'m4x: Here')

  for_(0,7,1,`define(`i',m4x)dnl
    for_(0,2,1,`define(`j',m4x)define(`k',`eval(((i+2**j)/2**j)%2)')dnl
      move to A`'i.In`'eval(j+2)
      line up V`'k`'j.y - Here.y dnl
      ifelse(eval((i==7)|(i==(7-2**j))),1,
       `then to V`'k`'j ifelse(eval(k==1),1,`then up del')',
       `; dot')
      ')
    move to A`'i.In5; line to (Here,V02)
    reversed(`BUFFER_gate',up_ short)
    "$`I_'i$" at last line.end rjust
    ')

.PE
