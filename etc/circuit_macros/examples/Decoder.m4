% Decoder.m4
.PS
ifdef(`NAND_gate',,`include(HOMELIB_`'liblog.m4)')
log_init

  define(`nlines',3) # OK for nlines from 1 to approximately 4

  define(`ndata',`eval(2**nlines-1)')
  for_(0,nlines-1,1,
   `{right_; A`'m4x: dot(at rsvec_(0,m4x*AND_ht))
     "$A_{m4x}$" at last [].w rjust}')

  for_(0,ndata,1,
   `define(`m4y',m4x) down_
    G`'m4y: AND_gate(nlines) \
      at A0+svec_(AND_ht*3/2,(m4y+1/2)*AND_ht*3/2)
     line down linewid/3; dot; "$\;D_{m4y}$" at last [].s below
    for_(0,nlines-1,1,
     `ifelse(eval(m4y/2**m4x%2),0,
      `up_; NOT_circle with .s at',`move to') G`'m4y.In`'eval(nlines-m4x)
       line to (Here,A`'m4x) ifelse(m4y,ndata,`then to A`'m4x',`; dot')
     ')
   ')

.PE
