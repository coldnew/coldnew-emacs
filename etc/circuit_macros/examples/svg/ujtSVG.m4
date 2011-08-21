.PS
# `ujt.m4'
cct_init
dwn = 1.0
dsep = 0.15

define(`demo_ujt',`up_
  ujt(`$1',`$2',`$3',`$4')
  "B1" ifelse(`$5',,below,ljust) at last [].B1
  "E" at 1.5 between last [].Bulk.c and last [].E
  "B2" ifelse(`$5',,above,ljust) at last [].B2
 ')

[demo_ujt(up_ dimen_,,,E,A)
`"ujt(up_ dimen_,,,E)"' wid 1.3 at last [].s+(0,-0.2) below ]

[demo_ujt(,,P,)
`"ujt(,,P,)"' at last [].s+(0,-dsep) below] \
with .sw at last [].se+(0.8,0)

[demo_ujt(,R,,)
`"ujt(,R,,)"' at last [].s+(0,-dsep) below] \
with .sw at last [].se+(0.8,0)

[demo_ujt(,R,P,)
`"ujt(,R,P,)"' at last [].s+(0,-dsep) below] \
with .sw at last [].se+(0.8,0)

.PE
