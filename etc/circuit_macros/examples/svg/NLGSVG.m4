.PS
# NLG.m4
cct_init

  define(`nullor',`nport(`$1'
    {`"0"' at Box.w ljust
     `"oo"' at Box.e rjust},shift($@))')

N:    nullor
`"nullor"' at last [].s+(0,-3pt__) below
G:    gyrator with .sw at N.se+(0.5,0)
`"gyrator"' at G.s+(0,-3pt__) below
I:    gyrator(invis,,0,N) with .w at G.e+(0.5,0)
`"gyrator(invis,,0,N)"' at I.s
V:    gyrator(invis wid boxht,,0,NV) with .w at I.e+(1,0)
`"gyrator(invis wid boxht,,0,NV)"' wid 2 at V.s+(0,-3pt__) below

  move right 0.9
.PE
