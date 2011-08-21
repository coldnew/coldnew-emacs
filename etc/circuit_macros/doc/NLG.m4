% NLG.m4
.PS
cct_init

  define(`nullor',`nport(`$1'
    {`"${}0$"' at Box.w ljust
     `"$\infty$"' at Box.e rjust},shift($@))')

N:    nullor
`"\tt nullor"' at last [].s+(0,-3pt__) below
G:    gyrator with .sw at N.se+(0.5,0)
`"\tt gyrator"' at G.s+(0,-3pt__) below
I:    gyrator(invis,,0,N) with .w at G.e+(0.5,0)
`"\tt gyrator(invis,,0,N)"' at I.s
V:    gyrator(invis wid boxht,,0,NV) with .w at I.e+(1,0)
`"\tt gyrator(invis wid boxht,,0,NV)"' at V.s+(0,-3pt__) below

  move right 0.9
.PE
