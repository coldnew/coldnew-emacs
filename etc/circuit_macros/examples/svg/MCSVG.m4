.PS
#MC.m4
cct_init

  dv = dimen_*0.6
  dh = dimen_*0.4
  backup = dimen_/6

  for i=1 to 3 do {{
    if i==2 then { Input: dot } else { line up (2-i)*dv }
    source(right_ dimen_,AC)
#   The pound sign has to be protected from m4
    llabel(,,sprintf("svg_it(v`'svg_sub(&``#''%g;,66%))",96+i))
    resistor(right_ dimen_ from Here-(backup/2,0))
    if i==1 then { llabel(,R`'svg_sub(i))}
    inductor(right_ dimen_ from Here-(backup,0),W)
    if i==1 then { llabel(,L`'svg_sub(i))}
    line right_ i*dh
    b_current(sprintf("svg_it(i`'svg_sub(&``#''%g;,66%))",96+i)); dot
    { line down (3-i)*dv; capacitor(down_ dimen_)
      if i==1 then { rlabel(,C`'svg_it(i)) }
      if i==2 then { dot } else { line right (2-i)*dh }}
    line right 3*dh; dot
    E: Here+((3-i)*dh+dimen_*3/2,(4-2*i)*dv)
    for j=1 to 3 do {{
      line up (6-j*3)*dv then right (3-i)*dh
      switch(right_ E.x-Here.x)
      llabel(,,sprintf("svg_it(S`'svg_sub(&``#''%g;&``#''%g;,66%))",96+i,64+j))
      if i==2 then { dot } else { line down (2-i)*dv }
      }}
    line from E right dh then down E.y-Here.y
    arrow right dimen_/2
    sprintf("svg_it(i`'svg_sub(&`#'%g;,66%))",64+i) above
    resistor(right_ dimen_ from Here-(arrowht/2,0))
    if i==1 then {llabel(,R`'svg_sub(o))}
    inductor(right_ dimen_ from Here-(backup,0),W)
    if i==1 then {llabel(,L`'svg_sub(o))}
    source(right_ dimen_,AC)
    llabel(sprintf("svg_it(v`'svg_sub(&``#''%g;,66%))",64+i))
    if i==2 then { Output: dot } else { line down (2-i)*dv }
    }}
.PE
