% `Tline.m4'
.PS
cct_init
hgt = elen_*1.5
ewd = dimen_*0.9
define(`sresistor',`resistor(right_ ewd); llabel(,r)')
define(`sinductor',`inductor(right_ ewd,W); llabel(,L)')
define(`tsection',`sinductor
  { dot; line down_ hgt*0.25; dot
    gpar_( resistor(down_ hgt*0.5); rlabel(,R),
          capacitor(down_ hgt*0.5); rlabel(,C))
    dot; line down_ hgt*0.25; dot }
  sresistor ')

SW: Here
  gap(up_ hgt)
  sresistor
  for i=1 to 4 do { tsection }
  line dotted right_ dimen_/2
  tsection
  gap(down_ hgt)
  line to SW
.PE
