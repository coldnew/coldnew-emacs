.PS
# sixpole.m4
define(`biquad',`[
In: Here
  resistor(right_)
E: dot
  resistor
  dot
  {capacitor(down_,C); ground(,T)}
  line right_ elen_/3
  { dot
    line up_ elen_
    amp(to (E,Here),elen_/2)
    capacitor(from E to Here,C) }
  amp(right_,elen_/2)
  Out: Here
  ]')

cct_init
define(`elen_',linewid)

  biquad
  biquad with .In at last [].Out+(-linewid/4,0)
  biquad with .In at last [].Out+(-linewid/4,0)
.PE
