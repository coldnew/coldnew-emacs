.PS
# `Variable.m4'
cct_init

define(`elen_',dimen_)
down_
[ { variable(`capacitor') }
  move right; down_
  { variable(`resistor',uN) }
  move right; down_
  { variable(`capacitor(,C)') }
  move right; down_
  { variable(`inductor') }
  move right; down_
  { variable(`inductor(,W)') }
  ]
[
  skp = 0.4
  hskip = linewid*0.5
  cskip = hskip
Orig: Here
  { move right cskip
    move right elen_; move up 0.15 then down 0.15 then right_ hskip
    line invis right_ elen_ "C"; move right_ hskip
    line invis right_ elen_ "S" }
  move down skp*0.5; right_
  { line invis right_ cskip "A"
    variable(`capacitor(,C)',A); move right_ hskip
    variable(`capacitor(,C)',AC); move right_ hskip
    variable(`capacitor(,C)',AS) }
  move down skp; right_
  { line invis right_ cskip "P"
    variable(`capacitor(,C)',P); move right_ hskip
    variable(`capacitor(,C)',PC); move right_ hskip
    variable(`capacitor(,C)',PS) }
  move down skp; right_
  { line invis right_ cskip "L"
    variable(`capacitor(,C)',L); move right_ hskip
    variable(`capacitor(,C)',LC); move right_ hskip
    variable(`capacitor(,C)',LS) }
  move down skp; right_
  { line invis right_ cskip "N"
    variable(`capacitor(,C)',N); move right_ hskip
    variable(`capacitor(,C)',NC); move right_ hskip
    variable(`capacitor(,C)',NS) }
  ] with .w at last [].e+(0.4,0)
.PE
