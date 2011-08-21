divert(-1)
# `Xform.m4'
define(`trlabels',
 `{ thinlines_
    ifelse(`$1',R,
     `arrow from `$3'.P1 <- right 0.12 up 0.072 chop 1pt__ ; "P1" ljust
      arrow from `$3'.P2 <- right 0.12 down 0.072 chop 1pt__ ; "P2" ljust
      arrow from `$3'.TP <- right 0.12 chop 1pt__ ; "TP" ljust
      arrow from `$3'.S1 <- left 0.12 up 0.072 chop 1pt__ ; "S1" rjust
      arrow from `$3'.S2 <- left 0.12 down 0.072 chop 1pt__ ; "S2" rjust
      arrow from `$3'.TS <- left 0.12 chop 1pt__ ; "TS" rjust
     ',`
      arrow from `$3'.P1 <- left 0.12 up 0.072 chop 1pt__ ; "P1" rjust
      arrow from `$3'.P2 <- left 0.12 down 0.072 chop 1pt__ ; "P2" rjust
      arrow from `$3'.TP <- left 0.12 chop 1pt__ ; "TP" rjust
      arrow from `$3'.S1 <- right 0.12 up 0.072 chop 1pt__ ; "S1" ljust
      arrow from `$3'.S2 <- right 0.12 down 0.072 chop 1pt__ ; "S2" ljust
      arrow from `$3'.TS <- right 0.12 chop 1pt__ ; "TS" ljust
     ')
    `"'`$2'`"' at `$3'.s+(0,-(`$4'))
  thicklines_ } ')
divert(0)dnl
.PS
cct_init

move right 0.5
  down_
  T1: transformer
      trlabels(,`transformer',T1,0.3)
  T2: transformer(down_ 0.6,,2,,8) with .w at T1.e+(1,0)
      trlabels(,`...(down_ 0.6,,2,,8)',T2,0.3)
  T3: transformer(,,8,W,4) with .w at T2.e+(1,0)
      trlabels(,`...(,,8,W,4)',T3,0.23)
  T4: transformer(,,9,AL) with .sw at T3.se+(1,0)
      trlabels(,`...(,,9,AL)',T4,0.3)
  T5: transformer(,R,8,AW) with .sw at T4.se+(1,0)
      { move right 1}
      trlabels(R,`...(,R,8,AW)',T5,0.3)

.PE
