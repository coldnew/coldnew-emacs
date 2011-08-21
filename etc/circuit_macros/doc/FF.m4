% `FF.m4'
.PS
log_init
S: NOR_gate
  left_
R: NOR_gate at S+(0,-L_unit*(AND_ht+1))
  line from S.Out right L_unit*3 then down S.Out.y-R.In2.y then to R.In2
  line from R.Out left L_unit*3 then up S.In2.y-R.Out.y then to S.In2
  line left 4*L_unit from S.In1 ; "$S$sp_" rjust
  line right 4*L_unit from R.In1 ; "sp_$R$" ljust
.PE
