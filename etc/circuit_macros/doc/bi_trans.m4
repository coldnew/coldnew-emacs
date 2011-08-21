% `bi_trans.m4'
.PS
sinclude(CMman.dim)
s_init(bitrans)
ifdef(`bitrans_1',,`sinclude(tst.dim)')

cct_init

[[
Q: bi_trans(,,BCuEBUS)
  s_box($C$) at Q.C below
  s_box($B$) at Q.B above
  s_box($E$) at Q.E below

thinlines_
M4_xyO: Q.M4_xyO
  spline <- from tr_xy(0,5.25) left 0.1 then up 0.15 left 0.15 
    s_box(\tt B) above
  spline <- from 0.5<tr_xy(3,0),tr_xy(1.2,4)> right 0.1 then right 0.15 up 0.15 
    s_box(\tt C) ljust
  spline <- from 0.7<tr_xy(-2,4),tr_xy(2,4)> up 0.1 then right 0.15 up 0.15
   s_box(\tt BU) above
  spline <- from 0.7<tr_xy(-3,0),tr_xy(-1.2,4)> left 0.1 then left 0.15 up 0.15
   s_box(\tt uE) rjust
  arrow <- from Q.Bulk.start+vec_(vscal_(m4_U,0,0.5)) up 0.15 left 0.15
    s_box(\tt S) above rjust
  arrow <- from Q.Bulk.end up 0.1 right 0.1
    s_box($\;\;$\tt S) above
  ]
  s_box(\tt bi\_trans(,,BCuEBUS)) at last [].s below
]

[[
Q2: bi_trans(,,BCdE2BU)
  s_box($C$) at Q2.C below
  s_box($B$) at Q2.B above

thinlines_
M4_xyO: Q2.M4_xyO
  arrow <- from Q2.E0 down 0.1 right 0.1; s_box($\;\;E0$) below
  arrow <- from Q2.E2 down 0.1 left 0.1; s_box($E2\;\;$) below
  arrow <- from Q2.E1 down 0.1 ; s_box($E1$) below
  spline <- from 0.4<Q2.Em2.start,Q2.Em2.end> left 0.1 then up 0.15 left 0.15 
    s_box($Em2\;\;$) above
  ]
  s_box(\tt bi\_trans(,,BCdE2BU)) at last [].s below
] with .nw at last [].ne+(0.5,0)

[[
Q2: bi_trans(,,BC2dEBU)
  s_box($E$) at Q2.E below
  s_box($B$) at Q2.B above

thinlines_
M4_xyO: Q2.M4_xyO
  arrow <- from Q2.C0 down 0.1 left 0.1; s_box($C0\;\;$) below
  arrow <- from Q2.C2 down 0.1 right 0.1; s_box($\;\;C2$) below
  arrow <- from Q2.C1 down 0.1 ; s_box($C1$) below
  spline <- from 0.4<Q2.Cm2.start,Q2.Cm2.end> right 0.1 then up 0.15 right 0.15 
    s_box($Cm2\;\;$) above
  ]
  s_box(\tt bi\_trans(,,BC2dEBU)) at last [].s below
] with .nw at last [].ne+(0.5,0)

.PE
