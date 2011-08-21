% `eboxdims.m4'
.PS
sinclude(CMman.dim)  # The main input file is CMman.tex
box fill_(0.9) wid boxdim(Q,w) + 5pt__ ht boxdim(Q,v) + 5pt__ \
  "\boxdims{Q}{\large$\displaystyle\int_0^T e^{tA}\,dt$}"
thinlines_
  s_init(eboxdims)
  d2 = 5pt__/2
  textoffset = 2 pt__
  dimension_(from last box.sw to last box.se chop d2, -0.1,
    s_box(\tt Q\_w),W,2pt__)
  dimension_(from last box.se to last box.ne chop d2, -0.1,
    s_box($\!\!\!$\tt Q\_h${+}$Q\_d) ljust,H,2pt__)
.PE
