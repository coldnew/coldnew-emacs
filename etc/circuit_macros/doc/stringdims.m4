% stringdims.m4
.PS
sinclude(CMman.dim)
s_init(stringdims)
[B: box
  s_box(`Left text') at B.w rjust
  s_box(`Right text: $x^%g$',2) at B.e ljust
  ]
  box dotted ht last [].ht wid last [].wid at last []
.PE

