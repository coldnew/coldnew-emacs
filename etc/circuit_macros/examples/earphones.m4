% test.m4
divert(-1)
define(`earphones',
 `[ L: circle diam dimen_*0.4
    R: circle diam dimen_*0.4 at L+vec_(dimen_,0)
    arc ifelse(`$2',R,c)cw \
      from L+(Rect_(dimen_*0.2,ifelse(`$2',R,-)45)) \
        to R+(Rect_(dimen_*0.2,ifelse(`$2',R,-)135)) \
        with .c at (0.5 between L and R) ]')
divert(0)dnl
.PS
A: earphones
B: earphones(,R) with .n at A.s+(0,-0.25)
.PE
