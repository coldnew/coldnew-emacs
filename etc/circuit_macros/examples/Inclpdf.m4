.PS
#`Inclpdf.m4 : Illustrating one way to import and overwrite graphics'

include(HOMELIB_`'darrow.m4)
s_init(Inclpdf)
sinclude(FInclpdf.dim)

define(`backarrow',`darrow(`$1',,,2pt__,6pt__,6pt__,<-|)')

#                           A blank box the size of the included pdf figure:
S: s_box("\hskip %gin\vrule width 0pt height %gin",\
   boxdim(InclA,w),boxdim(InclA,v))

#                           Annotate the included figure:
thinlines_
  backarrow( from boxcoord(S,0.4,0.95) up 0.4 right 0.7 )
    s_box(\sf Basket) ljust
  backarrow( from boxcoord(S,0.9,0.41) up 0.2 right 0.5 )
    s_box(\sf Ball) ljust
  backarrow( from boxcoord(S,0.85,0.32) up 0.2 right 0.5 )
    s_box(`\sf\shortstack[l]{Star\\ player}') ljust
.PE
