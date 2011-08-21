dnl Circuit_macro customizations
ifdef(`m4picprocessor',,`include(/u/aplevich/lib/pstricks.m4)')dnl
ifdef(`cct_init',,`include(HOMELIB_`'libcct.m4)')dnl
define(`cct_init',`
 gen_init
 scale = 25.4                   # Coordinates in mm
                                # Set defaults to integer values of mm
 lineht = 13; linewid = 13; moveht = 13; movewid = 13
 arcrad = 7; circlerad = 7
 boxht = 13; boxwid = 19
 ellipseht = 13; ellipsewid = 19
 dashwid = 2; arrowht = 3; arrowwid = arrowht/2
')dnl
