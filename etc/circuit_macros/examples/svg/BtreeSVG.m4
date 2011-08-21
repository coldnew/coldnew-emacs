.PS
# Btree.m4
# One way to draw a binary tree using pic macros
gen_init
circlerad = 0.35

define n { [C: circle fill_(0.9) $1
            if "$2" != "" then {
              L: $2 with .ne at C.s-(circlerad/20, circlerad/2)
              line from C to L.C chop }
            if "$3" != "" then {
              R: $3 with .nw at C.s+(circlerad/20,-circlerad/2)
              line from C to R.C chop }
            ] }

# Redrawn from T. A. Standish, "Data Structure Techniques," Addison-Wesley 1980.
# Node data:
define Sirius { n("Sirius",Canopus,Vega) }
define Canopus { n("Canopus",AlphaCentauri,Capella) }
define AlphaCentauri { n("Alpha" "Centauri",Achernar,Arcturus) }
define Arcturus { n("Arcturus",Betelgeux) }
define Betelgeux { n("Betelgeux",BetaCentauri) }
define Capella { n("Capella",Rigel) }
define Rigel { n("Rigel",Procyon) }
define Achernar { n("Achernar") }
define BetaCentauri { n("Beta" "Centauri") }
define Procyon { n("Procyon") }
define Vega { n("Vega") }

# Build the tree from the root
S: Sirius
.PE
