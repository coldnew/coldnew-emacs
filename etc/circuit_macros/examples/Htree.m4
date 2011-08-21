% Htree.m4
% One way to draw a horizontal binary tree using m4 macros
.PS
gen_init
circlerad = 0.35

# Node definiton, with all arguments optional except the first:
#    node( name, "content", left child name, right child name,
#       left position adjustment, right position adjustment )
# If "content" is nil use "name" as content
# A tree is enclosed in [ ] brackets, with the root block named N

define(`node',`define(`$1',`[ N: s_box(ifelse(`$2',,"``$1''",``$2''))
   ifdef(`$3',`$3 with .nw at N.e+(boxwid/5,0) `$5'
    line from last [].nw to last [].w
    ')dnl
  ifdef(`$4',`$4 with .sw at N.e+(boxwid/5,0) `$6'
    line from last [].sw to last [].w
    ')]')')

# Redrawn from T. A. Standish, "Data Structure Techniques," Addison-Wesley 1980.
# Node data:
node(Sirius,,Canopus,Vega)
node(Canopus,,AlphaCentauri,Capella)
node(AlphaCentauri,"Alpha" "Centauri",Achernar,Arcturus)
node(Arcturus,,Betelgeux)
node(Betelgeux,,BetaCentauri)
node(Capella,,Rigel)
node(Rigel,,Procyon)
node(Achernar)
node(BetaCentauri,"Beta" "Centauri")
node(Procyon)
node(Vega)

# Now build the tree from the root
Sirius
.PE

