.PS
# ex08.m4
cct_init
ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')

define(`del',`L_unit*5/2')
ifelse(ifmpost(T)`'ifpostscript(T),,`command "{\sf"')

# Input labels
C:  "C" rjust at grid_(0,0)
DE: C+grid_(0,AND_ht*7/4)
A0: "A0" rjust at DE+grid_(0,BUF_ht*3/2)
A1: "A1" rjust at A0+grid_(0,BUF_ht*3/2)
A2: "A2" wid 0.2 rjust at A1+grid_(0,BUF_ht*3/2)
  move to (-0.2,0)   # Lettering within the global object 

# Buffer strings
for_(1,3,1,
  `line right 3*del from A`'eval(3-m4x)
   B`'eval(2*m4x-1): NOT_gate; line right 2*del
   T`'m4x: last line.c
   B`'eval(2*m4x): BUFFER_gate(,N) ')

B7: BUFFER_gate at (B1,C)

G1: NOR_gate with .Out at (B1.Out,DE)
G2: NOR_gate at (B2+grid_(N_rad,0),G1-grid_(0,AND_ht))
G3: AND_gate(4) with .In1 at (G2.Out+(9*del,0),A2)
G5: AND_gate(4) with .In4 at (G3.In1,G2.Out)
G6: AND_gate at G5+grid_(AND_wd,AND_ht*3/4)+(del*2,0)
G4: NOR_gate with .In1 at (G6.Out,G3)+(2*del,0)
G7: NOR_gate with .In2 at (G4.In2,G5.Out)

# Output to other latches
TOL: (G2.Out,C)+(4*del,-4*L_unit)
  move to TOL+(3*del,-del/3) ; {move down 0.15}
  "To other latches" below

# Remaining input lines
  line from G1.In1 to (DE,G1.In1); "D" rjust
  line from G1.In2 to (DE,G1.In2); "E" rjust
  dot(at G1.In2-(del,0)); line to (Here,G2.In2) then to G2.In2
  line from C to B7.In1

# Connect Bi, and connect to output bus
for_(1,3,1,
 `dot(at T`'m4x.c) ; line down (A2.y-A1.y)/2
  line to (TOL-(m4x*del,0),Here) ; arrow to (Here,TOL) ')

# G1 to G2
  dot(at (T1.c,G1)); line to (Here,G2.In1) then to G2.In1

# Upper And inputs to output bus
for_(1,4,1,
 `line left eval(5-m4x)*del from G3.In`'m4x
  arrow to (Here,TOL) ')

  line from B2.Out to (G3.In1,B2)-(4*del,0) ; dot
  line from B4.Out to (G3.In2,B4)-(3*del,0) ; dot
  line from B6.Out to (G3.In3,B6)-(2*del,0) ; dot
  line from G1.Out to (G3.In4,G1)-(del,0) ; dot

# Lower And inputs
  line left 4*del from G5.In1 ; dot
  line left 3*del from G5.In2 ; dot
  line left 2*del from G5.In3 ; dot
  line from G2.Out to G5.In4 ; dot(at (TOL,Here)) ; arrow to (Here,TOL)

# Fix up G4, G6, G7
  line from G3.Out to G4.In1
  line right del from G4.Out
  {dot; line right del
   "Q7" wid 0.2 ljust; move right 0.2 } # letters in object
  line down 2*del then to G6.In1+(-del,del)
    line to (Here,G6.In1) then to G6.In1
  line from G4.In2 to (G6.In1+(-del,0),G4.In2)
    line down del then to G7.Out+(del,2*del)
    line to (Here,G7) then to G7.Out
  line from G6.Out right del ; line to (Here,G7.In1) then to G7.In1
  line from G5.Out to G7.In2
  line left del from G6.In2 ; arrow to (Here,TOL)
  dot(at (Here,C)) ; line to B7.Out

ifelse(ifmpost(T)`'ifpostscript(T),,`command "}"')
.PE
