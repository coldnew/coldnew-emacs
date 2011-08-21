.PS

[
{box invis "input"; arrow; box invis "output"
 move
 line up "ljust text" ljust with .c at Here
 move right 1.5 from last line
 line up "rjust text" rjust with .c at Here}
move down; right
arrow "on top of"; move
arrow "above" "below"; move
arrow "above" above; move
arrow "below" below; move
arrow "above" "on top of" "below"
]

boxwid = 1.25
movewid = 0.35
define centerfold { {line thick 0.4 from last box.n to last box.s} }

[
box invis "PV" "Ts" ; centerfold
{ "\tt  \"PV\"  \"Ts\" " at last box.s below }
move
box invis "PV" rjust "Ts" ; centerfold
{ "\tt  \"PV\" rjust  \"Ts\" " at last box.s below }
move
box invis "PV" "Ts" rjust ; centerfold
{ "\tt  \"PV\"  \"Ts\" rjust " at last box.s below }
move
box invis "PV" rjust "Ts" rjust ; centerfold
{ "\tt  \"PV\" rjust  \"Ts\" rjust " at last box.s below }
] with .nw at last [].sw + (0,-0.5)

[
box invis "PV" "Ts" ; centerfold
{ "\tt  \"PV\"  \"Ts\" " at last box.s below }
move
box invis "PV" ljust "Ts" ; centerfold
{ "\tt  \"PV\" ljust  \"Ts\" " at last box.s below }
move
box invis "PV" "Ts" ljust ; centerfold
{ "\tt  \"PV\"  \"Ts\" ljust " at last box.s below }
move
box invis "PV" ljust "Ts" ljust ; centerfold
{ "\tt  \"PV\" ljust  \"Ts\" ljust " at last box.s below }
] with .nw at last [].sw + (0,-0.5)

[
move right 1.0
box invis "PV" ljust "Ts" rjust ; centerfold
{ "\tt  \"PV\" ljust  \"Ts\" rjust " at last box.s below }
move right 1.5
box invis "PV" rjust "Ts" ljust ; centerfold
{ "\tt  \"PV\" rjust  \"Ts\" ljust " at last box.s below }
] with .nw at last [].sw + (0,-0.5)


.PE
