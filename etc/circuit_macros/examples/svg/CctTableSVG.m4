.PS
# `CctTable.m4'
cct_init
textwid = 1.5
movewid = 2 pt__
hm = 2.05
vm = 0.28
{  {resistor ; move ; svgLink(NportSVG.svg,"`resistor'" ljust)}
   move right_ hm
   {resistor(,,Q) ; move ; "`resistor(,,Q) '" ljust}
   move right_ hm
   {resistor(,,E) ; move ; "`resistor(,,E) = ebox'" ljust}
}
   move down vm; right_
{  {resistor(,,H) ; move ; "`resistor(,,H) '" ljust}
   move right_ hm
   {resistor(,4,QR) ; move ; "`resistor(,4,QR) '" ljust}
   move right_ hm
   {inductor ; move ; "`inductor'" ljust}
}
   move down vm; right_
{  {inductor(,W); move ; "`inductor(,W)'" ljust}
   move right_ hm
   {inductor(,L); move ; "`inductor(,L)'" ljust}
   move right_ hm
   {inductor(,W,6,M); move ; "`inductor(,W,6,M)'" ljust}
}
   move down vm; right_
{  {inductor(,,,M) ; move ; "`inductor(,,,M)'" ljust}
   move right_ hm
   {ttmotor(,G) ; move ; "`ttmotor(,G)'" ljust}
   move right_ hm
   {capacitor ; move ; "`capacitor'" ljust}
}
   move down vm; right_
{  {capacitor(,C); move ; "`capacitor(,C)'" ljust}
   move right_ hm
   {capacitor(,C+); move ; "`capacitor(,C+)'" ljust}
   move right_ hm
   {capacitor(,P); move ; "`capacitor(,P)'" ljust}
}
   move down vm; right_
{  {capacitor(,E); move ; "`capacitor(,E)'" ljust}
   move right_ hm
   {capacitor(,K); move ; "`capacitor(,K)'" ljust}
   move right_ hm
   {xtal ; move ; "`xtal'" ljust}
}
   move down 0.25; right_
{  {gap ; move ; "`gap'" ljust}
   move right_ hm
   {gap(,,A) ; move ; "`gap(,,A)'" ljust}
   move right_ hm
   {arrowline ; move; "`arrowline'" ljust}
}
   move down 0.25; right_
{  {ebox(,,,0.5) ; move ; "`ebox(,,,0.5)'" ljust}
   move right_ hm
   {memristor ; move; "`memristor'" ljust}
   move right_ hm
   {ebox(,0.5,0.3) ; move ; "`ebox(,0.5,0.3)'" ljust}
}
   move down 0.25; right_
{  {tline ; move; "`tline'" ljust}
}

.PE
