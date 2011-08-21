% `CctTable.m4'
.PS
cct_init
textwid = 1.5
movewid = 2 pt__
hm = 2.05
vm = 0.28
{  {resistor ; move ; "`{\tt resistor}'" ljust}
   move right_ hm
   {resistor(,,Q) ; move ; "`\tt resistor(,,Q) '" ljust}
   move right_ hm
   {resistor(,,E) ; move ; "`\tt resistor(,,E) $\equiv$ ebox'" ljust}
}
   move down vm; right_
{  {resistor(,,H) ; move ; "`\tt resistor(,,H) '" ljust}
   move right_ hm
   {resistor(,4,QR) ; move ; "`\tt resistor(,4,QR) '" ljust}
   move right_ hm
   {inductor ; move ; "`{\tt inductor}'" ljust}
}
   move down vm; right_
{  {inductor(,W); move ; "`{\tt inductor(,W)}'" ljust}
   move right_ hm
   {inductor(,L); move ; "`{\tt inductor(,L)}'" ljust}
   move right_ hm
   {inductor(,W,6,M); move ; "`{\tt inductor(,W,6,M)}'" ljust}
}
   move down vm; right_
{  {inductor(,,,M) ; move ; "`{\tt inductor(,,,M)}'" ljust}
   move right_ hm
   {ttmotor(,G) ; move ; "`{\tt ttmotor(,G)}'" ljust}
   move right_ hm
   {capacitor ; move ; "`{\tt capacitor}'" ljust}
}
   move down vm; right_
{  {capacitor(,C); move ; "`{\tt capacitor(,C)}'" ljust}
   move right_ hm
   {capacitor(,C+); move ; "`{\tt capacitor(,C+)}'" ljust}
   move right_ hm
   {capacitor(,P); move ; "`{\tt capacitor(,P)}'" ljust}
}
   move down vm; right_
{  {capacitor(,E); move ; "`{\tt capacitor(,E)}'" ljust}
   move right_ hm
   {capacitor(,K); move ; "`{\tt capacitor(,K)}'" ljust}
   move right_ hm
   {xtal ; move ; "`{\tt xtal}'" ljust}
}
   move down 0.25; right_
{  {gap ; move ; "`{\tt gap}'" ljust}
   move right_ hm
   {gap(,,A) ; move ; "`{\tt gap(,,A)}'" ljust}
   move right_ hm
   {arrowline ; move; "`{\tt arrowline}'" ljust}
}
   move down 0.25; right_
{  {ebox(,,,0.5) ; move ; "`{\tt ebox(,,,0.5)}'" ljust}
   move right_ hm
   {memristor ; move; "`{\tt memristor}'" ljust}
   move right_ hm
   {ebox(,0.5,0.3) ; move ; "`{\tt ebox(,0.5,0.3)}'" ljust}
}
   move down 0.25; right_
{  {tline ; move; "`{\tt tline}'" ljust}
}

.PE
