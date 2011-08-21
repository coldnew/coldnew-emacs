.PS
# `Diodes.m4'
cct_init
textwid = 0.7
movewid = 2pt__
{  {diode ; move ; "`diode'" ljust}
   move down 0.2; right_
   {diode(,S) ; move ; "`diode(,S)'" ljust}
   move down 0.2; right_
   {diode(,V) ; move ; "`diode(,V)'" ljust}
   move down 0.2; right_
   {diode(,v) ; move ; "`diode(,v)'" ljust}
   move down 0.2; right_
   {diode(,B) ; move ; "`diode(,B)'" ljust}
   move down 0.2; right_
   {diode(,CR) ; move ; "`diode(,CR)'" ljust}
   }

   move right_ 2.0
{  {diode(,K) ; move ; "`diode(,K)'" ljust}
   move down 0.2; right_
   {diode(,L) ; move ; "`diode(,L)'" ljust}
   move down 0.2; right_
   {diode(,D) ; move ; "`diode(,D)'" ljust}
   move down 0.35; right_
   {diode(,LE) ; move ; "`diode(,LE)'" ljust}
   }

   move right_ 2.0
{  {diode(,Z,RE) ; move ; "`diode(,Z,RE)'" wid 1 ljust}
   move down 0.4; right_
   {diode(,T,E) ; move ; "`diode(,T,E)'" ljust}
   move down 0.3; right_
   {diode(,PR) ; move ; "`diode(,PR)'" ljust}
   }

.PE
