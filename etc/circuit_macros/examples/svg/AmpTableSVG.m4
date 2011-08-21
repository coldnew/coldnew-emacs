.PS
# `Amptable.m4'
cct_init
textwid = 0.7
movewid = 2pt__
{  
   {amp ; move ; "`amp'" ljust}
   move down 0.4; right_
   {amp(,0.3) ; move ; "`amp'(,0.3)" ljust}
   }
   move right_ 2.0
{  {delay ; move ; "`delay'" ljust}
   move down 0.4; right_
   {delay(,0.2) ; move ; "`delay'(,0.2)" ljust}
   }

   move right_ 2.0
{  {integrator ; move ; "`integrator'" ljust}
   move down 0.5; right_
   {integrator(,0.3) ; move ; "`integrator'(,0.3)" wid 1.2 ljust}
   }

.PE
