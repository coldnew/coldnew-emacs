% `Amptable.m4'
.PS
cct_init
textwid = 0.7
movewid = 2pt__
{  
   {amp ; move ; "{\tt `amp'}" ljust}
   move down 0.4; right_
   {amp(,0.3) ; move ; "{\tt `amp'(,0.3)}" ljust}
   }
   move right_ 2.0
{  {delay ; move ; "{\tt `delay'}" ljust}
   move down 0.4; right_
   {delay(,0.2) ; move ; "{\tt `delay'(,0.2)}" ljust}
   }

   move right_ 2.0
{  {integrator ; move ; "{\tt `integrator'}" ljust}
   move down 0.5; right_
   {integrator(,0.3) ; move ; "{\tt `integrator'(,0.3)}" ljust}
   }

.PE
