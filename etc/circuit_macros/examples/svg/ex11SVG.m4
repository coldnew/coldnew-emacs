.PS
# ex11.m4
cct_init

   "audio" ljust
   { arrow down_ elen_/4}
   resistor(down_ 2*elen_) ; llabel(R`'svg_sub(17))
G: ground
   capacitor(right_ dimen_ from last [].c) ; rlabel(,C`'svg_sub(19))
   {arrow <- right_ arrowht from last line.start}
   { resistor(down_ to (Here,G)); rlabel(,R`'svg_sub(19)) }
{R18: resistor(up_ elen_*1.2); llabel(,R`'svg_sub(18)) }
  
   line right_ elen_/4 ; up_
Q4: bi_tr(,,,E) with .B at Here; llabel(,,Q`'svg_sub(4))
   resistor(down_ from Q4.E to (Q4.E,G)); rlabel(,R`'svg_sub(21))
   resistor(up_ from Q4.C to (Q4.C,R18.end)); llabel(,R`'svg_sub(20))
   line up_ elen_/4; dot; "svg_it(V`'svg_sub(CC))" ljust

T1: Q4.C+(0,elen_/8)

   capacitor(right_ dimen_ from T1); llabel(,C`'svg_sub(20))
   { capacitor(down_ 0.5*(Here.y-G.y)); rlabel(,C`'svg_sub(21))
     resistor(right_ elen_); llabel(,R`'svg_sub(23))
     arrow <- from last [] to (last [],G)
     }
   resistor(right_ elen_); llabel(,R`'svg_sub(22))
   { resistor(down_ 0.5*(Here.y-G.y)); llabel(,R`'svg_sub(24))
     capacitor(down_ to (Here,G)); llabel(,C`'svg_sub(22)) }

   capacitor(right_ dimen_); llabel(,C`'svg_sub(23))
   { capacitor(down_ to (Here,G)); rlabel(,C`'svg_sub(24)) }
   line right_ elen_/2
Q5I: Here
   line right_ elen_/4 ; up_
Q5: bi_tr(,,,E) with .B at Here; rlabel(,Q`'svg_sub(5))
R27: resistor(down_ dimen_ from Q5.E); rlabel(,R`'svg_sub(27))
   line to (Here,G)
   resistor(from Q5I down_ Q5I.y-R27.end.y-elen_/12); rlabel(,R`'svg_sub(25))
   { capacitor(to (Here,G)); llabel(,C`'svg_sub(25)) }
   
   crossover(to (R27,Here)+(hoprad_,0),,R27)
R28: resistor(right_ elen_-hoprad_); llabel(,R`'svg_sub(28))
   resistor(down_ to (Here,G)); rlabel(,R`'svg_sub(30))
   line to G

   resistor(up_ from Q5.C to (Q5.C,R18.end)); llabel(,R`'svg_sub(26))
   line to R18.end

   up_
Q6: bi_tr(,,,E) with .E at (R28.end.x,Q5.E.y+(T1.y-Q4.B.y)); llabel(,,Q`'svg_sub(6))

   line from Q6.B to (Q5.C,Q6.B)
   resistor(down_ from Q6.E to R28.end); rlabel(,R`'svg_sub(29))

   line right_ dimen_*3/4 from Q6.C

   { R31: resistor(down_ elen_); circle diam dimen_*0.8 with .c at last line.c
    "svg_it(R`'svg_sub(31))" below at last circle.sw
    "svg_it(V)" below at last line.c+(dimen_/4,0) }
   line right_ dimen_
   { capacitor(down_ elen_); rlabel(,,C`'svg_sub(26))
     line down_ elen_/4; dot; "svg_it(V`'svg_sub(SS))" ljust }
   line right_ dimen_/2
T6: transformer(down_ elen_) with .P1 at Here
   line from T6.P2 to R31.end
   {"svg_it(T`'svg_sub(6))" at T6.n above }
   line right_ elen_/3 from T6.S1
   inductor(down_ elen_,W)
   line to T6.S2

   speaker(R) with .Box.w at last [].e+(dimen_/8,0)

.PE
