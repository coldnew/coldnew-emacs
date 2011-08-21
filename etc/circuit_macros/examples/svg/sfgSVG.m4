.PS
# sfg.m4
sfg_init( 1.2 )   # scale default size by 1.2

textht = textht*0.9

# Graph 1
move right 0.15
T:[
 Ft: sfgnode(,f(t),sfgbelow)
     sfgline(,1/K)
 Y1: sfgnode(,y`'svg_sub(1),sfgbelow rjust)
     sfgline(,K/M)
 DDY2: sfgnode(,y`'svg_sub(2),sfgbelow rjust)
     {".." at DDY2 +(-3,-1)*textoffset}
     sfgline(,1/s)
 DY2: sfgnode(,y`'svg_sub(2) = x`'svg_sub(2),sfgbelow ljust)
     {"." at DY2 +(2,-1)*textoffset}
     sfgline(,1/s)
 Y2: sfgnode(,y`'svg_sub(2) = x`'svg_sub(1),sfgbelow ljust)
 
 sf = 1
   sfgarc(from DY2 to DDY2,-B/M,below,,sf)
   sfgarc(from Y2 to DDY2,-K/M,above,ccw,sf)
   sfgarc(from Y2 to Y1,1,sfgabove,,sf)
 ]

# Graph 2
B: [
 R1: sfgnode(,r`'svg_sub(1),sfgabove)
     sfgline(,1)
 X1: sfgnode(,x`'svg_sub(1),ljust)
 R2: sfgnode(at R1+(0,-1),r`'svg_sub(2),sfgabove)
     sfgline(,1)
 X2: sfgnode(,x`'svg_sub(2),ljust)
     sfgarc(from X1 to X2,a`'svg_sub(21),rjust,ccw)
     sfgarc(from X2 to X1,a`'svg_sub(12),ljust,ccw)
 
     sfgself(at X1,,q`'svg_sub(11))
     sfgself(at X1,R,a`'svg_sub(11),ljust,cw,1.5)
     sfgself(at X2,-90,a`'svg_sub(22),sfgbelow)
 ] with .nw at T.sw + (0,linewid/2)

# Graph 3
G3: [ sfg_init( 0.85,0.35/2 )   # change node spacing and increase node size
#Point_(15)
 for_(1,4,1,`N`'m4x: sfgnode(,m4x,,fill_(0.9))
   sfgself(at N`'m4x,-90,P`'svg_sub(m4x),,cw)
   ifelse(m4x,4,,`sfgline(,G`'svg_sub(m4x),sfgbelow)') ')
 sfgarc(from N2 to N1,F`'svg_sub(2),sfgabove,ccw,)
 sfgarc(from N3 to N1,F`'svg_sub(3),sfgbelow,ccw,1.4)
] with .sw at B.se + (linewid/2,0)

A: [ sfg_init( 0.75,0.15 )
   arrow right linewid*2/3
 P: sfgnode(,p)
   sfgself(,90,a,below,cw)
   arrow from P.e "b" below
 Q: sfgnode(,q)
 R: sfgnode(at Q+(Q-P),r) # for dpic
   sfgarc(from Q to R,c,above,cw)
   sfgarc(from R to Q,d,below,cw)
   sfgself(at R,-90,e,above,cw)
   arrow right linewid*2/3 from R.e
 ] with .sw at G3.nw+(0,0.0)

.PE
