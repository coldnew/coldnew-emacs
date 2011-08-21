% ex07.m4
.PS
Barrel: box invis ht 0.4 wid 1.5
  up
Face: arc rad 2.6 from Barrel.e+(2.4,-1.7/2) to Barrel.e+(2.4,1.7/2)

  eps = 0.03
  arc ccw rad 2.6 from Face.start-(eps,0) to Face.end-(eps,0) \
    with .c at Face.c-(eps,0)
  line from Barrel.nw to Barrel.ne then to Face.end
  line from Barrel.sw to Barrel.se then to Face.start
  arc cw from Barrel.sw to Barrel.nw rad Barrel.ht*1.2

Cathode: Barrel.w+(0.3,0)
Term: Barrel.w+(-0.25,0)
  dcath = 0.13
  dan = 0.12

  line from Cathode up   dcath/2 left dcath/2 then to Term.x,Cathode.y+dcath/2
  dot
  line from Cathode down dcath/2 left dcath/2 then to Term.x,Cathode.y-dcath/2
  dot

  hole = 0.06
Hole: line invis up hole from Cathode+(0.5,-hole/2)
  line from Hole.end     up dan-hole/2 then to Term.x,Cathode.y+dan; dot
  line from Hole.start down dan-hole/2 then to Term.x,Cathode.y-dan; dot

  platewid = 0.25
  platesep = 0.15
Plates: box invis wid platewid ht platesep at Barrel.e+(-platewid/2,0)
  line right platewid from Plates.nw
  line right platewid from Plates.sw
  line up 0.3 from Plates.n
  line down 0.3 from Plates.s

linethick_(1.4)
psset_(linecolor=gray)

  move to Cathode+(0.03,0)
Spot: 0.9<Face.start,Face.end>
  arcto(Plates.c,Spot,(Plates.c.x-Cathode.x)*0.90)
  line to Spot chop 0 chop -0.02

psset_(linecolor=black)
thinlines_

  arrow <- from Cathode+(0.2,0.06) up 0.5 left 0.3
   "{\sl electron}" above "{\sl gun}" above

  arrow <- from Plates.nw+(0,0.06) up 0.5 left 0.3
   "{\sl deflection}" above "{\sl plates}" above

  arrow <- from 0.5<Plates.e,Spot>+(0,0.06) up 0.5 left 0.3
   "{\sl electron beam}" at Here+(-0.3,0) above

  spline <- from 1.015<Plates.e,Spot> right 0.1 up 0.1 then up 0.1 \
   then up 0.1 left 0.1
   "{\sl spot of light}sp_" rjust

  "{\sl vacuum}sp_" at 0.5<Face.start,Face.end>+(-0.4,0) rjust

  arrow <- from Face.start+(0.1,0.5) left 0.6*1.4 down 0.3*1.4
   "{\sl phosphor coating}sp_" rjust

  arrow <- from 0.2<Barrel.se,Face.start> left 0.6 down 0.3
   "{\sl glass enclosure (tube)}sp_" rjust
.PE
