divert(-1)
   libgen.m4                    Base macros for dpic and gpic diagrams

* Circuit_macros Version 6.83, copyright (c) 2010 J. D. Aplevich, under    *
* the LaTeX Project Public License. The files of this distribution may    *
* be redistributed or modified, provided that this copyright notice is    *
* included and provided that modifications are clearly marked to          *
* distinguish them from this distribution.  There is no warranty          *
* whatsoever for these files.                                             *

                                Installation directory.  You can set this to
                                the null string if you use an environment
                                variable to tell m4 where to search:
ifdef(`HOMELIB_',,
`define(`HOMELIB_',`/home/coldnew/.kde4/share/apps/cirkuit/circuit_macros/')')

                                Default pic processor: gpic.  To make dpic -p
                                the default, change gpic to pstricks in the
                                line below. To make dpic -g and TikZ-PGF the
                                default, change gpic to pgf:
define(`m4defaultprocessor',gpic)

ifdef(`m4picprocessor',,`include(HOMELIB_`'m4defaultprocessor.m4)divert(-1)')

==========================================================================

                                Processor shortcuts
define(`ifgpic',`ifelse(m4picprocessor,gpic,`$1',`$2')')
define(`ifdpic',`ifelse(m4picprocessor,dpic,`$1',`$2')')

define(`ifpstricks',`ifelse(m4postprocessor,pstricks,`$1',`$2')')
define(`ifpgf',`ifelse(m4postprocessor,pgf,`$1',`$2')')
define(`ifmfpic',`ifelse(m4postprocessor,mfpic,`$1',`$2')')
define(`ifmpost',`ifelse(m4postprocessor,mpost,`$1',`$2')')
define(`ifpostscript',`ifelse(m4postprocessor,postscript,`$1',`$2')')
define(`ifsvg',`ifelse(m4postprocessor,svg,`$1',`$2')')
define(`ifxfig',`ifelse(m4postprocessor,xfig,`$1',`$2')')
define(`ifroff',`ifelse(m4postprocessor,roff,`$1',`$2')')

                                Set environment direction
define(`right_',`define(`m4dir',right) manhattan m4dir')
define(`left_',`define(`m4dir',left) manhattan m4dir')
define(`up_',`define(`m4dir',up) manhattan m4dir')
define(`down_',`define(`m4dir',down) manhattan m4dir')

                                Manhattan direction cosines
define(`manhattan',`dnl
define(`m4a_',`ifelse(m4dir,left,-1,m4dir,up,0,m4dir,down,0,1)')dnl
define(`m4b_',`ifelse(m4dir,left,0,m4dir,up,1,m4dir,down,-1,0)')dnl
define(`m4c_',`neg_(m4b_)')dnl
define(`m4d_',`m4a_')')
                                Default current coordinates
define(`m4tx',`Here.x')
define(`m4ty',`Here.y')
                                Arbitrary direction cosines
                                `Point_(integer degrees)'
define(`Point_',`rp_ang = prod_(`$1',`dtor_')
define(`m4a_',`cos(rp_ang)')define(`m4b_',`sin(rp_ang)')dnl
define(`m4c_',`(-sin(rp_ang))')define(`m4d_',`cos(rp_ang)')dnl
')
                                Evaluate the argument as a macro
define(`m4xpand',`$1')
                                `direction_( U|D|L|R|degrees, default )
                                 set direction and default direction'
define(`direction_',`ifelse(`$1',,ifelse(`$2',,`right_',`$2'),
 `$1',U,`up_',`$1',L,`left_',`$1',D,`down_',`$1',R,`right_',`Point_(`$1')')')

                                `point_(radians)'
define(`point_',
`ifelse(`$1',rp_ang,,`rp_ang = `$1'')
 define(`m4a_',`cos(rp_ang)')define(`m4b_',`sin(rp_ang)')dnl
 define(`m4c_',`(-sin(rp_ang))')define(`m4d_',`cos(rp_ang)')dnl
')
                                `rpoint_( linespec )
                                (Gpic returns NaN for atan2(0,0)):'
define(`rpoint_',`line invis $1
  ifdef(`if_rpoint__',
   `rpoint__',`dnl
    rp_wid = last line.end.x-last line.start.x
    rp_ht = last line.end.y-last line.start.y
    rp_len = vlength(rp_wid,rp_ht); move to last line.start
    ifgpic(`if (rp_len == 0) then { rp_ang=0 } else { ')dnl
    rp_ang = atan2(rp_ht,rp_wid) ifgpic(` }')')
  point_(rp_ang)' )

                                Polar to rectangular coords, returning a pair
                                `rect_(radius,radians)'
define(`rect_',`vscal_(`$1',cos(`$2'),sin(`$2'))')
                                `Rect_(radius,degrees)'
define(`Rect_',`vscal_(`$1',cosd(`$2'),sind(`$2'))')

                                Rectangular to polar coords, returning a pair
                                `polar_(x,y)'
define(`polar_',`vlength(`$1',`$2'),atan2(`$2',`$1')')
                                `Polar_(x,y)' returns degrees
define(`Polar_',`vlength(`$1',`$2'),rtod_*atan2(`$2',`$1')')

                               `Unique arc, eg arcr(A,0.5,0,pi_/2) cw ->
                arcr( position, radius, start radians, end radians)'
define(`arcr',`arc from (`$1')+(rect_(`$2',`$3')) to (`$1')+(rect_(`$2',`$4')) \
                     rad `$2' with .c at `$1'')

                               `Unique arc, with angles in degrees:
                                eg arcd(A,0.5,0,90) dashed
                arcd( position, radius, start degrees, end degrees)'
define(`arcd',`arc from (`$1')+(Rect_(`$2',`$3')) to (`$1')+(Rect_(`$2',`$4')) \
                     rad `$2' with .c at `$1'')

                               `Unique acute-angle arc, obtuse if radius is -ve
                                arca(chord linespec, ccw|cw, radius, modifiers)'
define(`arca',`[Chord: line invis `$1'
  dy = Chord.end.y-Chord.start.y; dx = Chord.end.x-Chord.start.x
  ssq = dx*dx+dy*dy
  drad = max(sqrt(ssq)/2,ifelse(`$3',,arcrad,abs(`$3')))
  if ssq==0 then { dfac = 1 } else { dfac = sqrt(drad^2/ssq-0.25) }
  ifelse(`$3',,,`dfac = sign(`$3')*dfac;')dnl
  arc `$2' `$4' from Chord.start to Chord.end rad drad \
    with .c at Chord.c ifelse(`$2',cw,-,+)(-dy*dfac,dx*dfac)
  Origin: 0,0 ] with .Origin at (0,0) ')

                               `Integer for loop with index variable m4x:
                                for_(initial,final,incr,`actions')
                                eg for_(0,10,2,`print m4x')'
define(`for_',`ifelse(eval(`$1'!=(`$2'+(`$3'))),1,`pushdef(`m4x',`$1') $4
  popdef(`m4x')for_(eval(`$1'+(`$3')),`$2',`$3',`$4')')')

                               `Loopover_(`variable',actions,value1,value2,...)
                                Repeat actions with variable set to each of
                                value1, value2, ... in succession
                                eg Loopover_(`x',`print "x"',Tom,Dick,Mary)'
define(`Loopover_',`ifelse(`$3',,,`define(`$1',`$3')dnl
  $2
Loopover_(`$1',`$2',shift(shift(shift($@))))')')

                                Intersection of named lines
                                `Intersect_(Name1,Name2)'
define(`Intersect_',`intersect_($1.start,$1.end,$2.start,$2.end)')

                                Intersection of lines joining named positions
                                `intersect_(Start1,End1,Start2,End2)'
define(`intersect_',`((($3.x-$1.x)*($3.y-$4.y)-($3.y-$1.y)*($3.x-$4.x))/\
   (($2.x-$1.x)*($3.y-$4.y)-($2.y-$1.y)*($3.x-$4.x))\
   between `$1' and `$2')')

                               `Dashed line drawn in detail
                                dashline(linespec, thickness|<->|color,
                                  dash len, gap len,G)
                                Arg5=G ends the line with a gap'
define(`dashline',`rpoint_(`$1')define(`m4opts',`$2')
  define(`m4h',`ifelse(`$3',,dashwid,abs(`$3'))')dnl
  define(`m4v',`ifelse(`$4',,m4h/2,abs(`$4'))')dnl
  define(`m4lar',)define(`m4rar',)dnl
  ifelse(m4xtract(`m4opts',<->),1,`define(`m4lar',<-)define(`m4rar',->)',
    m4xtract(`m4opts',<-),1,`define(`m4lar',<-)',
    m4xtract(`m4opts',->),1,`define(`m4rar',->)')dnl
  if (m4h+m4v)==0 then { dashline_n = 1 } \
  else { dashline_n = max(1,int(abs((rp_len ifelse(`$5',G,,+m4v))/(m4h+m4v)))) }
  dashline_f = (rp_len-(dashline_n ifelse(`$5',G,,-1))*m4v)/dashline_n
  for m4ti=1 to dashline_n do {
    if m4ti==1 then { line m4opts m4lar to rvec_(dashline_f,0) } \
    else { if m4ti==dashline_n then { line m4opts m4rar to rvec_(dashline_f,0)}\
    else { line m4opts to rvec_(dashline_f,0) }}
    ifelse(`$5',G,,
     `if m4ti < dashline_n then {')move to rvec_(m4v,0)ifelse(`$5',G,,})
    } ')

                               `round(line thickness,at location,color)
                                filled circle for rounded corners'
define(`round',`{ circle thick ifelse(`$1',,`linethick/2',(`$1')/2) \
  diameter ifelse(`$1',,`linethick',`($1)')/2 bp__ \
  ifelse(`$2',,`at Here',`$2') $3}')

                               `corner(line thickness,color)
                                filled square to make square line corner'
define(`corner',`define(`m4lfr',`ifelse(`$1',,linethick,`($1)')')
  {line thick m4lfr $2 \
   from (m4lfr bp__)/lin_leng(last line)/2 \
   between last line.end and last line.start \
   to 1+(m4lfr bp__)/lin_leng(last line)/2 \
   between last line.start and last line.end}')

                               `Technical drawing centerline:
                                centerline_(linespec, thickness|color,
                                  minimum long dash len, short dash len,
                                  gap len)'
define(`centerline_',`rpoint_(`$1')
  M4start: last line.start
  M4end: last line.end
  define(`m4h',`ifelse(`$3',,linewid,abs(`$3'))')dnl
  define(`m4v',`ifelse(`$4',,dashwid,abs(`$4'))')dnl
  define(`m4w',`ifelse(`$4',,dashwid/2,abs(`$5'))')dnl
  if (m4h+m4v+m4w*2)==0 then { centerline_n = 1 } \
  else { centerline_n = max(1,int(rp_len/(m4h+m4v+m4w*2))) }
  centerline_f = (rp_len-(centerline_n-1)*(m4v+m4w*2))/centerline_n
  for m4ti=1 to centerline_n do {
    line `$2' to rvec_(centerline_f,0)
    if m4ti < centerline_n then {
      move to rvec_(m4w,0)
      line `$2' to rvec_(m4v,0)
      move to rvec_(m4w,0)}
    }
  line invis from M4start to M4end
 ')

                               `m4xtract(str1,str2)
                                Return value 1 if str2 present in str1 else 0,
                                delete str2 from str1'
define(`m4xtract',`define(`m4I',index($1,`$2'))dnl
define(`$1',substr($1,0,m4I)`'substr($1,ifelse(m4I,-1,0,eval(m4I+len($2)))))dnl
ifelse(m4I,-1,0,1)')

                                Generalized attributes and coordinates that work
                                exactly provided the current direction is up,
                                down, left or right.  Macros for elements need
                                only be written once assuming a default
                                direction of right, say.

                                String attributes, default right
define(`above_',`ifelse(m4dir,right,above,m4dir,left,below,m4dir,up,rjust,ljust)')
define(`below_',`ifelse(m4dir,right,below,m4dir,left,above,m4dir,up,ljust,rjust)')
define(`ljust_',`ifelse(m4dir,right,ljust,m4dir,left,rjust,m4dir,up,above,below)')
define(`rjust_',`ifelse(m4dir,right,rjust,m4dir,left,ljust,m4dir,up,below,above)')
                                Dimensions: wid, ht
define(`wid_',`ifelse(m4dir,right,wid,m4dir,left,wid,m4dir,up,ht,ht)')
define(`ht_',`ifelse(m4dir,right,ht,m4dir,left,ht,m4dir,up,wid,wid)')
                                Compass corners
define(`n_',`ifelse(m4dir,right,n,m4dir,left,s,m4dir,up,w,e)')
define(`s_',`ifelse(m4dir,right,s,m4dir,left,t,m4dir,up,r,l)')
define(`w_',`ifelse(m4dir,right,w,m4dir,left,e,m4dir,up,s,n)')
define(`e_',`ifelse(m4dir,right,e,m4dir,left,w,m4dir,up,n,s)')
define(`ne_',`ifelse(m4dir,right,ne,m4dir,left,sw,m4dir,up,nw,se)')
define(`nw_',`ifelse(m4dir,right,nw,m4dir,left,se,m4dir,up,sw,ne)')
define(`sw_',`ifelse(m4dir,right,sw,m4dir,left,ne,m4dir,up,se,nw)')
define(`se_',`ifelse(m4dir,right,se,m4dir,left,nw,m4dir,up,ne,sw)')
define(`loc_',`ifelse(m4dir,right,($1,$2),m4dir,left,($1,$2),($2,$1))')
                                Directions
define(`rt_',`ifelse(m4dir,right,right,m4dir,left,left,m4dir,up,up,down)')
define(`dn_',`ifelse(m4dir,right,down,m4dir,left,up,m4dir,up,right,left)')
define(`lt_',`ifelse(m4dir,right,left,m4dir,left,right,m4dir,up,down,up)')
define(`up__',`ifelse(m4dir,right,up,m4dir,left,down,m4dir,up,left,right)')

                                Binary operations giving simplified readable
                                expressions when possible.
define(`prod_',`ifelse($1,0,0,$1,-0,0,$2,0,0,$2,-0,0,$1,1,`$2',$2,1,`$1',
  $1,-1,`-($2)',$2,-1,`-($1)',`($1)*($2)')')
define(`sum_',`ifelse($1,0,`$2',$1,-0,`$2',$2,0,`$1',$2,-0,`$1',
  index($2,-),0,`$1$2',`$1+$2')')
define(`diff_',`ifelse($2,0,`$1',$2,-0,`$1',$1,0,`-($2)',$1,-0,`-($2)',
  `$1-($2)')')

                                Unary arithmetic operations (for single terms:
                                              use parentheses when necessary!)
define(`abs_',`ifelse(index($1,-),0,`substr($1,1)',`$1')')
define(`neg_',`ifelse($1,0,0,index($1,-),0,`substr($1,1)',-`$1')')
define(`sign_',`ifelse(index($1,-),0,-,)')

                                Vector rotation: returns a pair
                                `vrot_(x,y,ct,st)'
define(`vrot_',
  `diff_(prod_($3,$1),prod_($4,$2)),sum_(prod_($4,$1),prod_($3,$2))')
define(`rrot_',`Here+(vrot_($1,$2,cos($3),sin($3)))')

                                Vector scaling: returns a pair
                                `vscal_(scalefactor,x,y)'
define(`vscal_',`prod_($1,$2),prod_($1,$3)')
                                Relative position
define(`rpos_',`Here+`$1'')
                                Absolute and relative vector determined by
                                current direction numbers
define(`vec_',
 `(sum_(prod_($1,m4a_),prod_($2,m4c_)),sum_(prod_($1,m4b_),prod_($2,m4d_)))')
define(`rvec_',`(m4tx,m4ty)+vec_($1,$2)')

                                Sine and cosine of integer degrees with
                                simplified values for special cases
define(`Sin',`ifelse(eval(`$1'<0),1,`neg_(Sin(abs_(eval(`$1'))))',dnl
eval(`$1'>180),1,`Sin(eval(`$1'-360))',dnl
eval(`$1'>90),1,`Sin(eval(180-(`$1')))',dnl
eval(`$1'==90),1,1,eval(`$1'==30),1,0.5,eval(`$1'==0),1,0,sin((`$1')*`dtor_'))')
define(`Cos',`Sin(eval($1+90))')

                                Sine and cosine of arbitrary argument in degrees
define(`sind',`ifelse(`$1',0,0,`$1',90,1,`$1',-90,-1,sin((`$1')*`dtor_'))')
define(`cosd',`ifelse(`$1',0,1,`$1',90,0,`$1',-90, 0,cos((`$1')*`dtor_'))')

                                Constants
define(`dtor_',`ifdef(`dtor__',`dtor`'_',`0.017453292519943295')') Deg to rad
define(`rtod_',`ifdef(`rtod__',`rtod`'_',`57.295779513082323')') Rad to deg
define(`twopi_',`ifdef(`twopi__',`twopi`'_',`6.2831853071795862')') 2*pi
define(`pi_',`ifdef(`pi__',`pi`'_',`(twopi_/2)')') 2*pi
define(`E_',     `2.718281828459045')       e
define(`log10E_',`0.434294481903252')      `log10(E_)'

                                Max, min of an arbitrary number of arguments
define(`Max',`ifelse(`$2',,`$1',`max(`$1',Max(shift($@)))')')
define(`Min',`ifelse(`$2',,`$1',`min(`$1',Min(shift($@)))')')

                                Dpic May 2001 or later has these built in
ifgpic(`
define(`abs',`max(`$1',-(`$1'))')
define(`sign',`((`$1')/abs(`$1'))')
define(`floor',`int(`$1')')     `old versions of gpic use the floor function'
define(`Int_',`(sign(`$1')*int(abs(`$1')))')  `corrected int() fn'
define(`loge',`(log(`$1')/log10E_)')
define(`expe',`(exp((`$1')*log10E_))')
define(`tan', `(sin(`$1')/cos(`$1'))')
define(`acos',`atan2(sqrt(1-(`$1')^2),`$1')') `acos(ratio)'
define(`asin',`atan2(`$1',sqrt(1-(`$1')^2))') `asin(ratio)'
define(`pmod',`((((`$1')%(`$2'))+`$2')%(`$2'))') `+ve mod(M,N) eg pmod(-3,5)=2'
',`
define(`Int_',`int(`$1')')
')

define(`ceiling_',`(-floor(-(`$1')))')
define(`round_',`(int(abs(`$1')+sign(`$1')*0.5))')

define(`bp__', `*(scale/72)')    Absolute Adobe point
define(`pt__', `*(scale/72.27)') Absolute TeX point
define(`pc__', `*12pt__')        Absolute Picas
define(`in__', `*scale')         Absolute inches
define(`mm__', `*(scale/25.4)')  Absolute mm

                                `Use with \boxdim to get the dimensions of a
                                 TeX \hbox
                                 boxdim(label, h|w|d|v, default length)'
define(`boxdim',`ifelse(`$2',v,`sum_(boxdim(`$1',h,`$3'),boxdim(`$1',d,0))',
  ifdef(`$1_$2',`$1_$2',ifelse(`$3',,0,`$3')))')

                                `String conveniences:
                                 extract substring without deletion'
define(`xtract',`ifelse(index(`$1',`$2'),-1,,`$2')')
                               `Test if in string ifinstr(str,substr,in,notin)'
define(`ifinstr',`ifelse(index(`$1',`$2'),-1,`$4',`$3')')

                                `String with exact typeset dimensions:
                                 Requires s_init(name), sinclude(filename.dim),
                                 boxdims.sty, and processing twice.  If there
                                 are two or more args they are given to
                                 sprintf(...); e.g. s_box($x^%g_%g$,3,4)'
define(`s_box',
`ifsvg(`ifelse(`$2',,,`sprintf(')"ifelse(index(`$1',"),0,
`substr(`$1',1,eval(len(`$1')-2))',`$1')"ifelse(`$2',,,`,shift($@))')',
`define(`m4_k',ifdef(`m4_k',incr(m4_k),1))dnl
ifelse(`$2',,,`sprintf(')"ifdef(`s_name',,`{\bf !!}')dnl
\boxdims{s_name`'_`'m4_k}{ifelse(index(`$1',"),0,
  `substr(`$1',1,eval(len(`$1')-2))',`$1')}dnl
"ifelse(`$2',,,`,shift($@))')\
  wid s_wd(,`textwid') ht s_ht(,`textht')+s_dp')')

define(`text_ang',90)
                                `Like s_box but text is rotated text_ang degrees
                                 (requires PSTricks or pgf)'
define(`rs_box',
`define(`m4_k',ifdef(`m4_k',incr(m4_k),1))dnl
 define(`m4_text',
 `ifelse(index(`$1',"),0,`substr(`$1',1,eval(len(`$1')-2))',`$1')')dnl
ifelse(`$2',,,`sprintf(')"ifdef(`s_name',,`{\bf !!}')dnl
\defboxdim{s_name`'_`'m4_k}{m4_text}dnl
ifelse(m4postprocessor,pstricks, `\rput[c]{text_ang}(0,0)',
       m4postprocessor,pgf, `\pgftext[rotate=text_ang]')dnl
{m4_text}"ifelse(`$2',,,`,shift($@),shift($@))') \
 wid s_wd(,textwid)*cosd(text_ang) + (s_ht(,textht)+s_dp)*sind(text_ang) \
 ht (s_ht(,textht)+s_dp)*cosd(text_ang)+s_wd(,textwid)*sind(text_ang)')

                                `Initialize string index: s_init(name)'
define(`s_init',`define(`s_name',`$1')')

                                `Dimensions of the most recent s_box
                                 (or corresponding to the first argument)'
define(`s_ht',`boxdim(ifelse(`$1',,`s_name`'_`'m4_k',`$1'),h,`$2')')
define(`s_wd',`boxdim(ifelse(`$1',,`s_name`'_`'m4_k',`$1'),w,`$2')')
define(`s_dp',`boxdim(ifelse(`$1',,`s_name`'_`'m4_k',`$1'),d,`$2')')

                                `f_box( boxspec, labelargs ) Evaluates to
                                 box boxspec s_box(labelargs)
                                 or, if there is only one argument, to
                                 box invis fill_(1) s_box(arg1)
                                 Like s_box but overlays text on a box
                                 specified by the first argument, eg
                                 f_box( invis fill_(0.9), labelargs)'
define(`f_box',`box ifelse(`$2',,
 `invis fill_(1) s_box($1)',
 `$1 s_box(shift($@))')')

                                `dot(at location,radius,fill)'
define(`dotrad_',(0.02*scale))
define(`dot',`[define(`m4ft',`ifelse(`$3',,0,(`$3'))')dnl
  ifdef(`r_',`rgbfill(r_+(1-r_)*m4ft, g_+(1-g_)*m4ft, b_+(1-b_)*m4ft,
    circle rad ifelse(`$2',,`dotrad_',`$2'))',
   `circle rad ifelse(`$2',,`dotrad_',`$2') fill_(m4ft)')
  `$4'] with .c ifelse(`$1',,`at Here',`$1')
  move to last [].c')
                                `cross(at location); assumes that a
                                 cross always has manhattan directions'
define(`crosswd_',(0.05*scale))
define(`cross',`[{line from Here+(0,neg_(crosswd_)) to Here+(0,crosswd_)}
                  line from Here+(neg_(crosswd_),0) to Here+(crosswd_,0)
  `$2'] with .c ifelse(`$1',,`at Here',`$1'); move to last [].c')

                                `boxcoord(name,xfraction,yfraction)
                                 internal position in a named planar object'
define(`boxcoord',
  `(`$2' between `$1'.w and `$1'.e,`$3' between `$1'.s and `$1'.n)')

                                `shadebox(box boxspec,shadewid)' Shaded box
define(`shadebox',` $1
  ifpstricks(`m4t1 = linethick; command "{\psset{linecolor=gray}%"',
 `ifpgf(`m4t1 = linethick; command "\begin{scope}[draw=gray]%"',
 `if linethick < 0 then { m4t1 = 0.8 } else { m4t1 = linethick }')')
  define(`m4h',`ifelse(`$2',,m4t1*5/4,`($2)')')dnl
  define(`m4v',`(m4h+m4t1)/2 bp__')dnl
  {line thickness m4h from last box.sw+(m4v,neg_(m4v)) \
    to last box.se+(m4v,neg_(m4v)) then to last box.ne+(m4v,neg_(m4v))}
  {move to last box.se+(m4v,neg_(m4v))+(m4h/2 bp__,neg_(m4h)/2 bp__)}
  ifpstricks(`command "}%"')
  ifpgf(`command "\end{scope}"')
  ')

                                `lbox(wid,ht,type)
                                 box oriented in current direction;
                                 type= eg dotted'
define(`lbox',`pushdef(`m4bwd',ifelse(`$1',,boxwid,`($1)'))dnl
  pushdef(`m4bht',ifelse(`$2',,boxht,`($2)'))dnl
  line from rvec_(m4bwd,0) to rvec_(m4bwd,m4bht/2) then to rvec_(0,m4bht/2) \
    then to rvec_(0,neg_(m4bht)/2) then to rvec_(m4bwd,neg_(m4bht)/2) \
    then to rvec_(m4bwd,0) `$3' popdef(`m4bwd')popdef(`m4bht') ')

                                `rotbox(wid,ht,type)
                                 box oriented in current direction in [] block;
                                 type= eg dotted shaded "green"'
define(`rotbox',`[pushdef(`m4bwd',ifelse(`$1',,boxwid,`($1)'))dnl
  pushdef(`m4bht',ifelse(`$2',,boxht,`($2)'))dnl
  N: vec_(0,m4bht/2); E: vec_(m4bwd,0); S: vec_(0,-m4bht/2); W: 0,0
  NE: vec_(m4bwd,m4bht/2); SE: vec_(m4bwd,-m4bht/2)
  NW: vec_(0,m4bht/2); SW: vec_(0,-m4bht/2)
  line from E to NE then to NW then to SW then to SE then to E `$3'] \
     wid abs(m4a_*m4bwd)+abs(m4b_*m4bht) \
     ht abs(m4c_*m4bwd)+abs(m4d_*m4bht) popdef(`m4bwd')popdef(`m4bht')')

                                `rotellipse(wid,ht,type)
                                 ellipse oriented in current direction and
                                 enclosed in a [] block, e.g.
                                 Point_(45); rotellipse(,,dotted fill_(0.9))'
define(`rotellipse',
 `[define(`m4ehw',(ifelse(`$1',,ellipsewid,`($1)')/2))dnl
  define(`m4ehh',(ifelse(`$2',,ellipseht,`($2)')/2))dnl
  N: vec_(0,m4ehh); E: vec_(m4ehw,0); S: vec_(0,-m4ehh); W: vec_(-m4ehw,0)
  spline ifdpic(
   `0.551784 from S to vec_(m4ehw,-m4ehh) \
      then to vec_(m4ehw,m4ehh) then to vec_(-m4ehw,m4ehh) \
      then to vec_(-m4ehw,-m4ehh) then to S',
   `from S to vec_(m4ehw/64,-m4ehh) \
      for_(1,31,1,
       `then to vec_(m4ehw*sin(twopi_*m4x/32),-m4ehh*cos(twopi_*m4x/32))\')\
      then to vec_(-m4ehw/64,-m4ehh) then to S') \
    `$3'] wid 2*sqrt((m4a_*m4ehw)^2+(m4b_*m4ehh)^2) \
      ht  2*sqrt((m4c_*m4ehw)^2+(m4d_*m4ehh)^2)')

                                Small space for string justification
define(`sp_',`ifgpic(`\hbox{$\:$}')')

                               `Select arg 1 if it begins with " or sprintf,
                                else arg 2'
define(`m4lstring',`ifelse(
  index(`$1',"),0,`$1',index(`$1',sprintf),0,`$1',`$2')')

                       `Dimensioning for diagrams:
                        dimension_(linespec, vert offset, label,
                                   D|H|W|blank width, tic offset, <-|->)
                        If arg 3 is s_box(...) and arg4=D|H|W then arg4 means:
                           D: blank width is the diagonal length of arg3
                           H: blank width is height of arg3 + textoffset*2
                           W: blank width is width of arg3 + textoffset*2
                        otherwise: arg4 is absolute blank width '
define(`dimension_',`rpoint_(`$1') ; {
  define(`m4g',ifelse(`$4',,0,
   `$4',W,(s_wd + textoffset*2),
   `$4',H,(s_ht + s_dp + textoffset*2),
   `$4',D,vlength(s_wd,s_ht),`($4)'))dnl
  define(`m4h',`(rp_len ifelse((`$4'),(),,neg_(m4g)))')dnl
  ifelse((`$2'),(),,`if (`$2') != 0 then {
     {move to rvec_(0,     sign(`$2')*ifelse(`$5',,`3.6bp__',(`$5')))
       line to rvec_(0,`$2')}
     {move to rvec_(rp_len,sign(`$2')*ifelse(`$5',,`3.6bp__',(`$5')))
       line to rvec_(0,`$2')}
     move to rvec_(0,`$2') }')
  if m4h > 2*arrowht then {
      { line ifelse(`$6',,<-,index($6,<),0,<-) to rvec_(m4h/2,0)
        ifelse((`$4'),(),,`move to rvec_(m4g,0)')
        line ifelse(`$6',,->,eval(index($6,>)>0),1,->) to rvec_(m4h/2,0) }
    } else {
      { arrow from rvec_(-arrowht*1.5,0) to Here
        arrow from rvec_(rp_len+arrowht*1.5,0) to rvec_(rp_len,0) }
    }
  ifelse(`$3',,,
    `m4lstring(`$3',"`$3'") at rvec_(rp_len/2,0)')
  }')

                                `shade(gray value,closed line specs)
                                 Fill an arbitray closed curve with a gray value
                                 (requires gpic, pstricks, or postscript out)'
define(`shade',`beginshade(`$1')
  `$2'
  endshade')

                                `like shade(,) but without the argument:
                                 beginshade(gray value)
                                   closed line specs
                                 endshade'
define(`beginshade',`define(`m4_shade',`ifelse(`$1',,`fillval',`($1)')')dnl
  ifelse(m4picprocessor,gpic,
   `command sprintf("\special{sh %g}",1-m4_shade)',
  m4postprocessor,mfpic,
   `ifelse(`$1',0,
    `command "\gfill\draw\lclosed"',
    `if (m4_shade<0.99) then {
      command sprintf(`"\shade[%gpt]\draw\lclosed"',expe((m4_shade-0.3)*2))}')',
  m4postprocessor,mpost,
   `command "def Y="',
  m4postprocessor,svg,
   `command sprintf("<g fill=\"rgb(%g,%g,%g)\">",\
      int(m4_shade*255),int(m4_shade*255),int(m4_shade*255))',
  m4postprocessor,pgf,
   `command "\global\let\dpicshdraw=\dpicdraw\global\def\dpicdraw{}"
    command "\global\def\dpicstop{--}"
    command sprintf("\dpicshdraw[fill=white!%g!black]",m4_shade*100)',
  m4postprocessor,pstricks,
   `command sprintf("\newgray{m4fillv}{%g}",m4_shade)
    command "\pscustom[fillstyle=solid`,'fillcolor=m4fillv]{%"',
  m4postprocessor,postscript,
   `command "/endstroke {}def /npath {}def newpath"')dnl
')')

define(`endshade',
 `ifelse(m4postprocessor,pstricks,
   `command "}%"',
  m4postprocessor,postscript,
   `command sprintf(" gsave %g setgray fill grestore",m4_shade)
    command "/endstroke {ostroke} def /npath {newpath} def endstroke"',
  m4postprocessor,pgf,
   `command "cycle; \global\let\dpicdraw=\dpicshdraw\global\def\dpicstop{;}"',
  m4postprocessor,svg,
   `command "</g>"',
  m4postprocessor,mpost,
   `command "enddef; def drw= enddef; def X=--enddef;"
    command sprintf("fill Y cycle withcolor %gwhite;",m4_shade)
    command "def drw=draw enddef; def X=;enddef; Y;"dnl
')')
                                `gshade(gray value,A,B,...,Z,A,B) (Note last
                                 two arguments).  Shade a polygon with named
                                 vertices, attempting to avoid sharp corners.'
define(`gshade',`m4tmp = linethick; linethick = 0
  shade(`$1',line from 0.5 between `$2' and `$3' \
  to gpolyline(.004,shift($@)) \
  then to 0.5 between `$2' and `$3')
  linethick = m4tmp')
define(`gpolyline',`1-(`$1') between `$2' and `$3' \
  then to `$1' between `$3' and `$4' ifelse(`$5',,,`\
  then to gpolyline(`$1',shift(shift($@)))')')

                                `m4_arrow(linespec, ht, wid)'
define(`m4_arrow',
   ifelse(ifgpic(T)`'ifpstricks(T)`'ifxfig(T)`'ifpgf(T)`'ifsvg(T),,
    `open_arrow(`$1',`$2',`$3')'
    `arrow `$1' ifelse(`$2',,,ht `$2') ifelse(`$3',,,wid `$3')'))

                                `open_arrow(linespec, ht, wid)
                                 arrow with outlined head'
define(`open_arrow',`arrow invis `$1'
     m4t1 = Here.x - last arrow.start.x; m4t2 = Here.y - last arrow.start.y
     m4t3 = (ifelse(`$3',,arrowwid,`$3'))/vlength(m4t1,m4t2)/2
     line from last arrow.start to Here chop 0 chop ifelse(`$2',,arrowht,`$2')
     line from last arrow.end to Here+(-m4t2*m4t3,m4t1*m4t3) \
       then to Here-(-m4t2*m4t3,m4t1*m4t3) then to last arrow.end ')

                                `elchop(E,A) chop for ellipses
                                 evaluates to "chop r" where r is the distance
                                 from the centre of ellipse E to the
                                 intersection of E with a line to location A'
define(`elchop',`chop 0.5 * `$1'.wid * `$1'.ht * sqrt(\
        ((`$2'.x-`$1'.x)^2 + (`$2'.y-`$1'.y)^2) /\
        ( ((`$2'.x-`$1'.x)*`$1'.ht)^2 + ((`$2'.y-`$1'.y)*`$1'.wid)^2 ) ) ')

                                `vlength(x,y) 2-D vector length'
define(`vlength',`sqrt((`$1')^2+(`$2')^2)')
                                `distance(Pos1,Pos2)' distance between positions
define(`distance',`vlength(`$1'.x-`$2'.x,`$1'.y-`$2'.y)')
                                `lin_leng(linear object)' calculates length
define(`lin_leng',`distance(`$1'.start,`$1'.end)')
                                `inner_prod(linear obj,linear obj)'
define(`inner_prod',`(sum_(dnl
  prod_(`$1'.end.x-`$1'.start.x,`$2'.end.x-`$2'.start.x),dnl
  prod_(`$1'.end.y-`$1'.start.y,`$2'.end.y-`$2'.start.y)))')
                                `Unit vector pair CCW perpendicular to linear
                                 object vperp(obj)'
define(`vperp',
 `define(`m4pdx',`(`$1'.end.x-`$1'.start.x)')dnl
  define(`m4pdy',`(`$1'.end.y-`$1'.start.y)')dnl
  -m4pdy/vlength(m4pdx,m4pdy),m4pdx/vlength(m4pdx,m4pdy)')
                                `Unit vector pair CCW perpendicular to line
                                 joining two named positions Vperp(Pos1,Pos2)'
define(`Vperp',
 `define(`m4pdx',`(`$2'.x-`$1'.x)')dnl
  define(`m4pdy',`(`$2'.y-`$1'.y)')dnl
  -m4pdy/vlength(m4pdx,m4pdy),m4pdx/vlength(m4pdx,m4pdy)')

                                `Equidist3(Pos1,Pos2,Pos3,Result)
                                 Result is the name of the point equidistant
                                 from Pos1, Pos2, Pos3; eg Equidist3(A,B,C,D)
                                    arc from A to C with .c at D'
define(`Equidist3',`
  M4tmp_P1: 0.5 between `$1' and `$2'
  M4tmp_T1: M4tmp_P1+(Vperp(`$1',`$2'))
  M4tmp_P2: 0.5 between `$2' and `$3'
  M4tmp_T2: M4tmp_P2+(Vperp(`$2',`$3'))
  `$4': intersect_(M4tmp_P1,M4tmp_T1,M4tmp_P2,M4tmp_T2)
  ')
                                `cintersect(Pos1,rad1,Pos2,rad2,[R])
                                 Upper (lower if arg5=R) intersection of
                                 circles at Pos1 and Pos2, radius rad1 and rad2'
define(`cintersect',
 `define(`m4cdx',`((`$3').x-(`$1').x)')dnl
  define(`m4cdy',`((`$3').y-(`$1').y)')dnl
  define(`m4cls',`(m4cdx^2+m4cdy^2)')dnl
  define(`m4cq',`((m4cls+(`$2')^2-(`$4')^2)/2)')dnl
  (m4cq/m4cls between `$1' and `$3') ifinstr(`$5',R,-,+)\
   (vscal_(sqrt(max(0,m4cls*(`$2')^2-m4cq^2))/m4cls,-m4cdy,m4cdx))')

                                `Convenience for points along a linear obj'
define(`along_',`between `$1'.start and `$1'.end')

                                `Convenience to draw the bounding box of an obj
                                 (default last []), e.g. showbox_(B,dotted)'
define(`showbox_',
 `{box wid ifelse(`$1',,last [],`$1').wid \
       ht ifelse(`$1',,last [],`$1').ht \
       at ifelse(`$1',,last [],`$1') `$2'}')

                                `use continue if dpic, otherwise line or other'
define(`contline',`ifdpic(continue,ifelse(`$1',,line,`$1'))')

                                `arcto(pos1,pos2,radius,[dashed|dotted])
                                 line toward pos1 with rounded corner toward
                                 pos2, similar to Postscript arcto.  A spline
                                 might be better for dotted or dashed lines.'
define(`arcto',
 `H_arcto: Here; X_arcto:`$1'; Y_arcto:`$2'
  U_arcto: X_arcto-(H_arcto.x,H_arcto.y)
  V_arcto: Y_arcto-(X_arcto.x,X_arcto.y)
  L_arcto: (vlength(U_arcto.x,U_arcto.y),vlength(V_arcto.x,V_arcto.y))
  S_arcto: (U_arcto.x*V_arcto.y-U_arcto.y*V_arcto.x, \
            U_arcto.x*V_arcto.x+U_arcto.y*V_arcto.y)
  if (S_arcto.x==0) && (S_arcto.y==0) then { S_arcto:(`$3',0) } \
  else { S_arcto:(`$3',atan2(S_arcto.x,S_arcto.y)) }
  a_arcto = S_arcto.x*tan(abs(S_arcto.y)/2)
  if S_arcto.x*S_arcto.y*L_arcto.x*L_arcto.y == 0 \
  then { TX_arcto: X_arcto; TY_arcto: X_arcto; C_arcto: X_arcto } \
  else {
    TX_arcto: a_arcto/L_arcto.x between X_arcto and H_arcto
    TY_arcto: a_arcto/L_arcto.y between X_arcto and Y_arcto
    C_arcto: TX_arcto + (vscal_(sign(S_arcto.y)*S_arcto.x/L_arcto.x,
       -U_arcto.y,U_arcto.x)) }
  line `$4' from H_arcto to TX_arcto
  if S_arcto.y >= 0 then {
        arc `$4' ccw rad `$3' to TY_arcto with .c at C_arcto } \
  else {arc `$4'  cw rad `$3' to TY_arcto with .c at C_arcto } ')

                               `fitcurve(V,n,[e.g. dotted],m (default 0))
                                Draw a spline through V[m],...V[n]
                                Works only with dpic (and n-m > 2):
                                The calculated control points P[i] satisfy
                                P[0] = V[0]
                                P[i-1]/8 + P[i]*3/4 + P[i+1]/8 = V[i]
                                P[n] = V[n]'
ifdpic(
`define(`fitcurve',`
                                # Fit curve to $1
 {[ m4fn = `$2'; m4fm = ifelse(`$4',,0,`$4')
  M4P_[0]: `$1'[m4fm]
  for i=m4fm+1 to m4fn-1 do { M4P_[i-m4fm]: `$1'[i]*(4/3) }
  M4P_[m4fn-m4fm]: `$1'[m4fn]
  n = m4fn-m4fm
                                # forward substitution
  M4P_[1]: M4P_[1]-M4P_[0]/6
  d[1] = 1
  for i = 2 to n-1 do {
    M4P_[i]: M4P_[i]-M4P_[i-1]/(d[i-1]*6)
    d[i] = 1-1/d[i-1]/36 }
                                # backward substitution
  for i= n-1 to 1 by -1 do {
    M4P_[i]: (M4P_[i]-M4P_[i+1]/6)/d[i] }
                                # draw using computed control points
    spline 0.55 `$3' from M4P_[0] to 11/32 between M4P_[0] and M4P_[1] \
       then to 5/32 between M4P_[1] and M4P_[2]
    for i=2 to n-2 do { continue to M4P_[i] }
    continue to 27/32 between M4P_[n-2] and M4P_[n-1] \
      then to 21/32 between M4P_[n-1] and M4P_[n] then to M4P_[n]
  ] with .M4P_[0] at `$1'[ifelse(`$4',,0,`$4')]} ') ')

                                `Sinusoids and lollipop signals
                                 Cosine( amplitude, freq, time, phase )'
define(`Cosine',`(`$1')*cos((`$2')*(`$3')ifelse((`$4'),(),,`+(`$4')'))')

                                `lpop(xcoord, ycoord, radius, fill, zeroht)
                                 for lollipop graphs'
define(`lpop',`dot(at (`$1',`$2'),`$3',`$4')
  line to (`$1',ifelse(`$5',,0,`$5')) chop ifelse(`$3',,`dotrad_',`$3') chop 0')

                                `sinc(x) the sinc function'
define(`sinc',`sin(max(pi_*abs(`$1'),1e-6))/max(pi_*abs(`$1'),1e-6)')

                              `sinusoid( amplitude, frequency, phase, tmin,tmax)
                               in the current direction (only with dpic), e.g.
                               sinusoid( 1,twopi_,0,0,1) with .Origin at (1,0)'
ifdpic(
define(`sinusoid',
 `[ Tmin: vec_((`$4'),0) ; Tmax: vec_((`$5'),0)
  if ((`$4') <= 0) && ((`$5') >= 0) then { Origin: (0,0) }
  h = `$5'-(`$4')
  tm = 2*(`$2')
  w0 = tm*(`$4') + 2*(`$3')
  h0 = -m4_t_fun(-1,h,(`$1')/2)/h;
  n = (int(h/(twopi_/(tm/2)))+1)*12
  for i = 0 to n do {
    t = `$4'+m4_t_fun(h0,h*i/n,(`$1')/2);
    M4S_[i]: vec_(t,Cosine(`$1',`$2',t,`$3')) }
  fitcurve(M4S_,n)
  ]')
  define(`m4_t_fun',
    `((`$1'+(`$3'))*(`$2')-(`$3')/tm*(sin(tm*(`$2')+w0)-sin(w0)))') ')

                              `def_bisect defines the pic procedure
                               bisect ( func, xmin, xmax, eps, result )
                               that finds a root of func(arg,value)
                               to precision eps in the interval (xmin,xmax)
                               by the method of bisection.  Example to find
                               the root of x^2-1 between 0 and 2:
                                 def_bisect
                                 `define' parabola { $2 = ($1)^2 - 1 }
                                 bisect( parabola, 0, 2, 1e-8, x )
                                 print x'
define(`def_bisect',`
`define bisect' { x_m_$`'1 = $`'2; x_M_$`'1 = $`'3
                  x_c_$`'1 = (x_m_$`'1+x_M_$`'1)/2
  if (abs(x_m_$`'1-x_M_$`'1) <= $`'4) then { $`'5 = x_c_$`'1 } else {
     $`'1(x_m_$`'1,f_m_$`'1)
     $`'1(x_c_$`'1,f_c_$`'1)
     if (sign(f_c_$`'1)==sign(f_m_$`'1)) then { 
       bisect($`'1,x_c_$`'1,x_M_$`'1,$`'4,$`'5) } \
     else {
       bisect($`'1,x_m_$`'1,x_c_$`'1,$`'4,$`'5) } }
  }')

                                Normally redefined in initialization macros:
ifelse(ifgpic(,F)`'ifpstricks(,F)`'ifmfpic(,F)`'ifpgf(,F)`'ifsvg(,F),,`
define(`thinlines_',`
command "\thinlines%"
')                              TeX commands are written on separate lines
define(`thicklines_',`
command "\thicklines%"
')')
                                Colour routines
                                `setrgb(red value, green value, blue value,
                                   [name]) define colour for lines and text'
define(`setrgb',`pushdef(`r_',`$1')pushdef(`g_',`$2')pushdef(`b_',`$3')dnl
pushdef(`m4cl_',ifelse(`$4',,lcspec,`$4'))dnl
ifelse(m4postprocessor,pstricks,
 `command sprintf("\definecolor{m4cl_}{rgb}{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)
  command sprintf("\color[rgb]{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)
  command "\psset{linecolor=m4cl_}%%" ',
m4postprocessor,svg,`
 command "<g ifelse(ifelse(`$5',,D,`ifinstr(`$5',D,D)'),D,
  `stroke=\"rgb(r_,g_,b_)\"') ifelse(ifelse(`$5',,F,`ifinstr(`%5',F,F)'),F,
  `fill=\"rgb(r_,g_,b_)\"')>"',
m4postprocessor,pgf,`
  command sprintf("\definecolor{m4cl_}{rgb}{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)
  command sprintf("\color[rgb]{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)
  ifdef(`m4pco',,
  `command "\global\let\dpiclidraw=\dpicdraw\global\let\dpicfidraw=\filldraw"
   command "\global\def\dpicdraw{\dpiclidraw[color=m4cl_]}"
   command "\global\def\filldraw{\dpicfidraw[color=m4cl_]}"pushdef(`m4pco')')',
m4postprocessor,mpost,`
  command "def drw=draw enddef; def X=lcolr;enddef;"
  command sprintf("def m4cl_=(%7.5f,%7.5f,%7.5f)enddef;",r_,g_,b_)
  command "def lcolr=withcolor m4cl_ enddef;"'
)')
                                `resetrgb: reset to previous colour definitions'
define(`resetrgb',`popdef(`m4cl_')popdef(`r_')popdef(`g_')popdef(`b_')dnl
ifelse(m4postprocessor,pstricks,`ifdef(`r_',
 `command sprintf("\definecolor{m4cl_}{rgb}{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)
  command "\color[rgb]{r_,g_,b_}%"
  command sprintf("\color[rgb]{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)
  command "\psset{linecolor=m4cl_}%%" ',
 `command "\psset{linecolor=black}\color{black}%"') ',
m4postprocessor,svg,`
  command "</g>" ',
m4postprocessor,pgf,` popdef(`m4pco')
  ifdef(`m4pco',
   `command sprintf("\definecolor{lcspec}{rgb}{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)
    command sprintf("\color[rgb]{%7.5f,%7.5f,%7.5f}%%",r_,g_,b_)',
   `command "\color{black}\global\let\dpicdraw=\dpiclidraw%"
    command "\global\let\filldraw=\dpicfidraw"') ',
m4postprocessor,mpost,`
  command "def X=;enddef; def lcolr= enddef;"'
)')
                                `rgbfill(color triple, closed path)'
define(`rgbfill',`
ifelse(m4postprocessor,pstricks,
 `command sprintf("\definecolor{fcspec}{rgb}{%7.5f,%7.5f,%7.5f}%%",\
    `$1',`$2',`$3')
  command "\pscustom[fillcolor=fcspec`,'fillstyle=solid]{%"
  shift(shift(shift($@)))
  command "}%"',
m4postprocessor,svg,
 `command sprintf("<g fill=\"rgb(%g,%g,%g)\">",int(`$1'),int(`$2'),int(`$3'))
  shift(shift(shift($@)))
  command "</g>"',
m4postprocessor,pgf,
 `command sprintf("\definecolor{fcspec}{rgb}{%7.5f,%7.5f,%7.5f}%%",\
    `$1',`$2',`$3')
  command "\global\def\dpicstop{--}"
  command "\global\let\dpicfdraw=\dpicdraw\global\def\dpicdraw{}"
  ifdef(`m4pco',
   `command "\draw[line width=\dpiclw][color=lcspec][fill=fcspec]"',
   `command "\dpicfdraw[fill=fcspec]"')
  shift(shift(shift($@)))
  command "cycle; \global\let\dpicdraw=\dpicfdraw\global\def\dpicstop{;}"',
m4postprocessor,mpost,
 `command "def Y="
  shift(shift(shift($@)))
  command "enddef; def drw= enddef; def X=--enddef;"
  command sprintf("fill Y cycle withcolor (%7.5f,%7.5f,%7.5f);",`$1',`$2',`$3')
  command "def drw=draw enddef; def X=;enddef; Y;"'
)')

                                `rgbdraw(color triple, drawing commands)'
define(`rgbdraw',`setrgb(`$1',`$2',`$3',,ifsvg(D))
shift(shift(shift($@)))
resetrgb
')

                                Pstricks conditional command
define(`psset_',`ifpstricks(`dnl
\psset{$@}%
')')
                                Adjust fill value if gpic is used
define(`fill_',`dnl
 fill ifgpic(`1-(')ifelse(`$1',,fillval,`$1') ifgpic(`)')')

                                Initialization
define(`m4dir',right)
define(`m4_k',0)
                               `Set \psbezier to use all coordinate pairs
                                inside \pscustom'
define(`M4PatchPSTricks',`command "\makeatletter%
\@ifundefined{ifPst@noCurrentPoint}{%
  \@ifundefined{MPS@PatchA}{\gdef\MPS@PatchA{}%
    \typeout{ Dpic -p: patching psbezier in pstricks.tex (some versions) }%
    \def\psbezier@ii{\addto@pscode{%
    \ifshowpoints true \else false \fi\tx@OpenBezier%
    \ifshowpoints\tx@BezierShowPoints\fi}\end@OpenObj}}{}}%
{\@ifundefined{MPS@PatchB}{\gdef\MPS@PatchB{}%
  \typeout{ Dpic -p: Use all psbezier coordinate pairs }%
  \psset{noCurrentPoint}}{}}%
\makeatother%"')

define(`gen_init',`dnl Do not change the format of the next line:
`#' `$0' Version 6.83: ifelse(m4picprocessor,gpic,`Gpic',
  m4postprocessor,pstricks,`PSTricks',
  m4postprocessor,pgf,`TikZ PGF',
  m4postprocessor,mfpic,`Mfpic',
  m4postprocessor,mpost,`MetaPost',
  m4postprocessor,xfig,`Xfig',
  m4postprocessor,svg,`SVG',
  m4postprocessor,postscript,`Postscript',`Unknown') m4 macro settings used.

  define(`if_rpoint__')`define rpoint__' {
    rp_wid = last line.end.x-last line.start.x
    rp_ht = last line.end.y-last line.start.y
    rp_len = vlength(rp_wid,rp_ht); move to last line.start
    ifgpic(`if (rp_len == 0) then { rp_ang=0 } else {')dnl
    rp_ang = atan2(rp_ht,rp_wid)ifgpic(` }') }

  rp_ang = 0
  define(`rtod__',`57.295779513082323')`rtod_' = rtod__
  define(`dtor__',`0.017453292519943295')`dtor_' = dtor__
  define(`twopi__',`6.2831853071795862')`twopi_' = twopi__
  define(`pi__',`(twopi_/2)')`pi_' = pi__

  ifpostscript(`command " 0.8 setlinewidth"',`thicklines_')
  ifpstricks(M4PatchPSTricks)
dnl                             Insert customizations as desired,
dnl                             e.g.  ifmpost(`  ...  ')  etc
  ifdef(`local_init',`local_init')
`# gen_init end'
')

                                Define m4x_ etc for horiz and vert lines
manhattan
divert(0)dnl
