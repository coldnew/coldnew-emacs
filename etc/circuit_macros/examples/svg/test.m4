divert(-1)
  libcct.m4

* Circuit_macros Version 6.94, copyright (c) 2011 J. D. Aplevich, under    *
* the LaTeX Project Public License. The files of this distribution may    *
* be redistributed or modified, provided that this copyright notice is    *
* included and provided that modifications are clearly marked to          *
* distinguish them from this distribution.  There is no warranty          *
* whatsoever for these files.                                             *

                                Enter the installation directory path in this
                                definition of HOMELIB_.  You can set this to
                                the null string if you use an environment
                                variable to tell m4 where to search:
ifdef(`HOMELIB_',,
#`define(`HOMELIB_',`/u/aplevich/lib/')')dnl For Unix-like systems
 `define(`HOMELIB_',`/cygdrive/c/Dwight/lib/')')dnl for new Cygwin
#`define(`HOMELIB_',`')')dnl for bash users, but define M4PATH in your bashrc
#`define(`HOMELIB_',`C:\Dwight\lib\')')dnl for PCs without M4PATH defined

                                Default pic processor: gpic.  To make dpic -p
                                the default, change gpic to pstricks in the
                                line below. To make dpic -g and TikZ-PGF the
                                default, change gpic to pgf:
define(`m4defaultprocessor',gpic)

ifdef(`m4picprocessor',,`include(HOMELIB_`'m4defaultprocessor.m4)divert(-1)')

`==============================================================================
HINTS:

THE ARGUMENTS of circuit elements are optional; if omitted, default values
   are assumed.

TWO-TERMINAL ELEMENTS are constructed as follows, with variations:

   # Draw the initial invisible line (default length rp_len), and set the
   #   direction cosines:
   eleminit_(`$1')

   # Element body height and width
   define(`m4v',...)define(`m4h',...)

   # Visible lines:
   { line to rvec_(rp_len/2-m4h/2,0)
     (element body lines)
     line to rvec_(rp_len/2-m4h/2,0) }

   # The invisible body block:
   {[box invis ht_ m4v wid_ m4h ] at rvec_(rp_len/2,0)}

   # The final invisible line:
   line to rvec_(rp_len,0) invis

A side effect of the eleminit_ macro is to change the current drawing angle.

==============================================================================

NON-TWO-TERMINAL ELEMENTS are usually constructed within a block:

   set dimension parameters
   [ set size and direction from the initial linespec argument
     set orientation
     draw internal elements
     define internal locations
     ]

   If there is a linespec argument, it determines orientation
   but not the placement of the element, since [] blocks are placed
   as if they were boxes.

==============================================================================

CUSTOMIZATIONS:
   The non-two-terminal circuit elements enclosed in [] blocks allow
   a final argument not shown in the argument list.  The last argument is
   expanded just before exit from the [] block to allow custom named additions
   to the elements.

   Subcomponents of a circuit element are drawn selectively according
   to a "dna_" string and a sequence of calls to sc_draw(`dna_',arg2,arg3,arg4).
   If the second argument of sc_draw is a substring of the first, it is deleted
   from the first and the third argument is expanded, otherwise the fourth
   argument is expanded when present.

==============================================================================

DEBUGGING: The statement
     print "`$0'($@)" ;
   inserted into a macro will display the macro name and current arguments

==============================================================================
 This file redefines default arrow dimensions and the dot macro.
=============================================================================='

                                `capacitor( linespec,[char][+],R )
                                 char:
                                  F or none: flat plates
                                  C = polarized, curved plate
                                  P = alternate polarized
                                  E = polarized rectangular plates
                                  K = filled rectangular plates
                                  + = added polarity sign;
                                 arg3 = R: reversed orientation'
define(`capacitor',
 `ifelse(`$3',R,`reversed(`capacitor',`$1',`$2',,shift(shift(shift($@))))',
 `eleminit_(`$1')
  define(`m4ctype',ifelse(`$2',,F,`$2',+,F,`$2'))m4_dna(`m4ctype',+)dnl
  { ifelse(m4ctype,F,
   `define(`m4ht',`dimen_/3')define(`m4wd',`m4ht*0.3')dnl
      line to rvec_(rp_len/2-m4wd/2,0)
      {line from rvec_(0,-m4ht/2) to rvec_(0,m4ht/2)}
      move to rvec_(m4wd,0)
      {line from rvec_(0,-m4ht/2) to rvec_(0,m4ht/2)}
      line to rvec_(rp_len/2-m4wd/2,0) ',
  m4ctype,C,
   `define(`m4ht',`dimen_/4')define(`m4wd',`m4ht*0.4')dnl
    define(`m4cr',`dimen_*0.25')dnl
     line to rvec_(rp_len/2-m4wd/2,0)
     {line from rvec_(0,-m4ht/2) to rvec_(0,m4ht/2)}
     {arc cw from rvec_(m4wd,-m4ht/2) to rvec_(m4wd,m4ht/2) \
        with .c at rvec_(m4wd+sqrt((m4cr)^2-(m4ht/2)^2),0) }
     line from last arc.c+vec_(-m4cr,0) to rvec_(rp_len/2+m4wd/2,0) ',
  m4ctype,P,
   `define(`m4ht',`dimen_/4')define(`m4wd',`m4ht*0.4')dnl
    define(`m4cr',`dimen_*0.25')dnl
     line to rvec_(rp_len/2-m4wd/2,0)
     {line from rvec_(m4wd,-m4ht/2) to rvec_(0,-m4ht/2) \
        then to rvec_(0,m4ht/2) then to rvec_(m4wd,m4ht/2) }
     {line from rvec_(m4wd*2/3,-m4ht*3/8) to rvec_(m4wd*2/3,m4ht*3/8)}
     line from rvec_(m4wd*2/3,0) to rvec_(rp_len/2+m4wd/2,0) ',
  m4ctype,E,
   `define(`m4ht',`dimen_/5')define(`m4wd',`m4ht')define(`m4cs',`(m4wd/3.2)')dnl
     line to rvec_(rp_len/2-m4wd/2,0)
     m4linethicktemp = linethick; thinlines_
     { lbox(m4cs,m4ht) }
     move to rvec_(m4wd,0)
     {ifsvg(`lbox(-m4cs,m4ht,fill_(0))',`m4cshade lbox(-m4cs,m4ht))')}
     linethick_(m4linethicktemp)
     line to rvec_(rp_len/2-m4wd/2,0) ',
  m4ctype,K,
   `define(`m4ht',`dimen_/5')define(`m4wd',`m4ht')define(`m4cs',`(m4wd/3.2)')dnl
      line to rvec_(rp_len/2-m4wd/2,0)
      {ifsvg(`lbox(m4cs,m4ht,fill_(0))',`m4cshade lbox(m4cs,m4ht))')}
      move to rvec_(m4wd,0)
      {ifsvg(`lbox(-m4cs,m4ht,fill_(0))',`m4cshade lbox(-m4cs,m4ht))')}
      line to rvec_(rp_len/2-m4wd/2,0) ') }
  ifelse(index(`$2',+),-1,,
   `{ move to rvec_(rp_len/2-m4wd/2-m4ht/3,-m4ht/3)
      {line thick 0.5 from rvec_(m4ht/6,0) to rvec_(-m4ht/6,0)}
      line thick 0.5 from rvec_(0,m4ht/6) to rvec_(0,-m4ht/6)}')
  {[box invis ht_ m4ht wid_ m4wd ] at rvec_(rp_len/2,0)}
  line to rvec_(rp_len,0) invis ')')

                                `resistor( linespec, cycles|E, chars )
                                 arg2: E=default ebox (kept for compatibility)
                                 chars : E=ebox
                                         Q=offset
                                         H=squared (default 3 cycles)
                                         N=IEEE (default 3 cycles)
                                         R=to right of drawing direction'
define(`resistor',`ifelse(
 `$2',E,
   `ebox(`$1',shift(shift($@)))',
 `$3',E,
   `ebox(`$1',shift(shift(shift($@))))',
 `define(`dna_',ifelse(`$3',,N,`$3'))dnl
  eleminit_(`$1')
  sc_draw(`dna_',N,
   `define(`m4n',`ifelse(`$2',,6,`eval(2*($2))')')dnl
    define(`m4h',`dimen_/24')dnl
    define(`m4v',2)dnl
    if m4h*m4n*2 > rp_len then { eleminit_(to rvec_(m4h*m4n*2,0)) }
    tr_xy_init(last line.c, m4h, sc_draw(`dna_',R,-))dnl
    { line from last line.start to tr_xy(-m4n,0)\
      for_(2,m4n,2,
        `then to tr_xy(eval(2*m4x-3-m4n), m4v) \
         then to tr_xy(eval(2*m4x-1-m4n),-m4v) \')dnl
      then to tr_xy(m4n, 0) then to last line.end
      [box invis ht_ m4h*m4v*2 wid_ m4h*m4n*2] at 2nd last line.c
      }')
  sc_draw(`dna_',Q,
   `define(`m4n',`ifelse(`$2',,6,`eval(2*($2))')')dnl
    define(`m4h',`dimen_/24')dnl
    define(`m4v',2)dnl
    if m4h*m4n*2 > rp_len then { eleminit_(to rvec_(m4h*m4n*2,0)) }
    tr_xy_init(last line.c, m4h, sc_draw(`dna_',R,-))dnl
    { line from last line.start to tr_xy(-m4n,0)\
      for_(2,m4n,2,
       `then to tr_xy(eval(2*m4x-2-m4n), m4v*2) \
        then to tr_xy(eval(2*m4x-m4n),0) \')dnl
      then to tr_xy(m4n, 0) then to last line.end
      [box invis ht_ m4h*m4v*2 wid_ m4h*m4n*2] at 2nd last line.c + ta_xy(0,m4v)
      }')
  sc_draw(`dna_',H,
   `define(`m4n',`ifelse(`$2',,5,`eval(2*($2)-1)')')dnl
    define(`m4h',`dimen_/20')dnl
    define(`m4v',7/3)dnl
    if m4h*m4n*2 > rp_len then { eleminit_(to rvec_(m4h*m4n*2,0)) }
    tr_xy_init(last line.c, m4h, sc_draw(`dna_',R,-))dnl
    { line from last line.start to tr_xy(-m4n,0)\
      for_(-m4n,m4n,2,
       `ifelse(eval(((m4x+m4n)/2)%2),0,
         `then to tr_xy(m4x,m4v) then to tr_xy(eval(m4x+2),m4v) \',
         `then to tr_xy(m4x,0) \
          ifelse(m4x,m4n,,`then to tr_xy(eval(m4x+2),0)')\')')dnl
      then to tr_xy(m4n, 0) then to last line.end
      [box invis ht_ m4h*m4v wid_ m4h*m4n*2] at 2nd last line.c+(0,m4h*m4v/2)
      }')
  line invis from 2nd last line.start to 2nd last line.end ')')

                         `potentiometer(linespec, cycles,
                            fractional pos, length, fractional pos, length,...)
                          Resistor in a block, tapped at fractional positions
                          with specified (possibly negative) arrow lengths.
                          Taps are labelled T1, T2, ...
                          Uses side effects of resistor macro'
define(`potentiometer',`[R: resistor(`$1',`$2'); Start: R.start; End: R.end
  m4pot_arrows(1,ifelse(`$3',,0.5,`$3'),ifelse(`$4',,`dimen_*5/12',`$4'),
  shift(shift(shift(shift($@)))))] ')
define(`m4pot_arrows',`ifelse(`$2',,,`
  x = (`$2')*2*m4n+1; x = (int(x)%4)+(x-int(x))
  M4_Tmp: `$2' between last [].c-vec_(prod_(m4n,m4h),0) \
    and last [].c+vec_(prod_(m4n,m4h),0)
  T`$1': M4_Tmp + vec_(0,ifelse(`$3',,`dimen_*5/12',`$3'))
  { arrow from T`$1' to M4_Tmp+vec_(0,m4h*m4v*(1-(x-2)*sign(x-2))) }
  m4pot_arrows(incr($1),shift(shift(shift($@))))')')

                         `b_current( label, above_|below_, OUT, S|E, frac )
                          Branch current for last-drawn element.  The arrowhead
                          is drawn frac (default 2/3) of the way between
                          the line end and element body.'
define(`b_current',
 `define(`m4y',`ifelse(`$5',,2/3,`($5)')')dnl
  define(`m4v',`ifinstr(`$4',E,
   `ifinstr(`$3',O,-)',`ifinstr(`$3',O,,-)')arrowht')dnl
  define(`m4h',`(rp_len-last [].wid_)/2')
  { move to last line.start+vec_(ifinstr(`$4',E,`rp_len-')dnl
    ifinstr(`$3',O,`(m4h-arrowht)*m4y',`(m4h*m4y+arrowht/3)'),0)
    arrow <- to rvec_(m4v,0) ifelse(`$1',,,
   `m4lstring(`$1',"ifsvg(`svg_it(` $1 ')',`sp_$ `$1'$sp_')") \
     ifelse(`$2',,`above_',`$2')')}')

                                `larrow( label, <-, separation )
                                 Arrow alongside the left of the last-drawn
                                 element'
define(`larrow',`define(`m4h',`min(lin_leng(last line),linewid)/2')dnl
 define(`m4v',`ifelse(`$3',,`4pt__',`($3)')')dnl
 {arrow `$2' from last [].n_+vec_(-m4h,m4v) to last [].n_+vec_(m4h,m4v) \
  m4lstring(`$1',"ifsvg(`svg_it(` $1 ')',`sp_$ `$1'$sp_')") above_}')

                                `rarrow( label, <-, separation )
                                 Arrow alongside the right of the last-drawn
                                 element'
define(`rarrow',`define(`m4h',`min(lin_leng(last line),linewid)/2')dnl
 define(`m4v',`ifelse(`$3',,`4pt__',`($3)')')dnl
 {arrow `$2' from last [].s_+vec_(-m4h,-m4v) to last [].s_+vec_(m4h,-m4v) \
  m4lstring(`$1',"ifsvg(`svg_it(` $1 ')',`sp_$ `$1'$sp_')") below_}')

                                `inductor( linespec,W|L,cycles,M )'
define(`inductor',`eleminit_(`$1')
 define(`m4di',`dimen_'/10) define(`m4n',`ifelse(`$3',,4,`$3')')dnl
 ifelse(`$2',W,
  `m4Inductor(`$4')',
 `$2',L,
  `m4Lnductor(`$4')',
  `m4inductor(`$4')')
 ifelse(`$4',M,`m4m_core(rvec_(rp_len/2,0),m4wd,m4ht+dimen_/24,dimen_/16)
   define(`m4hs',`(dimen_/24+dimen_/16)')',`define(`m4hs',0)')
 {[box invis ht_ m4ht+m4hs+m4dp wid_ m4wd] \
   at rvec_(rp_len/2,(m4ht+m4hs-m4dp)/2)}
 line to rvec_(rp_len,0) invis ')
                                `Wide (iron-core) inductor'
define(`m4Inductor',`define(`m4ht',`(1+m4st)*m4di')dnl
 define(`m4dp',`(m4s2t-m4st)*m4di')define(`m4wd',((2*m4n-2)*m4c2t+2)*m4di)dnl
 define(`m4lth4',`(linethick/4 bp__)')dnl
 { line to rvec_(rp_len/2-((m4n-1)*m4c2t+m4ct)*m4di,0)
   { arc ccw thick linethick/2 from rvec_(0,-m4lth4) \
       to rvec_(vscal_(m4lth4,m4ct,m4st)) with .c at Here }
   arc cw from Here to rvec_(vscal_(m4di,m4ct+m4c2t,m4st-m4s2t)) \
      with .c at rvec_(vscal_(m4di,m4ct,m4st))
   { arc cw thick linethick/2 from rvec_(vscal_(m4lth4,m4c2t,-m4s2t)) \
       to rvec_(vscal_(m4lth4,-m4c2t,-m4s2t)) with .c at Here }
   for m4i=3 to m4n do { arc cw from Here to rvec_(vscal_(m4di,2*m4c2t,0)) \
      with .c at rvec_(vscal_(m4di,m4c2t,m4s2t))
      { arc cw thick linethick/2 from rvec_(vscal_(m4lth4,m4c2t,-m4s2t)) \
          to rvec_(vscal_(m4lth4,-m4c2t,-m4s2t)) with .c at Here } }
   arc cw from Here to rvec_(vscal_(m4di,m4ct+m4c2t,m4s2t-m4st)) \
      with .c at rvec_(vscal_(m4di,m4c2t,m4s2t))
   { arc cw thick linethick/2 from rvec_(0,-m4lth4) \
       to rvec_(vscal_(m4lth4,-m4ct,m4st)) with .c at Here }
   line to rvec_(rp_len/2-((m4n-1)*m4c2t+m4ct)*m4di,0) } ')
define(`m4ct',`Cos(25)')define(`m4st',`Sin(25)')
define(`m4c2t',`Cos(50)')define(`m4s2t',`Sin(50)')
                                `Looped inductor'
define(`m4Lnductor',`define(`m4ht',`dimen_/8')dnl
 define(`m4dp',`m4di/2')define(`m4wd',(m4n+1)*m4di)dnl
 define(`m4lth4',`(linethick/4 bp__)')dnl
 { line to rvec_(rp_len/2-m4wd/2,0)
   { arc ccw thick linethick/2 from rvec_(0,-m4lth4) to rvec_(m4lth4,0) \
     with .c at Here }
   spline ifdpic(0.5523) to rvec_(0,m4ht) for_(1,m4n,1,`\
     then to rvec_((m4x+0.3)*m4di, m4ht) then to rvec_((m4x+0.3)*m4di,-m4dp) \
     then to rvec_((m4x-0.3)*m4di,-m4dp) then to rvec_((m4x-0.3)*m4di, m4ht)\')\
     then to rvec_(m4wd,m4ht) then to rvec_(m4wd,0)
   { arc ccw thick linethick/2 from rvec_(-m4lth4,0) to rvec_(0,-m4lth4) \
     with .c at Here }
   line to rvec_(rp_len/2-m4wd/2,0) }')
                                `Narrow inductor'
define(`m4inductor',`define(`m4ht',`dimen_/16')dnl
 define(`m4dp',0)define(`m4wd',m4n*m4ht*2)dnl
 ifelse(ifpstricks(T)`'ifmpost(T)`'ifpgf(T)`'ifsvg(T),T,
  `define(`m4y')',`undefine(`m4y')')dnl
 { line to rvec_((rp_len-m4wd)/2,0)
   ifdef(`m4y',`{line to rvec_(0,-linethick/2 bp__)};')dnl
   for m4i=1 to m4n do {
     arc cw from Here to rvec_(m4ht*2,0) with .c at rvec_(m4ht,0)
     ifdef(`m4y',`{line to rvec_(0,-linethick/2 bp__)}') }
   line to rvec_((rp_len-m4wd)/2,0)} ')

                               `m4m_core(bottom ctr,length,ht offset,separation)
                                 Two lines for the metal core'
define(`m4m_core',`dnl
  {M4Core1: line from `$1'+vec_(-(`$2')/2,`$3') to `$1'+vec_( (`$2')/2,`$3')
   M4Core2: line from M4Core1.start+vec_(0,`$4') to M4Core1.end+vec_(0,`$4')}')

                                `transformer( linespec, L|R, np, [A][W|L], ns )
                                 2-winding transformer:
                                 np = number of primary arcs
                                 A = air core W = wide windings; L = looped
                                 ns = number of secondary arcs'
define(`transformer', `[ P1: Here
 define(`m4WL',`ifinstr(`$4',W,W,`ifinstr(`$4',L,L)')')dnl
 define(`m4np',`ifelse(`$3',,4,(`$3'))')dnl
 define(`m4ns',`ifelse(`$5',,4,(`$5'))')dnl
 move ifelse(`$1',,
 `to rvec_(max(ifelse(m4WL,W,`dimen_/5*((m4np-1)*m4c2t+m4ct)',`m4np*dimen_/8'),\
               dimen_*2/3), 0 ) ',
 `$1' )
 P2: Here
  L1: inductor(from ifelse(`$2',R,`P2 to P1',`P1 to P2'),m4WL,`$3')
  define(`m4t',`ifelse(m4WL,W,((2*m4ns-2)*m4c2t+2)*m4di,m4WL,L,(m4ns+1)*m4di,
                       m4ns*m4ht*2)')dnl
  ifinstr(`$4',A,
   `move to last line.c+vec_(0,m4ht*ifelse(m4WL,W,3,m4WL,L,3,4))',
   `m4m_core(rvec_(-(rp_len/2),0),max(m4wd,m4t),m4ht+dimen_/12,dimen_/8)
    move to last line.c+vec_(0,m4ht+dimen_/12)')
 TS: Here
 S2: rvec_( ifelse(`$2',R,-)(ifelse(`$5',,rp_len/2,m4t/2)), 0 )
 S1: 2 between S2 and Here
  L2: inductor(from ifelse(`$2',R,`S1 to S2',`S2 to S1'),m4WL,`$5')
 TP: 0.5 between P1 and P2; `$6'
  manhattan ]')

                                `delay( linespec, width )'
define(`delay',`eleminit_(`$1')
 define(`m4ht',`ifelse(`$2',,`delay_rad_*2',`($2)')')dnl
 define(`m4wd',`m4ht*5/6')dnl
 { line to rvec_(rp_len/2-m4wd/2,0)
   { line from rvec_(m4ht/3,-m4ht/2) to rvec_(0,-m4ht/2)\
       then to rvec_(0,m4ht/2) then to rvec_(m4ht/3,m4ht/2)
     arc cw from Here to rvec_(0,-m4ht) with .c at rvec_(0,-m4ht/2) }
   move to rvec_(m4wd,0)
   line to rvec_(rp_len/2-m4wd/2,0) }
 { [box invis ht_ m4ht wid_ m4wd ] at rvec_(rp_len/2,0)}
 line to rvec_(rp_len,0) invis ')

                                `crystal xtal( linespec )'
define(`xtal',`eleminit_(`$1')
 define(`m4ht',`dimen_/4')define(`m4wd',`m4ht*2/3')define(`m4cs',`m4ht/3')dnl
 { line to rvec_(rp_len/2-m4wd/2,0)
   {line from rvec_(0,-m4ht/3) to rvec_(0,m4ht/3)}
   { move to rvec_(m4wd/2-m4cs/2,0)
     line to rvec_(0,m4ht/2) then to rvec_(m4cs,m4ht/2) \
       then to rvec_(m4cs,-m4ht/2) then to rvec_(0,-m4ht/2) then to Here }
   move to rvec_(m4wd,0)
   {line from rvec_(0,-m4ht/3) to rvec_(0,m4ht/3)}
   line to rvec_(rp_len/2-m4wd/2,0) }
 {[box invis ht_ m4ht wid_ m4wd ] at rvec_(rp_len/2,0)}
 line to rvec_(rp_len,0) invis ')

                    `source( linespec,
                             V|v|I|i|AC|F|G|Q|L|P|R|S|T|X|U|string,
                             diameter, R)
                     V = voltage source; v=alternate voltage source;
                     I = current source; i = alternate current source;
                     AC = AC source, F = fluorescent, G = generator,
                     Q = charge, L = lamp, P = pulse, R = ramp, S = sinusoid,
                     T = triangle, X = interior X, U = square-wave,
                     string = interior label,
                     R = reversed polarity
                     arg 5 can be used to modify the circle with e.g. color
                       or fill'
define(`source',`ifelse(`$4',R,
 `reversed(`source',`$1',`$2',`$3',,shift(shift(shift(shift($@)))))',
 `eleminit_(`$1')
 define(`m4h',ifelse(`$3',,`sourcerad_',`($3)/2'))dnl
 ifelse(
 `$2',G,`m4_sourceGQ($@)',
 `$2',Q,`m4_sourceGQ($@)',
 `{ line to rvec_(rp_len/2-m4h,0)
    move to rvec_(m4h,0)
  { circle rad m4h `$5' at Here }
  ifelse(`$2',,,
  `$2',F,`{ line from rvec_(-m4h,0) to rvec_(-m4h/2,0)}
          { line from rvec_(-m4h/2,-m4h/2) to rvec_(-m4h/2,m4h/2)}
          { line from rvec_(m4h/2,-m4h/2) to rvec_(m4h/2,m4h/2)}
          { line from rvec_(m4h,0) to rvec_(m4h/2,0)}',
  `$2',I,`{arrow from rvec_(-m4h*3/4,0) to rvec_(m4h*3/4,0)}',
  `$2',i,`{line from rvec_(0,-m4h) to rvec_(0,m4h)}',
  `$2',L,`{line from rvec_(-m4h,0) to rvec_(-m4h/4,0)
           spline to rvec_(m4h/12,m4h*2/3) \
             then to rvec_(m4h*5/12,m4h*2/3) \
             then to rvec_(m4h/2,0); line to rvec_(m4h*3/4,0)}',
  `$2',V,`{"ifsvg(-,`$-$')" at rvec_(-m4h/2,0) ifsvg(+(0,textht/10))}
          {"ifsvg(+,`$+$')" at rvec_( m4h/2,0) ifsvg(+(0,textht/10))}',
  `$2',v,`{line from rvec_(-m4h,0) to rvec_(m4h,0)}',
  `$2',AC,`{
    arc rad m4h/3 cw from Here-(m4h*2/3,0) to Here with .c at Here-(m4h/3,0)
    arc rad m4h/3 ccw from Here to Here+(m4h*2/3,0) with .c at Here+(m4h/3,0)}',
  `$2',P,`{ line from Here+(-m4h/2,-m4h/4) to Here+(-m4h/4,-m4h/4) \
            then to Here+(-m4h/4,m4h/4) then to Here+(m4h/4,m4h/4) \
            then to Here+(m4h/4,-m4h/4) then to Here+(m4h/2,-m4h/4) }',
  `$2',U,`{ line from Here+(-m4h/2,0) to Here+(-m4h/2,m4h/3) \
            then to Here+(0,m4h/3) then to Here+(0,-m4h/3) \
            then to Here+(m4h/2,-m4h/3) then to Here+(m4h/2,0) }',
  `$2',R,`{ line from Here+(-m4h*2/3,-m4h/3) to Here+(m4h/3,m4h/2) \
              then to Here+(m4h/3,-m4h/3) }',
  `$2',S,`{ ifgpic(
    `arc rad m4h/3 cw from Here-(m4h*2/3,0) to Here with .c at Here-(m4h/3,0)
     arc rad m4h/3 ccw from Here to Here+(m4h*2/3,0) with .c at Here+(m4h/3,0)',
    `m4smp_ang = rp_ang; rp_ang = 0
     sinusoid(m4h/2,twopi_/(m4h),pi_/2,-m4h/2,m4h/2) with .Origin at Here
     rp_ang = m4smp_ang') }',
  `$2',T,`{ line from Here+(-m4h*3/4,-m4h/4) to Here+(-m4h/4,m4h/4) \
              then to Here+(m4h/4,-m4h/4) then to Here+(m4h*3/4,m4h/4) }',
  `$2',X,`define(`m4v',`m4h/sqrt(2)')dnl
    {line from rvec_(-m4v,m4v) to rvec_(m4v,-m4v)}
    {line from rvec_(-m4v,-m4v) to rvec_(m4v,m4v)}',
  `{$2}' )
  line from rvec_(m4h,0) to rvec_(rp_len/2,0)} ')
  { [box invis wid m4h*2 ht m4h*2] at rvec_(rp_len/2,0) }
  line to rvec_(rp_len,0) invis ')')

define(`m4_sourceGQ',dnl `Internal to source macro'
`{ line to rvec_(rp_len/2-3/2*m4h,0)
  {[C1: circle rad m4h
    C2: ifelse(`$2',G,
   `circle rad m4h',
   `arc rad m4h from C1 +vec_(m4h/2,-m4h*sqrt(3)/2) \
                  to C1 +vec_(m4h/2, m4h*sqrt(3)/2) ') \
      with .c at C1 +vec_(m4h,0)] at rvec_(m4h*3/2,0)}
  line from rvec_(m4h*3,0) to rvec_(rp_len/2+3/2*m4h,0) } ')

                    `ttmotor( linespec, string, diameter, brushwid, brushht )'
define(`ttmotor',`eleminit_(`$1')
  define(`m4r',ifelse(`$3',,`sourcerad_',`($3)/2'))dnl
  define(`m4w',ifelse(`$4',,`sourcerad_/4',`($4)'))dnl
  define(`m4h',ifelse(`$5',,`sourcerad_/2',`($5)'))dnl
  define(`m4cr',`(m4r-sqrt(max(m4r*m4r-m4h*m4h/4,0)))')dnl
  { line to rvec_(max(rp_len/2-m4r-m4w,0),0)
    { line from rvec_(m4w+m4cr,m4h/2) to rvec_(0,m4h/2) \
        then to rvec_(0,-m4h/2) then to rvec_(m4w+m4cr,-m4h/2) }
    { circle rad m4r at rvec_(m4w+m4r,0) ifelse(`$2',,,`m4lstring($2,"$2")') }
    move to rvec_((m4w+m4r)*2,0)
    { line from rvec_(-m4w-m4cr,m4h/2) to rvec_(0,m4h/2) \
        then to rvec_(0,-m4h/2) then to rvec_(-m4w-m4cr,-m4h/2) }
    line to rvec_(max(rp_len/2-m4r-m4w,0), 0) }
  { [box invis ht_ m4r*2 wid_ (m4r+m4w)*2] at last circle.c }
  line to rvec_(rp_len,0) invis ')

                                `Controlled source
                                 consource( linespec ,V|I|v|i, R )
                                 arg 4 can be used to modify the body with e.g.
                                   color or fill'
define(`consource',`ifelse(`$3',R,
 `reversed(`consource',`$1',`$2',,shift(shift(shift($@))))',
 `eleminit_(`$1')
  {line to rvec_(rp_len/2-csdim_,0)
      {line to rvec_(csdim_,csdim_) then to rvec_(2*csdim_,0)\
       then to rvec_(csdim_,-csdim_) then to Here `$4' }
   ifelse(`$2',I,
    `{arrow from rvec_(csdim_/4,0) to rvec_(csdim_*7/4,0)}',
   `$2',i,
    `{line from rvec_(csdim_,csdim_) to rvec_(csdim_,-csdim_)}',
   `$2',V,
    `{"ifsvg(-,`$-$')" at rvec_(csdim_*0.5,0) ifsvg(+(0,textht/10))}
     {"ifsvg(+,`$+$')" at rvec_(csdim_*1.5,0) ifsvg(+(0,textht/10))}',
   `$2',v,
    `{line to rvec_(2*csdim_,0)} ')
   line from rvec_(2*csdim_,0) to rvec_(rp_len/2+csdim_,0) }
  {[box invis ht_ 2*csdim_ wid_ 2*csdim_] at rvec_(rp_len/2,0)}
  line to rvec_(rp_len,0) invis ')')

                                `battery( linespec, n, R )
                                 Arg 3: reversed polarity'
define(`battery',`ifelse(`$3',R,
 `reversed(`battery',`$1',`$2',,shift(shift(shift($@))))',
 `eleminit_(`$1')
  define(`m4n',`ifelse(`$2',,1,(`$2'))')define(`m4cs',`dimen_/12')dnl
  define(`m4wd',`m4cs*(m4n*2-1)')define(`m4ht',`dimen_/2')dnl
  { line to rvec_(rp_len/2-m4wd/2,0)
    for m4i = 0 to 2*(m4n-1) by 2 do {
      { line from rvec_(m4i*m4cs,m4ht/4) \
        to rvec_(m4i*m4cs,-m4ht/4) }
      { line from rvec_((m4i+1)*m4cs,m4ht/2) \
        to rvec_((m4i+1)*m4cs,-m4ht/2) } }
    line from rvec_(m4wd,0) to rvec_(rp_len/2+m4wd/2,0) }
  {[box invis ht_ m4ht wid_ m4wd] at rvec_(rp_len/2,0)}
  line to rvec_(rp_len,0) invis ')')

                                `ebox(linespec, wid, ht, greyvalue)
                                 Box element'
define(`ebox',`eleminit_(`$1')
   define(`m4wd',ifelse(`$2',,`dimen_/2',`($2)'))dnl
   define(`m4ht',ifelse(`$3',,`dimen_/5',`($3)'))dnl
   {line to rvec_(max(0,rp_len/2-m4wd/2),0)
    ifsvg(`lbox(m4wd,m4ht,ifelse(`$4',,,`fill $4'))',
    `ifelse(`$4',,,`m4cshade(`$4')') lbox(m4wd,m4ht) ifelse(`$4',,,`)')')
    line to rvec_(max(0,rp_len/2-m4wd/2),0)}
  {[box invis ht_ m4ht wid_ m4wd] at rvec_(rp_len/2,0)}
   line to rvec_(rp_len,0) invis ')

                                `fuse( linespec, chars, wid, ht )
                                 chars A|B|C|D|S|HB|HC or dA=D'
define(`fuse',`eleminit_(`$1')
  define(`m4fusetype',`ifelse(`$2',,A,`$2',D,dA,`$2')')dnl
  define(`m4ht',ifelse(`$4',,`dimen_/5'ifinstr(`$2',H,*5/3),`($4)'))dnl
  define(`m4d',ifinstr(`$2',H,`m4ht/5',0))dnl
  define(`m4wd',ifelse(`$3',,`m4ht*2',`($3)'))dnl
  {line to rvec_(max(0,rp_len/2-m4wd/2),0)
  sc_draw(`m4fusetype',HB,
   `{move to rvec_(m4d,0); lbox(m4wd-2*m4d,m4ht-2*m4d)}
    {lbox(m4wd,m4ht)}
    line to rvec_(m4wd+max(0,rp_len/2-m4wd/2),0)}')
  sc_draw(`m4fusetype',HC,
   `{move to rvec_(m4d,0); {lbox(m4wd-2*m4d,m4ht-2*m4d)}
    {line from rvec_((m4wd-2*m4d)/5,m4ht/2-m4d) \
            to rvec_((m4wd-2*m4d)/5,-m4ht/2+m4d)}
    line from rvec_((m4wd-2*m4d)*4/5,m4ht/2-m4d) \
            to rvec_((m4wd-2*m4d)*4/5,-m4ht/2+m4d) }
    {lbox(m4wd,m4ht)}
    move to rvec_(m4wd,0); line to rvec_(max(0,rp_len/2-m4wd/2),0)}')
  sc_draw(`m4fusetype',A,
   `arc  cw to rvec_(m4ht,0) rad m4ht/2 with .c at rvec_(m4ht/2,0)dnl
    ifelse(m4a,d,`; {dot(at last arc.start,,1)}')
    arc ccw to rvec_(m4ht,0) rad m4ht/2 with .c at rvec_(m4ht/2,0)
    line to rvec_(max(0,rp_len/2-m4wd/2),0)dnl
    ifelse(m4a,d,`; dot(at last line.start,,1)') }')
  sc_draw(`m4fusetype',B,
   `{lbox(m4wd,m4ht)}
    line to rvec_(m4wd+max(0,rp_len/2-m4wd/2),0)}')
  sc_draw(`m4fusetype',C,
   `{lbox(m4wd,m4ht)}
    {line from rvec_(m4wd/5,-m4ht/2) to rvec_(m4wd/5,m4ht/2)}
    {line from rvec_(m4wd*4/5,-m4ht/2) to rvec_(m4wd*4/5,m4ht/2)}
    move to rvec_(m4wd,0); line to rvec_(max(0,rp_len/2-m4wd/2),0)}')
  sc_draw(`m4fusetype',S,
   `{lbox(m4wd,m4ht)}
    {ifsvg(`lbox(m4wd/5,m4ht,fill m4fill)',`m4cshade lbox(m4wd/5,m4ht))')}
    move to rvec_(m4wd,0); line to rvec_(max(0,rp_len/2-m4wd/2),0)}')
  {[box invis ht_ m4ht wid_ m4wd] at rvec_(rp_len/2,0)}
   line to rvec_(rp_len,0) invis ')

                                `memristor( linespec, wid, ht )'
define(`memristor',`eleminit_(`$1')
  define(`m4ht',ifelse(`$3',,`dimen_/5',`($3)'))define(`m4htx',`m4ht/4')dnl
  define(`m4wd',ifelse(`$2',,`m4ht*2',`($2)'))define(`m4wdx',`m4wd*4/25')dnl
  { line to rvec_(max(0,rp_len/2-m4wd/2),0)
    {[lbox(m4wd,m4ht)] at rvec_(m4wd/2,0)}
    line to rvec_(m4wdx,0) then to rvec_(m4wdx,m4htx) \
      then to rvec_(m4wdx*2,m4htx) then to rvec_(m4wdx*2,-m4htx) \
      then to rvec_(m4wdx*3,-m4htx) then to rvec_(m4wdx*3,m4htx) \
      then to rvec_(m4wdx*4,m4htx) then to rvec_(m4wdx*4,0) \
      then to rvec_(m4wdx*5,0)
    ifsvg(`lbox(m4wd/5,m4ht,fill m4fill)',`m4cshade lbox(m4wd/5,m4ht))')
    line to rvec_(max(0,rp_len/2-m4wd/2),0) }
  line invis to rvec_(rp_len,0)')

                                `cbreaker( linespec, L|R, D )
                                 circuit breaker to left or right of linespec,
                                 D=dotted'
define(`cbreaker',`eleminit_(`$1') define(`m4R',`ifelse(`$2',R,-)')
  define(`m4h',`dimen_/3') define(`m4cr',`((m4h+2*dimen_/32)*5/8)')dnl
  define(`m4ht',`(m4cr-sqrt(m4cr^2-(m4h/2+dimen_/32)^2)+dimen_/16)')dnl
  {line to rvec_(max(0,rp_len/2-m4h/2),0)
  {ifelse(`$3',D,`dot(,,1)
   move to rvec_(0,m4R`'dotrad_);')dnl
   arc ifelse(`$2',R,c)cw from rvec_(-dimen_/32,m4R`'dimen_/16) \
     to rvec_(m4h+dimen_/32,m4R`'dimen_/16) rad m4cr \
     with .c at rvec_(m4h/2,m4R`'(m4ht-m4cr))}
  {line from rvec_(m4h,0) to rvec_(m4h+max(0,rp_len/2-m4h/2),0)
   ifelse(`$3',D,`dot(at last line.start,,1)') }
  [box invis ht_ m4ht ifelse(`$3',D,`+2*dotrad_') wid_ m4h+dimen_/16] \
    at rvec_(m4h/2,m4R`'(m4ht/2)) }
  line to rvec_(rp_len,0) invis ')

                                `gap( linespec,fill,A )
                                 Gap with filled dots e.g.
                                 gap(down_ linewid/2,1); rlabel(+,v_1,-)
                                 A: chopped arrow between dots'
define(`gap',`eleminit_(`$1')
  dot(,,ifelse(`$2',,0,`$2')); dot(at last line.end,,ifelse(`$2',,0,`$2'))
  ifelse(`$3',A,
   `{arrow from last line.start to last line.end chop dotrad_*3}')
  {[box invis ht_ 0 wid_ min(rp_len,(dimen_*4/9+rp_len)/3)] at last line.c}
  ')

                                `arrowline( linespec ) line, centered arrowhead
                                 e.g. arrowline(up 1 dotted); llabel(,I_2)'
define(`arrowline',`line ifelse(`$1',,`to rvec_(elen_,0)',`$1')
  { arrow from last line.start to last line.end \
      chop lin_leng(last line)/2-arrowht/2
   [box invis ht_ arrowwid wid_ arrowht] at last line.c }')

                          `ground( at position, T, N|F|S|L|P|E, U|D|L|R|degrees)
                                 T=truncated stem; N=normal ground,
                                 F=frame, S=signal, L=low-noise, P=protective,
                                 E=European;
                                 up, down (default), left, right, angle (deg)'
define(`ground',`box invis ht 0 wid 0 with .c ifelse(`$1',,`at Here',`$1')
  define(`m4v',`dimen_/6')define(`m4h',`dimen_/16')dnl
  m4tmp_ang = rp_ang
  {direction_(ifelse(`$4',,-90,`$4'))
  ifelse(`$2',,`line from last box.c to rvec_(dimen_/4,0)')
  ifelse(`$3',F,
    `{line from rvec_(dimen_/8,m4v-dimen_/12) to rvec_(0,m4v) \
       then to rvec_(0,-m4v) then to rvec_(dimen_/8,-m4v-dimen_/12)}
     line to rvec_(dimen_/8,-dimen_/12)',
  `$3',S,
    `{line to rvec_(0,m4v) then to rvec_(m4v*1.5,0) then to rvec_(0,-m4v) \
      then to Here}',
  `$3',L,
    `{move to rvec_(m4h,0)
      arc cw rad m4v*3/2 from rvec_(Rect_(m4v*3/2,-60)) \
        to rvec_(Rect_(m4v*3/2,60)) with .c at Here}
     ground(,T,,`$4')',
  `$3',P,
    `{circle rad m4v*3/2 at rvec_(m4h,0)}
     ground(,T,,`$4')',
  `$3',E,
    `{line from rvec_(0,m4v*2/3) to rvec_(0,-m4v*2/3) thick linethick*2}',
  `{line from rvec_(0,m4v) to rvec_(0,-m4v)}
   {line from rvec_(m4h,dimen_/9) to rvec_(m4h,-dimen_/9)}
   line from rvec_(2*m4h,dimen_/14) to rvec_(2*m4h,-dimen_/14)')
  }; point_(m4tmp_ang)')

                        `antenna(at position, T, A|L|T|S|D|P|F, U|D|L|R|degrees)
                                 A=aerial; L=loop, T=triangle, S=diamond,
                                 D=dipole, P=phased, F=fork;
                                 up, down (default), left, right, angle (deg)'
define(`antenna',`[ T: Here
  define(`m4v',`dimen_/2')define(`m4h',`dimen_/12')dnl
  define(`m4atype',ifelse(`$3',,A,`$3'))dnl
  m4tmp_ang = rp_ang
  direction_(ifelse(`$4',,90,`$4'))
  ifelse(
  m4atype,L,
   `T1: rvec_(0,m4h); T2: rvec_(0,-m4h)
    ifelse(`$2',,`move to rvec_(m4h*2,0)')
    line from T1 to rvec_(0,m4h) then to rvec_(0,m4v/2) \
      then to rvec_(m4v-m4h,m4v/2) then to rvec_(m4v-m4h,-m4v/2+m4h) \
      then to rvec_(m4h,-m4v/2+m4h) then to rvec_(m4h,m4v/2-m4h) \
      then to rvec_(m4v,m4v/2-m4h) then to rvec_(m4v,-m4v/2) \
      then to rvec_(0,-m4v/2) then to rvec_(0,-m4h) then to T2',
  m4atype,T,
   `ifelse(`$2',,`move to rvec_(m4h*2,0)')
    line to rvec_(m4v*3/4,m4v*sqrt(3)/4) \
      then to rvec_(m4v*3/4,-m4v*sqrt(3)/4) then to Here
    line from rvec_(m4v*3/4,0) to T',
  m4atype,S,
   `T1: rvec_(0,m4h); T2: rvec_(0,-m4h)
    ifelse(`$2',,`move to rvec_(m4h*2,0)')
    line from T1 to rvec_(0,m4h) then to rvec_(m4v*3/4-m4h,m4v*3/4) \
      then to rvec_(2*m4v*3/4-m4h,0) then to rvec_(m4v*3/4-m4h,-m4v*3/4) \
      then to rvec_(0,-m4h) then to T2',
  m4atype,D,
   `T1: rvec_(0,m4h); T2: rvec_(0,-m4h)
    ifelse(`$2',,`move to rvec_(m4v,0)')
    { line from T1 to rvec_(0,m4h) then to rvec_(0,m4h*3) }
    { line from T2 to rvec_(0,-m4h) then to rvec_(0,-m4h*3) }',
  m4atype,P,
   `ifelse(`$2',,`move to rvec_(m4v*2/3,0)')
    line from T to Here
    { line from rvec_(0,-m4v/3) to rvec_(0,m4v/3) }
    { line from rvec_(m4h,-m4v*2/3) to rvec_(m4h,m4v*2/3) }',
  m4atype,F,
   `ifelse(`$2',,`move to rvec_(m4h*2,0)')
    { line from rvec_(m4v*3/4,m4v*sqrt(3)/4) to rvec_(0,m4v*sqrt(3)/4) \
        then to rvec_(0,-m4v*sqrt(3)/4) then to rvec_(m4v*3/4,-m4v*sqrt(3)/4)}
    line from rvec_(m4v*3/4,0) to T',
  m4atype,A,
  `ifelse(`$2',,`move to rvec_(m4h*2,0)')
   line from rvec_(m4v*3/4,m4v*sqrt(3)/4) to Here
   line from rvec_(m4v*3/4,-m4v*sqrt(3)/4) to Here
   line from rvec_(m4v*3/4,0) to T')
  point_(m4tmp_ang); `$5'] with .T ifelse(`$1',,`at Here',`$1')')

                                `switch( linespec,L|R,[O|C][D],B|D )
                                 R=right orientation (default L=left)
                                 if arg4=blank (knife switch): arg3 = [O|C][D]
                                   O= opening; C=closing; D=dots
                                 if arg4=B (button switch): arg3 = [O|C]
                                   O=normally open; C=normally closed
                                 if arg4=D: arg3 = same as for dswitch'
define(`switch',`ifelse(
 `$4',, `lswitch(`$1',`$2',`$3')',
 `$4',B,`bswitch(`$1',`$2',`$3')',
 `$4',D,`define(`m4qna_',`$3')dnl
  define(`m4rna_',W`'ifinstr(`$2',C,dBK,B)`'m4qna_)dnl
  dswitch(`$1',`$2',m4rna_)')')

                                `bswitch( linespec,L|R,chars ) pushbutton switch
                                 R=right orientation (default L=left)
                                 chars: O= normally open, C=normally closed'
define(`bswitch',`eleminit_(`$1') dnl
 define(`m4h',`dimen_/3') define(`m4cs',`0.069186*dimen_')dnl (2.5pt)
 define(`m4v',`ifelse(`$2',R,-m4cs,m4cs)')define(`dna_',`$3') dnl
 {line to rvec_(rp_len/2-m4h/2,0) chop 0 chop m4cs}
 { circle rad m4cs at rvec_(rp_len/2-m4h/2,0); move to last circle
   { circle rad m4cs at rvec_(m4h,0) }; { `$4' }
   sc_draw(`dna_',C,dnl
    `{ line from rvec_(-m4cs,-(m4v)) to rvec_(m4h+m4cs,-(m4v)) }
     { line from rvec_(m4h/2,-(m4v)) to rvec_(m4h/2,m4v*3) }
     {[box invis ht_ 4*m4cs wid_ m4h+2*m4cs] at rvec_(m4h/2,m4v)}',
    `{ line from rvec_(-m4cs,m4v*2.5) to rvec_(m4h+m4cs,m4v*2.5) }
     { line from rvec_(m4h/2,m4v*2.5) to rvec_(m4h/2,m4v*4.5) }
     {[box invis ht_ 5.5*m4cs wid_ m4h+2*m4cs] at rvec_(m4h/2,m4v*1.75)}')
   line from rvec_(m4h,0) to rvec_(m4h/2+rp_len/2,0) chop m4cs chop 0 }
 line to rvec_(rp_len,0) invis ')

                                `lswitch( linespec,L|R,chars ) knife switch
                                 R=right orientation (default L=left)
                                 chars=[O|C][D]
                                   O= opening; C=closing; D=dots'
define(`lswitch',`eleminit_(`$1') dnl
 define(`m4v',`dimen_/4')define(`m4cs',`dimen_/4*Sin(10)')dnl
 define(`dna_',`$3')m4_dna(`dna_',D)define(`m4d',m4I)dnl
 {line to rvec_(rp_len/2-dimen_/6,0)
   {line to rvec_(dimen_/4,ifelse(`$2',R,-)dimen_/4)
    ifelse(m4d,-1,,`dot(at last line.start,,1)') }
   m4t1 = arrowht; m4t2 = arrowwid;
   arrowht = dimen_/0.75*0.08; arrowwid = dimen_/0.75*0.053
   sc_draw(`dna_',C,`{ arc <- ifelse(`$2',R,,`c')cw \
       from rvec_(Rect_(dimen_/4,ifelse(`$2',R,,-)15))\
       to rvec_(Rect_(dimen_/4,ifelse(`$2',R,-)60)) \
       with .c at rvec_(rect_(-dimen_/4,ifelse(`$2',R,-)(60-15)/2*dtor_)) }')
   sc_draw(`dna_',O, `{ arc -> ifelse(`$2',R,,`c')cw \
       from rvec_(Rect_(dimen_/4,ifelse(`$2',R,,-)10))\
       to rvec_(Rect_(dimen_/4,ifelse(`$2',R,-)75)) \
       with .c at rvec_(rect_(-dimen_/4,ifelse(`$2',R,-)(75-10)/2*dtor_)) }')
   arrowht = m4t1 ; arrowwid = m4t2; { `$4' }
   [box invis ht_ dimen_/4+m4cs wid_ dimen_/4] \
     with .c at rvec_(dimen_/8,ifelse(`$2',R,-)(m4v-(m4cs))/2)}
 { line from rvec_(rp_len/2+dimen_/6,0) to rvec_(rp_len,0)
   ifelse(m4d,-1,,`dot(at last line.start,,1)') }
 line to rvec_(rp_len,0) invis ')

                                `dswitch(linespec,R,W[ud]B[K] chars)
                                 Comprehensive IEEE-IEC single-pole switch:
                                 arg2=R: orient to the right of drawing dir
                                 arg 3:
                                   blank means WB by default
                                   W=baseline
                                   B=contact blade
                                   dB=contact blade to the right of direction
                                   K=vertical closing contact line
                                     use WdBK for a normally-closed switch

                                   C = external operating mechanism
                                   D = circle at contact and hinge (dD = hinge
                                       only, uD = contact only)
                                   E = emergency button
                                   EL = early close (or late open)
                                   LE = late close (or early open)
                                   F = fused
                                   H = time delay closing
                                   uH = time delay opening
                                   HH = time delay opening and closing
                                   K = vertical closing contact
                                   L = limit
                                   M = maintained (latched)
                                   MM = momentary contact on make
                                   MR = momentary contact on release
                                   MMR = momentary contact on make and release
                                   O = hand operation button
                                   P = pushbutton
                                   R = time-delay operating arm
                                   T = thermal control linkage
                                   Y = pull switch
                                   Z = turn switch'
define(`dswitch',`eleminit_(`$1')
define(`dna_',ifelse(`$3',,WB,`$3'))dnl
define(`m4R',`ifelse(`$2',R,-)')define(`m4sc',`dimen_/24')dnl
 tr_xy_init(last line.c,m4sc,m4R)
 M4T: M4_xyO; M4S: M4T
 { line to tr_xy(-4,0)
   line from tr_xy(4,0) to 2nd last line.end
  sc_draw(`dna_',B,
   `define(`m4c',ifelse(m4a,d,-))dnl
    M4S: line from tr_xy(-4,0) to tr_xy(5,m4c`'9/sqrt(3))
    M4Q: 2 between M4_xyO and M4S.c')
  sc_draw(`dna_',MMR,
   `line from tr_xy(6,1.16) to tr_xy(4,0) then to tr_xy(6,-1.16)')
  sc_draw(`dna_',MM,
   `line from tr_xy(5,0) to tr_xy(4,0) then to tr_xy(6,1.16)')
  sc_draw(`dna_',MR,
   `line from tr_xy(5,0) to tr_xy(4,0) then to tr_xy(6,-1.16)')
  sc_draw(`dna_',D,
   `ifelse(m4a,u,,`dot(at tr_xy(-4,0),,1)')
    ifelse(m4a,d,,`dot(at tr_xy(4,0),,1)')')
  sc_draw(`dna_',EL,
   `line from 0.99 along_(M4S) to M4S.end \
      then to M4S.end + ta_xy(1.5,neg_(m4c)2.6)')
  sc_draw(`dna_',LE,
   `line from 0.99 along_(M4S) to M4S.end \
      then to M4S.end + ta_xy(-1.5,m4c`'2.6)')
  sc_draw(`dna_',K,
   `line from tr_xy(4,0) to tr_xy(4,m4c`'5.5)')
  sc_draw(`dna_',F,
   `M4dT: 1/4 along_(M4S); M4dQ: 3/4 along_(M4S)
    line from M4dT to M4dT + ta_xy(neg_(m4c)1/2,1) \
      then to M4dQ + ta_xy(neg_(m4c)1/2,1) then to M4dQ + ta_xy(m4c`'1/2,-1) \
      then to M4dT + ta_xy(m4c`'1/2,-1) then to M4dT ')
  sc_draw(`dna_',L,
   `M4dT: 11/16 along_(M4S) define(`m4e',ifelse(m4a,d,-))
    line from 5/16 along_(M4S) \
      to M4dT + ta_xy(neg_(m4e)1,ifelse(m4c,m4e,,-)2) then to M4dT')
  sc_draw(`dna_',T,
   `define(`m4t',ifelse(m4a,d,-))dnl
    M4T: M4S.c+ta_xy(0,m4t`'12)
    line from M4S.c to M4S.c+ta_xy(0,m4t`'4.5) then to M4S.c+ta_xy(3,m4t`'4.5) \
      then to M4S.c+ta_xy(3,m4t`'7.5) then to M4S.c+ta_xy(0,m4t`'7.5) \
      then to M4T ')
  sc_draw(`dna_',M,
   `define(`m4t',ifelse(m4a,d,-))dnl
    M4T: M4S.c+ta_xy(0,m4t`'12)
    line dashed 1.5*m4sc from M4S.c to M4S.c+ta_xy(0,m4t`'4.5)
    line to M4S.c+ta_xy(-3,m4t`'6) then to M4S.c+ta_xy(0,m4t`'7.5)
    line dashed 1.5*m4sc to M4T ')
  sc_draw(`dna_',C,
   `M4T: M4S.c+ta_xy(0,ifelse(m4a,d,-)12)
    line dashed from M4S.c to M4T ')
  sc_draw(`dna_',O,
   `line from M4T + ta_xy(-2.5,0) to M4T + ta_xy(2.5,0) ')
  sc_draw(`dna_',P,
   `line from M4T + ta_xy(-2.5,-2.5) to M4T + ta_xy(-2.5,0) \
      then to M4T + ta_xy(2.5,0) then to M4T + ta_xy(2.5,-2.5) ')
  sc_draw(`dna_',Y,
   `line from M4T + ta_xy(-2.5,2.5) to M4T + ta_xy(-2.5,0) \
      then to M4T + ta_xy(2.5,0) then to M4T + ta_xy(2.5,2.5) ')
  sc_draw(`dna_',Z,
   `line from M4T + ta_xy(-2.5,-2.5) to M4T + ta_xy(-2.5,0) \
      then to M4T + ta_xy(2.5,0) then to M4T + ta_xy(2.5,2.5) ')
  sc_draw(`dna_',R,
   `define(`m4t',ifelse(m4a,d,-))dnl
    M4dT: 5/12 along_(M4S); M4dQ: 7/12 along_(M4S)
    line from M4dT to M4dT + ta_xy(0,m4t`'12)
    line from M4dQ to M4dQ + ta_xy(0,m4t`'(neg_(m4c)sqrt(3)/2+12))
    M4T: 1/2 between Here and 2nd last line.end ')
  sc_draw(`dna_',HH,
   `arc ifelse(m4R,-,c)cw \
      from M4T+ta_xy(3,3/2) to M4T+ta_xy(-3,3/2) with .c at M4T+ta_xy(0,4.0)
    arc ifelse(m4R,,c)cw \
      from M4T+ta_xy(3,-3/2) to M4T+ta_xy(-3,-3/2) \
      with .c at M4T+ta_xy(0,-4.0) ')
  sc_draw(`dna_',H,
   `define(`m4t',ifelse(m4a,d,-))dnl
    arc ifelse(m4t,m4R,,c)cw \
      from M4T+ta_xy(3,m4t`'3/2) to M4T+ta_xy(-3,m4t`'3/2) \
      with .c at M4T + ta_xy(0,m4t`'4.0) ')
  sc_draw(`dna_',E,
   `line from M4T + ta_xy(-2.5,0) to M4T + ta_xy(2.5,0)
    arc ifelse(m4R,-,,c)cw to last line.start with .c at M4T + ta_xy(0,-1.5) ')
    M4dQ: M4Q - (M4_xyO.x,M4_xyO.y)
  M4dT: M4T - (M4_xyO.x,M4_xyO.y)
  if M4dQ.x*M4dT.x + M4dQ.y*M4dT.y > 0 then { M4Q: M4_xyO }
  [ box invis wid_ 8*m4sc ht_ distance(M4T,M4Q) ] \
      with .c at 0.5 between M4T and M4Q
 }
 line to rvec_(rp_len,0) invis ')

                                `Amplifier amp( linespec,size )'
define(`amp',`eleminit_(`$1') define(`m4wd',`ifelse(`$2',,`dimen_',`($2)')')dnl
 {line to rvec_(max(0,rp_len/2-m4wd/2),0)
   line from rvec_(m4wd,0) to rvec_(0,m4wd/2) then to rvec_(0,-m4wd/2) \
     then to rvec_(m4wd,0) then to rvec_(max(m4wd,rp_len/2+m4wd/2),0) }
 {[box invis ht_ m4wd wid_ m4wd] at rvec_(max(m4wd,rp_len)/2,0)}
 line to rvec_(max(rp_len,m4wd),0) invis ')

                                `integrator( linespec,size )'
define(`integrator',`eleminit_(`$1')
 define(`m4wd',`ifelse(`$2',,`dimen_',`($2)')')dnl
 {line from rvec_(m4wd/4,m4wd/2) to rvec_(0,m4wd/2) then to rvec_(0,-m4wd/2) \
    then to rvec_(m4wd/4,-m4wd/2) }
 {line from rvec_(m4wd*5/4,0) to rvec_(m4wd/4,m4wd/2) \
    then to rvec_(m4wd/4,-m4wd/2) then to rvec_(m4wd*5/4,0) \
    then to rvec_(max(rp_len,m4wd*5/4),0) }
 { [box invis ht_ m4wd wid_ m4wd*5/4] at rvec_(m4wd*5/8,0) }
 line to rvec_(max(rp_len,m4wd*5/4),0) invis ')

                                `opamp(linespec,
                                  - label, + label, size, chars)
                                   drawn as a []:
                                   defined positions:
                                     W, N, E, S, Out, E1, E1, In1, In2
                                   chars:
                                     P: power connections V1,V2
                                     R: labels at In1,In2 swapped
                                     T: truncated point '
define(`opamp',
`[define(`m4v',`ifelse(`$4',,`dimen_',`($4)')')define(`m4h',`m4v')dnl
define(`dna_',`$5')dnl
  eleminit_(`$1',max(elen_-m4h/4,m4h))
 W: Here
 N: vec_(0,m4v/2)
 S: vec_(0,-m4v/2)
 E: vec_(m4h,0)
 C: vec_(m4h/2,0)
 { sc_draw(`dna_',T,
    `line to N then to 0.75 between N and E then to 0.75 between S and E \
       then to S then to W; line from 0.75 between W and E to E',
    `line to N then to E then to S then to W; move to E')
   if rp_len > m4h then { line to rvec_(rp_len-m4h,0) }
 Out: Here }
 E1: vec_(m4h/2,m4v/4)
 E2: vec_(m4h/2,-m4v/4)
 In1: vec_(0,m4v/4)
 In2: vec_(0,-m4v/4)
   { move to In`'ifinstr(dna_,R,2,1)
     ifelse(`$2',,"ifsvg(-,`{\scriptsize$-$}')" \
       at rvec_(4pt__,0) ifsvg(+(0,textht/10)),m4lstring(`$2',"`$2'"))}
   { move to In`'ifinstr(dna_,R,1,2)
     ifelse(`$3',,"ifsvg(+,`{\scriptsize$+$}')" \
       at rvec_(4pt__,0) ifsvg(+(0,textht/10)),m4lstring(`$3',"`$3'"))}
 sc_draw(`dna_',P,
   `{line from E1 to (vec_(m4h/2,m4v/4+m4v/8));    V1: Here}
    {line from E2 to (vec_(m4h/2,-(m4v/4+m4v/8))); V2: Here}')
 `$6' ] ')

                                `dac(width,height,nIn,nN,nOut,nS)'
define(`dac',`[
  define(`dac_ht',`ifelse(`$2',,(dimen_),`$2')')dnl
  define(`dac_wd',`ifelse(`$1',,(dimen_+dac_ht/2),`$1')')dnl
In: Here
C:  rvec_((dac_wd-dac_ht/2)/2,0)
NE: rvec_(dac_wd-dac_ht/2, dac_ht/2)
Out:rvec_(dac_wd,0)
SE: rvec_(dac_wd-dac_ht/2,-dac_ht/2)
NW: rvec_(0,dac_ht/2)
SW: rvec_(0,-dac_ht/2)
  define(`m4dn',ifelse(`$3',,1,`eval($3)'))dnl
  for_(1,m4dn,1,`In`'m4x: NW-vec_(0,dac_ht*(2*m4x-1)/(2*m4dn))')dnl
  define(`m4dn',ifelse(`$4',,1,`eval($4)'))dnl
  for_(1,m4dn,1,`N`'m4x: NW+vec_((dac_wd-dac_ht/2)*m4x/(m4dn+1),0)')dnl
  define(`m4dn',ifelse(`$5',,1,`eval($5)'))dnl
  for_(1,m4dn,1,`r = eval(2*m4x-1)/eval(m4dn*2)
    Out`'m4x: Out-vec_(ifelse(eval((2*m4x-1)<m4dn),1,,-)dac_ht*(0.5-r),
      dac_ht*(0.5-r))')dnl
  define(`m4dn',ifelse(`$6',,1,`eval($6)'))dnl
  for_(1,m4dn,1,`S`'m4x: SW+vec_((dac_wd-dac_ht/2)*m4x/(m4dn+1),0)')dnl
  line to NW then to NE then to Out then to SE then to SW then to Here
  `$7']')

                                `adc(width,height,nIn,nN,nOut,nS)'
define(`adc',`[
  define(`adc_ht',`ifelse(`$2',,(dimen_),`$2')')dnl
  define(`adc_wd',`ifelse(`$1',,(dimen_+adc_ht/2),`$1')')dnl
In: Here
C:  rvec_(adc_ht/4+adc_wd/2,0)
NE: rvec_(adc_wd, adc_ht/2)
Out:rvec_(adc_wd,0)
SE: rvec_(adc_wd,-adc_ht/2)
NW: rvec_(adc_ht/2, adc_ht/2)
SW: rvec_(adc_ht/2,-adc_ht/2)
  define(`m4dn',ifelse(`$3',,1,`eval($3)'))dnl
  for_(1,m4dn,1,`r = eval(2*m4x-1)/eval(m4dn*2)
    In`'m4x: In+vec_(ifelse(eval((2*m4x-1)<m4dn),1,,-)adc_ht*(0.5-r),
      adc_ht*(0.5-r))')dnl
  define(`m4dn',ifelse(`$4',,1,`eval($4)'))dnl
  for_(1,m4dn,1,`N`'m4x: NW+vec_((adc_wd-adc_ht/2)*m4x/(m4dn+1),0)')dnl
  define(`m4dn',ifelse(`$5',,1,`eval($5)'))dnl
  for_(1,m4dn,1,`Out`'m4x: NE-vec_(0,adc_ht*(2*m4x-1)/(2*m4dn))')dnl
  define(`m4dn',ifelse(`$6',,1,`eval($6)'))dnl
  for_(1,m4dn,1,`S`'m4x: SW+vec_((adc_wd-adc_ht/2)*m4x/(m4dn+1),0)')dnl
  line from Out to SE then to SW then to In then to NW then to NE then to Out
  `$7']')

                               `diode(linespec,
                                      B|CR|D|K|L|LE[R]|P[R]|S|T|V|v|Z,[R][E])
                                 Arg 3: R=reversed polarity, E=enclosure'
define(`diode',`define(`m4dtype',`$3')sc_draw(`m4dtype',R,
 `reversed(`diode',`$1',`$2',m4dtype,shift(shift(shift($@))))',
 `eleminit_(`$1')dnl
  ifelse(`$2',, `m4gen_d',
         `$2',B,`m4gen_d(uLFZQuR)define(`m4dh',2*m4dh)',
         `$2',CR,`m4gen_d(LFcrCR)',
         `$2',D,`m4gen_d(LuFHdQR)define(`m4dv',2*m4dv)',
         `$2',K,`m4gen_d(LACR)',
         `$2',L,`m4gen_d(LAcACR)',
         `$2',LE,`m4gen_d(LuEFCR)',
         `$2',LER,`m4gen_d(LdEFCR)',
         `$2',P,`m4gen_d(LuPFCR)',
         `$2',PR,`m4gen_d(LdPFCR)',
         `$2',S,`m4gen_d(LFSR)',
         `$2',T,`m4gen_d(LFTR)',
         `$2',V,`m4gen_d(LFCXdR)',
         `$2',v,`m4gen_d(LFCvdR)',
         `$2',Z,`m4gen_d(LFZR)')
  sc_draw(`m4dtype',E,`define(`m4dh',`dimen_*0.7')define(`m4dv',`m4dh')dnl
    { circle diam m4dh at rvec_(rp_len/2,0) }')
  define(`m4dm',
   `ifelse(`$2',S,`m4dv/4',
           `$2',Z,`(m4dv/4-linethick pt__/2)',
           `$2',v,`m4dv/4',
                  0)')dnl
 { [ box invis ht_ m4dv+linethick pt__*sqrt(3) wid_ m4dh+linethick pt__ + m4dm
     ] at rvec_(rp_len/2+m4dm/2,0) }
 line invis to rvec_(rp_len,0)')')

                               `m4gen_d(chars):
                                [u|d]Ac Centre line of open arrowhead
                                [u|d]A open arrowhead shifted up, down, or 0
                                [u|d]B bar (gate) at arrowhead centre
                                [u|d]BB long bar (gate) at arrowhead centre
                                C vertical bar
                                [u|d]E em_arrows out
                                [u|d]F filled arrowhead shifted up, down, or 0
                                GG large SCR gate
                                G SCR gate
                                cr current regulator bars
                                H double-length vertical bars
                                [u]L  left stem, uL = shortened
                                [u|d]N thyristor gate at anode
                                [u|d]NN hockey-stick thyristor gate at anode
                                [u|d]P em_arrows
                                [u|d]Q left-pointing shifted filled arrowhead
                                [u]R  right stem, uR = shortened
                                S S-shape vertical bar
                                T T-diode vertical bar
                                [u|d]Vc centre line of left open arrowhead
                                [u|d]V left-pointing shifted open arrowhead
                                [u|d]W Thyristor gate
                                [u|d]WW hockey-stick thyristor gate
                                X varicap diode-capacitor
                                v varicap diode-capacitor curved plate
                                Z zener bar '
define(`m4gen_d',
`{define(`m4dv',`dimen_/6')define(`m4dh',sqrt(3)*m4dv/2)dnl
  define(`ddna_',`ifelse(`$1',,`LFCR',`$1')')dnl
  define(`m4dy',`(linethick pt__)*(sqrt(3)-1)/2')dnl
M4_s: last line.start; M4_e: last line.end
  sc_draw(`ddna_',L,dnl            left stem, uL = shortened
   `line from M4_s to 0.5 between M4_s and M4_e \
      chop 0 chop m4dh ifelse(m4a,,/2)')
  dnl                             Elements drawn from left of body
  sc_draw(`ddna_',E,dnl            EM radiation arrows pointing out
   `ifelse(m4a,d,
     `{em_arrows(,rp_ang*rtod_-135) with .Tail at rvec_(-m4dh*0.3,-m4dv*0.8)}',
     `{em_arrows(,rp_ang*rtod_+135) with .Tail at rvec_(-m4dh*0.3,m4dv*0.8)}')')
  sc_draw(`ddna_',P,dnl            EM radiation arrows
   `ifelse(m4a,d,
     `{em_arrows(,rp_ang*rtod_+45) with .Head at rvec_(-m4dh*0.3,-m4dv*0.8)}',
     `{em_arrows(,rp_ang*rtod_-45) with .Head at rvec_(-m4dh*0.3, m4dv*0.8)}')')
  sc_draw(`ddna_',GG,dnl           Large SCR gate
   `{line to 2 between Here and rvec_(m4dh,ifelse(m4a,d,-)m4dv/2) \
     then to rvec_(m4dh*2,ifelse(m4a,d,-)sqrt((4*dimen_/10)^2-(m4dh*3/2)^2))
    G: Here}')
  sc_draw(`ddna_',G,dnl            SCR gate
   `{line to 3/2 between Here and rvec_(m4dh,ifelse(m4a,d,-)m4dv/2)
     G: Here}')
  sc_draw(`ddna_',F,dnl            Filled arrowhead shifted up, down, or 0
   `define(`m4dn',`ifelse(m4a,u,m4dv/2,m4a,d,-m4dv/2,0)')
    ifelse(ifxfig(T)ifsvg(T),T,
     `line fill m4fill from rvec_(m4dh,m4dn) to rvec_(0,m4dn+m4dv/2)\
        then to rvec_(0,m4dn-m4dv/2) then to rvec_(m4dh,m4dn)',
     `m4cshade
        line from rvec_(m4dh,m4dn) to rvec_(0,m4dn+m4dv/2)\
        then to rvec_(0,m4dn-m4dv/2) then to rvec_(m4dh,m4dn)) ')
    ifelse(m4a,,,`; move to rvec_(0,neg_(m4dn))')')
  sc_draw(`ddna_',Ac,dnl           Centre line of open arrowhead
   `define(`m4dn',`ifelse(m4a,u,m4dv/2,m4a,d,-m4dv/2,0)')
    { line from rvec_(m4dh,m4dn) to rvec_(0,m4dn) }')
  sc_draw(`ddna_',A,dnl            Open arrowhead
   `define(`m4dn',`ifelse(m4a,u,m4dv/2,m4a,d,-m4dv/2,0)')
    line from rvec_(m4dh,m4dn) to rvec_(0,m4dn+m4dv/2) \
      then to rvec_(0,m4dn-m4dv/2) then to rvec_(m4dh,m4dn) dnl
    ifelse(m4a,,,`; move to rvec_(0,neg_(m4dn))')')
  sc_draw(`ddna_',BB,dnl           Long vertical bar at arrowhead centre
   `{ifelse(m4a,u,
     `line from rvec_(-m4dh/2,m4dv/4) to rvec_(-m4dh/2,dimen_*0.35)',
     m4a,d,
     `line from rvec_(-m4dh/2,-m4dv/4) to rvec_(-m4dh/2,-dimen_*0.35)')}
     G: last line.end')
  sc_draw(`ddna_',B,dnl            Vertical bar at arrowhead centre
   `{ifelse(m4a,u,
     `line from rvec_(-m4dh/2,m4dv/4) to rvec_(-m4dh/2,m4dv*5/4+m4dy)',
     m4a,d,
     `line from rvec_(-m4dh/2,-m4dv/4) to rvec_(-m4dh/2,-m4dv*5/4-m4dy)',
     `G: line from rvec_(-m4dh/2,-m4dv/2-m4dy) to rvec_(-m4dh/2,m4dv/2+m4dy)')
     ifelse(m4a,,,G: last line.end) }')
  sc_draw(`ddna_',C,dnl            Vertical bar
   `{line from rvec_(0,-m4dv/2-m4dy) to rvec_(0,m4dv/2+m4dy)}')
  m4gen_d2($@)') dnl              macro split to keep within m4 buffer size

  define(`m4gen_d2',`dnl
  dnl                             Elements drawn at right of body
  sc_draw(`ddna_',cr,dnl            Current regulator bars
   `{line from rvec_(-m4dv/4,-m4dv/2-m4dy) to rvec_(m4dv/4,-m4dv/2-m4dy)}
    {line from rvec_(-m4dv/4,m4dv/2+m4dy) to rvec_(m4dv/4,m4dv/2+m4dy)}')
  sc_draw(`ddna_',H,dnl            Double length double vertical bars
   `{line from rvec_(0,-m4dv-m4dy) to rvec_(0,m4dv+m4dy)}
    {line from rvec_(-m4dh,-m4dv-m4dy) to rvec_(-m4dh,m4dv+m4dy)}
    move to rvec_(-m4dh,0)')
  sc_draw(`ddna_',NN,dnl            Big thyristor gate at anode
   `{line from rvec_(-m4dh,ifelse(m4a,d,-)m4dv/4) \
       to rvec_(-m4dh*2,ifelse(m4a,d,-)(m4dv*3/4)) \
       then to rvec_(-m4dh*2,ifelse(m4a,d,-)sqrt((dimen_*0.7/2)^2-(m4dh*3/2)^2))
     G: Here}')
  sc_draw(`ddna_',N,dnl            Thyristor gate at anode
   `{line from rvec_(-m4dh,ifelse(m4a,d,-)m4dv/4) \
                     to rvec_(-m4dh*2,ifelse(m4a,d,-)(m4dv*3/4))
     G: Here}')
  sc_draw(`ddna_',Q,dnl            Left-pointing filled arrowhead
   `define(`m4dn',`ifelse(m4a,u,m4dv/2,m4a,d,-m4dv/2,0)')
    { ifelse(ifxfig(T)ifsvg(T),T,
       `line fill m4fill from rvec_(0,m4dn) to rvec_(m4dh,m4dn+m4dv/2) \
          then to rvec_(m4dh,m4dn-m4dv/2) then to rvec_(0,m4dn)',
       `m4cshade
          line from rvec_(0,m4dn) to rvec_(m4dh,m4dn+m4dv/2) \
          then to rvec_(m4dh,m4dn-m4dv/2) then to rvec_(0,m4dn))') }')
  sc_draw(`ddna_',S,dnl            S-shape vertical bar
   `{line from rvec_(-m4dv/4,-m4dv/3) to rvec_(-m4dv/4,-m4dv/2-m4dy) \
       then to rvec_(0,-m4dv/2-m4dy) then to rvec_(0,m4dv/2+m4dy) \
       then to rvec_(m4dv/4,m4dv/2+m4dy) then to rvec_(m4dv/4,m4dv/3)}')
  sc_draw(`ddna_',T,dnl            T-diode vertical bar
   `{line from rvec_(-m4dv/4,-m4dv/2-m4dy) to rvec_(0,-m4dv/2-m4dy)\
       then to rvec_(0,m4dv/2+m4dy) then to rvec_(-m4dv/4,m4dv/2+m4dy)}')
  sc_draw(`ddna_',WW,dnl            Big thyristor gate
   `{line from rvec_(0,ifelse(m4a,d,-)m4dv/4) \
       to rvec_(m4dh,ifelse(m4a,d,-)(m4dv*3/4)) \
       then to rvec_(m4dh,ifelse(m4a,d,-)sqrt((dimen_*0.7/2)^2-(m4dh*3/2)^2))
     G: Here}')
  sc_draw(`ddna_',Vc,dnl           Centre line of left open arrowhead
   `define(`m4dn',`ifelse(m4a,u,m4dv/2,m4a,d,-m4dv/2,0)')
    { line from rvec_(0,m4dn) to rvec_(m4dh,m4dn) }')
  sc_draw(`ddna_',V,dnl            Left-pointing open arrowhead
   `define(`m4dn',`ifelse(m4a,u,m4dv/2,m4a,d,-m4dv/2,0)')
    { line from rvec_(0,m4dn) to rvec_(m4dh,m4dn+m4dv/2) \
        then to rvec_(m4dh,m4dn-m4dv/2) then to rvec_(0,m4dn) }')
  sc_draw(`ddna_',v,dnl            Variable capacitor curved plate
   `{arc cw from rvec_(m4dv/3,-m4dv/2-m4dy) to rvec_(m4dv/3,m4dv/2+m4dy) \
       with .c at rvec_(m4dv/3+m4dv*sqrt(3)/2,0)}
    {line from rvec_(m4dv/3+m4dv*sqrt(3)/2-last arc.rad,0) to rvec_(m4dv/4,0)}')
  sc_draw(`ddna_',W,dnl            Big thyristor gate
   `{line from rvec_(0,ifelse(m4a,d,-)m4dv/4) \
                     to rvec_(m4dh,ifelse(m4a,d,-)(m4dv*3/4))
     G: Here}')
  sc_draw(`ddna_',X,dnl            Variable capacitor plate
   `{line from rvec_(m4dv/4,-m4dv/2-m4dy) to rvec_(m4dv/4,m4dv/2+m4dy)}')
  sc_draw(`ddna_',Z,dnl            Zener bar
   `{line from rvec_(-m4dv/4,-m4dv/2-m4dy) to rvec_(0,-m4dv/2-m4dy)\
       then to rvec_(0,m4dv/2+m4dy) then to rvec_(m4dv/4,m4dv/2+m4dy)}')
  m4gen_d3($@)') dnl              macro split to keep within m4 buffer size

  define(`m4gen_d3',`dnl
  sc_draw(`ddna_',R,dnl            right stem, uR = shortened
   `line from 0.5 between M4_s and M4_e to M4_e \
      chop m4dh ifelse(m4a,,/2,m4a,d,/2+m4dv/4) chop 0') }
  ')dnl
                                `em_arrows( type,degrees,length)
                                 type=[N|I|E][D]  N=nonionizing, I=ionizing,
                                 E=simple; D=dot on arrow stem
                                 degrees = absolute arrow direction'
define(`em_arrows',`[ define(`dnm_',`ifelse($1,,N,$1)')dnl
  arrowhead = em_arrowhead
  define(`m4_len',
   `ifelse(`$3',,`dimen_*ifinstr(`$1',E,0.25,0.46)',`($3)')')
  ang = ifelse(`$2',,135,`($2)')*dtor_
  sc_draw(`dnm_',N,
   `{ A1: arrow to rrot_(m4_len,0,ang) wid em_arrowwid ht em_arrowht }
    move to rrot_(0,-em_arrowwid*9/8,ang)
    { A2: arrow to rrot_(m4_len,0,ang) wid em_arrowwid ht em_arrowht } ')
  sc_draw(`dnm_',I,`m4_rad_arr(A1)
    move to rrot_(0,-em_arrowwid*9/8,ang); m4_rad_arr(A2)')
  sc_draw(`dnm_',E,
   `{ A1: line to rrot_(m4_len,0,ang) \
        then to rrot_(m4_len-dimen_/18,dimen_/18,ang) }
    move to rrot_(0,-dimen_/8,ang)
    { A2: line to rrot_(m4_len,0,ang) \
        then to rrot_(m4_len-dimen_/18,dimen_/18,ang) }')
  sc_draw(`dnm_',D,`dot(at A1.start); dot(at A2.start)')
  Tail: 0.5 between A1.start and A2.start
  Head: 0.5 between A1.end and A2.end
  `$4']')
define(`m4_rad_arr',`{{`$1': line invis to rrot_(m4_len,0,ang)}
  for_(1,3,1,
   `arc ifelse(m4x,2,c)cw to rrot_(dimen_/10,0,ang) \
      with .c at rrot_(dimen_/20,0,ang)')
    arrow to `$1'.end wid em_arrowwid ht em_arrowht*3/4 }')

                                `thyristor(linespec, chars)
                                 chars:
                                   D: Diode
                                   A: Open diode
                                   N: Anode gate
                                   B: Bidirectional diode
                                   C: Type IEC
                                   G: Full-size gate terminal
                                   H: Gate at arrowhead centre
                                   R: Right orientation
                                   E: envelope
                                   U: centre line
                                   V: arrowhead centre gate bar'
define(`thyristor',`ifinstr(`$2',C, dnl
 `bi_trans(`$1',ifinstr(`$2',R,,R),uEdCBUT`'xtract(`$2',E),,A:E;K:C;G:T;`$4')',
 `[ifinstr(`$2',B,
   `define(`m4thyd',`dimen_*0.8') eleminit_(`$1',m4thyd)
    T1: last line.start; T2: last line.end; A: T1
    ifinstr(`$2',R,`m4gen_d(LdG'xtract(`$2',G)`dFHuQR)',
                   `m4gen_d(LG'xtract(`$2',G)`uFHdQR)')',
   `define(`m4thyd',`dimen_*0.7') eleminit_(`$1',m4thyd)
    A: last line.start; K: last line.end
    m4gen_d(LCR`'ifinstr(`$2',A,ifinstr(`$2',UA,Ac)A,F) dnl
    ifinstr(`$2',V,B,
    `$2',H,`ifinstr(`$2',R,d,u)`'ifinstr(`$2',E,B)B',
    `$2',N,`ifinstr(`$2',R,d)`'ifinstr(`$2',E,N)N',
    `ifinstr(`$2',R,d)`'ifinstr(`$2',E,W)W'))')
    ifinstr(`$2',E,`Env:circle diam m4thyd with .c at rvec_(rp_len/2,0)')
    `$4']')')
                                `scr(linespec, chars, label)
                                 Place thyristor as a two-term element with
                                 arg 3 as label'
define(`scr',`eleminit_(`$1')
  ifelse(`$3',,,`$3:') thyristor(`$1',`$2',`$4') with .A at last line.start')

                                `tgate( linespec, [B][R|L] ) Transmission gate
                                 B= box form
                                 L= left orientation'
define(`tgate',`[ eleminit_(`$1') define(`m4tgm',ifinstr(`$2',L,-))
 A: last line.start
 B: last line.end
 C: last line.center
  ifinstr(`$2',B,
   `ebox(from A to B)
 Gb: C+vec_(0,m4tgm`'m4ht/2)
    L1: line from 2 between Gb and C to 4 between Gb and C',
   `m4gen_d(uLAVuR)
    Circle: circle thick max(4pt__,linethick/2) rad m4dh/4 \
      at C+vec_(0,m4tgm`'m4dh/4*4/3)
    L2: line from last circle+vec_(0,m4tgm`'m4dh/4) to C+vec_(0,m4tgm`'m4dh*3/2)
 Gb: Here
    L3: line from C to C-vec_(0,m4tgm`'m4dh) ')
 G: Here
  `$3']')
                                `ptrans( linespec, [R|L] ) Pass transistor
                                 L= left orientation'
define(`ptrans',`[ eleminit_(`$1') define(`m4ptm',ifinstr(`$2',L,-))
  define(`m4pv',`dimen_/6')define(`m4pwd',m4pv*2)dnl
 A: last line.start
 B: last line.end
 C: last line.center
    La1: line to rvec_(rp_len/2-m4pwd/2,0)
    { La2: line to rvec_(m4pwd,m4pv) then to rvec_(m4pwd,neg_(m4pv)) \
      then to Here }
    { La3: line from rvec_(m4pwd,0) to rvec_(0,m4pv) \
      then to rvec_(0,neg_(m4pv)) then to rvec_(m4pwd,0)
    Circle: circle thick max(4pt__,linethick/2) rad m4pv/4 \
      at C+vec_(0,m4ptm`'m4pv*5/6)
    La4: line from last circle+vec_(0,m4ptm`'m4pv/4) to C+vec_(0,m4ptm`'m4pv*2)
 Gb:Here
    La5: line from C-vec_(0,m4ptm`'m4pv/2) to C-vec_(0,m4ptm`'m4pv*3/2)
 G: Here }
    La6: line from rvec_(m4pwd,0) to B
  `$3']')
                                `tline( linespec, wid, len ) Transmission line'
define(`tline',`eleminit_(`$1')
   define(`m4v',`ifelse(`$2',,`dimen_/6',`($2)')')dnl
   define(`m4h',`ifelse(`$3',,`dimen_*2/3',min(rp_len-m4v/2,`$3'))')dnl
   {[box invis ht_ m4v wid_ m4h+m4v/2] at last line.c}
   {line from last line.c+vec_(m4h/2+m4v/4,0) to last line.end}
   {line to 2nd last line.c+vec_(-m4h/2,0)
    ifdpic(
    `line from rvec_(0,-m4v/2) to rvec_(m4h,-m4v/2)
     spline 0.5523 to rvec_(m4v/4,0) then to rvec_(m4v/4,m4v) to rvec_(0,m4v)
     line to rvec_(-m4h,0)
     spline 0.5523 to rvec_(-m4v/4,0) then to rvec_(-m4v/4,-m4v)\
       then to rvec_(m4v/4,-m4v) then to rvec_(m4v/4,0) then to Here',
    `line from rvec_(m4v/4,-m4v/2) to rvec_(m4h-m4v/4,-m4v/2)
     spline to rvec_(m4v/2,0) then to rvec_(m4v/2,m4v) then to rvec_(0,m4v)
     line to rvec_(-m4h+m4v/2,0)
     spline to rvec_(-m4v/2,0) then to rvec_(-m4v/2,-m4v) then to rvec_(0,-m4v)\
       then to Here then to rvec_(-m4v/2,0) then to rvec_(-m4v/2,-m4v)\
       then to rvec_(0,-m4v)') }
   line to 5th last line.end invis ')

define(`m4_U',`dimen_/10')      `Semiconductor grid size'
define(`m4_ht',`m4_U*10/6')     `Semiconductor arrowhead ht and wd'
define(`m4_wd',`m4_U*10/9')

                                `Bipolar transistor bi_tr(linespec, L|R, P, E)'
define(`bi_tr',`bi_trans(`$1',`$2',ifelse(`$3',P,u,d)EBCBU,`$4')')

                                `Darlington(linespec, L|R, P)'
define(`Darlington',
 `[Q1: bi_trans(`$1',`$2',ifelse(`$3',P,u,d)EBCBU)
   rpoint_(from Q1.E to Q1.C)
   E: Q1.E
   Q2: bi_trans(,`$2',ifelse(`$3',P,u,d)EBCBU) with .E at Q1.B
   B: Q2.B
   T: Q2.C+(Q2.Bulk.x-Q2.B.x,Q2.Bulk.y-Q2.B.y)
   C: intersect_(Q2.C,T,Q1.E,Q1.C)
   L1: line from Q2.C to C then to Q1.C
   `$4' ]')

                                `igbt(linespec, L|R, [L][[d]D])
                                 Arg 3: L = 2nd gate type, D = parallel diode,
                                 dD = dotted connections'
define(`igbt',`bi_trans(`$1',`$2',ifinstr(`$3',L,,GH)CBUdE`$3',`$4')')

                                `Customizable bi_trans(linespec, L|R, chars, E)
                                 chars BU=bulk line; B=base line and label
                                 uEn|dEn=emitters E0 to En; uE|dE=emitter line
                                 Cn|uCn|dCn=collectors C0 to Cn; (arrow u or d)
                                 C|uC|dC=collector line; (arrow u or d)
                                 uT|dT=trigger line;
                                 G=gate line and location;
                                 H=gate line; L=L-gate line and location;
                                 S=Schottky
                                 [d]D=named parallel diode, d=dotted connection'
define(`bi_trans',
 `define(`m4R',`ifelse(`$2',R,-)')define(`dna_',`ifelse(`$3',,BCuEBU,`$3')')dnl
  define(`m4n',0)define(`m4dE',)dnl
[ ifelse(`$1',,`tr_xy_init(,m4_U,m4R); E: tr_xy(-3,0); C: tr_xy(3,0)',
   `eleminit_(`$1'); tr_xy_init(last line.c,m4_U,m4R)
    E: last line.start; line from E to tr_xy(-3,0) \
         then to tr_xy(-3,0)+ vec_(0,m4R`'linethick pt__)
    C: E+vec_(rp_len,0); line from C to tr_xy(3,0) \
         then to tr_xy(3,0)+vec_(0,m4R`'linethick pt__)')
  Bulk: line sc_draw(`dna_',BU,,invis) from tr_xy(-2,4) to tr_xy(2,4)
  sc_draw(`dna_',B,
   `B: tr_xy(0,6.5); line from B to tr_xy(0,4)')
  for_(1,8,1,
   `sc_draw(`dna_',E`'m4x,`define(`m4n',m4x*1.5) define(`m4dE',m4a)
    m4bi_Em(m4x,m4a,E)
    line from tr_xy(-2,4) to tr_xy(-2-m4n,4)
    Bulk: line invis from tr_xy(-2-m4n,4) to Bulk.end')')
  sc_draw(`dna_',E,
   `define(`m4dE',m4a) m4bi_Em(0,m4a,E)')
  for_(1,8,1,
   `sc_draw(`dna_',C`'m4x,`define(`m4n',m4x*1.5)
    m4bi_Em(m4x,m4a,C)
    line from tr_xy(2,4) to tr_xy(2+m4n,4)
    Bulk: line invis from Bulk.start to tr_xy(2+m4n,4)')')
  sc_draw(`dna_',C,
   `m4bi_Em(0,m4a,C)')
  sc_draw(`dna_',S,
   `Bulk: line invis from Bulk.start+ta_xy(-1,0) to Bulk.end+ta_xy(1,0)
    S1: line from Bulk.end+ta_xy(-1,0) to Bulk.end \
      then to Bulk.end+ta_xy(0,-0.5) then to Bulk.end+ta_xy(-0.5,-0.5)
    S2: line from Bulk.start+ta_xy(1,0) to Bulk.start \
      then to Bulk.start+ta_xy(0,0.5) then to Bulk.start+ta_xy(0.5,0.5)')
  sc_draw(`dna_',T,
   `Tm: line from tr_xy(0,4) to tr_xy(2/3,(4 ifelse(m4a,u,+,-)4/1.8*2/3))
    T: Here')
  sc_draw(`dna_',G,
   `G: tr_xy(0,6.5); line from G to tr_xy(0,4.7)')
  sc_draw(`dna_',H,
   `H1: line from tr_xy(-2,4.7) to tr_xy(2,4.7)')
  sc_draw(`dna_',L,
   `G: tr_xy(-1.5,6.2); line from G to tr_xy(-1.5,4.7) then to tr_xy(1.5,4.7)')
  sc_draw(`dna_',D,
   `D1: line from tr_xy(-5,0) to tr_xy(-5,-4)
    D2: line from tr_xy( 5,0) to tr_xy( 5,-4)
    ifelse(m4a,d,`dot(at tr_xy( 5,0)); dot(at tr_xy(-5,0))')
    Diode: diode(ifelse(m4dE,d,from,to) tr_xy(-5,-4) dnl
                 ifelse(m4dE,d,to,from) tr_xy( 5,-4))
    ifelse(m4dE,d,,rp_ang = rp_ang + pi_)
    ifelse(`$1',,`E: tr_xy(-5,0); line from E to tr_xy(-3,0);
                  C: tr_xy(5,0);  line from C to tr_xy(3,0)')')
  ifelse(`$4',E,
   `A1: arc ifelse(`$2',R,c)cw from Bulk.end+ta_xy(-2,2.5) \
      to Bulk.end+ta_xy(-2,-5.5) with .c at Bulk.end+ta_xy(-2,-1.5)
    L1: line to Bulk.start+ta_xy(2,-5.5)
    A2: arc ifelse(`$2',R,c)cw to Bulk.start+ta_xy(2,2.5) \
      with .c at Bulk.start+ta_xy(2,-1.5)
    L2: line to Bulk.end+ta_xy(-2,2.5)')
  `$5'; manhattan ] ')
                                `emitters E0 ... En or collectors C0 ... Cn'
define(`m4bi_Em',
   ``$3'`$1': tr_xy(ifelse(`$3',E,-)(3+(`$1')*1.5),0)
    `$3'm`$1': line from `$3'`$1' to tr_xy(ifelse(`$3',E,-)(1.2+(`$1')*1.5),4)
    ifelse(`$2',,,`arrow ht m4_ht wid m4_wd ifelse(`$2',d,<-) \
      from 1/4 between `$3'm`$1'.start and `$3'm`$1'.end \
      to 3/4 between `$3'm`$1'.start and `$3'm`$1'.end')
    ifelse(eval(`$1'>0),1,`m4bi_Em(eval(`$1'-1),`$2',`$3')')')

                                `Unijunction transistor ujt(linespec, R,P,E)
                                   Bulk and terminals B1, B2, E defined
                                   arg 2: drawn to the right of curr direction
                                   arg 3: P-channel, default N
                                   arg 4: envelope'
define(`ujt',
`[ ifelse(`$1',,,`eleminit_($1)')
B1: Here
   ifelse(`$1',,,`line to rvec_(rp_len/2-m4_U*2,0)')
   Bl1: line to rvec_(0,ifelse(`$2',R,-)3.5*m4_U)
Bulk: line from rvec_(-m4_U*0.5,0) to rvec_(m4_U*4.5,0)
   Bl2: line from Bulk.end+vec_(-m4_U/2,0) \
      to Bulk.end+vec_(-m4_U/2,ifelse(`$2',R,,-)3.5*m4_U) \
      ifelse(`$1',,,`then to Bulk.c+vec_(rp_len/2,ifelse(`$2',R,,-)3.5*m4_U)')
B2: Here
E:  Bulk.c+vec_(2*m4_U,ifelse(`$2',R,-)3.5*m4_U)
    E1: line from E to Bulk.center
    {arrow from last line.ifelse(`$3',P,`end to 1',`start to 7')/8 \
      between last line.start and last line.end wid m4_wd ht m4_ht}
    ifelse(`$4',E,dnl
      `Env: circle rad 4*m4_U with .c at Bulk.c')
   `$5'; manhattan ] ')

                        `FETS:     j_fet(linespec, R, P, E )
                                   e_fet(linespec, R, P, E|S )
                                   d_fet(linespec, R, P, E|S )
                                   c_fet(linespec, R, P )
                                   with terminals S, D, G.
                                   arg 2: G pin drawn to right of curr direction
                                   arg 3: P-channel, default N
                                   arg 4: envelope'
define(`j_fet',`mosfet(`$1',`$2',ifelse(`$3',P,u,d)GSDF,`$4',`$5')')
                                  `Enhancement-mode FET e_fet(linespec,R,P,S,E)'
define(`e_fet',`mosfet(`$1',`$2',
  ifelse(`$4',S,`TFD'ifelse(`$3',P,u,d)S,`LEDSQ'ifelse(`$3',P,d,u)B),
    `$4',`$5')')
                                  `Depletion-mode FET d_fet(linespec,R,P,S,E)'
define(`d_fet',`mosfet(`$1',`$2',
  ifelse(`$4',S,`TFDR'ifelse(`$3',P,u,d)S,`LFDSQ'ifelse(`$3',P,d,u)B),
    `$4',`$5')')
                                `Simplified switching c_fet(linespec,R,P)
                                   arg 3: negated G pin'
define(`c_fet',`mosfet(`$1',`$2',`ZSDF'ifelse(`$3',P,d)T,,`$4')')

 ` The comprehensive mosfet(linespec,R,BDEFGLQRSTXZ,E)
   Every drawn component is controlled by a letter or letter pair in arg 3;
   adding or changing elements is easily done by adding a test for a letter
   or letter sequence:
                               udB: center bulk connection pin; u or d arrow
                                 D: D pin and lead
                                 E: dashed substrate
                                 F: solid-line substrate
                               udG: G pin to substrate at source; u or d arrow
                               udH: G pin to substrate at center; u or d arrow
                                 L: G pin to channel (kept for compatibility
                                    for now; the same as dM below)
                               udM: G pin to channel center or
                                    u: pin at drain end, d: pin at source end
                                Pz: parallel zener diode
                                 Q: connect B pin to S pin
                                 R: thick channel
                               udS: S pin and lead; u or d arrow
                                dT: G pin to center of channel d: not circle
                                 X: XMOSFET terminal
                                 Z: simplified complementary MOS
                                 arg 2: body drawn to right of curr direction
                                 arg 4: envelope'
define(`mosfet',
 `define(`m4R',`ifelse(`$2',R,-)')dnl               right orientation flag
  define(`dna_',`ifelse(`$3',,DSEdMuBQ,`$3')')dnl
  define(`m4s',ifinstr(dna_,Z,2.5,3.5))dnl          size parameter
[ ifelse(`$1',,
   `tr_xy_init(,m4_U,m4R); S: tr_xy(-2,0); D: tr_xy(2,0)',
   `eleminit_(`$1'); tr_xy_init(last line.c,m4_U,m4R)
    S: last line.start; line from S to tr_xy(-2,0) \
         then to tr_xy(-2,0)+vec_(0,m4R`'linethick pt__)
    D: S+vec_(rp_len,0); line from D to tr_xy(2,0) \
         then to tr_xy(2,0)+vec_(0,m4R`'linethick pt__)')
  sc_draw(`dna_',B,
   `B: tr_xy(0,0); Bl: line from B to tr_xy(0,m4s)
    ifelse(m4a,,,`arrow ht m4_ht wid m4_wd ifelse(m4a,d,<-) \
      from tr_xy(0,m4s/2)-vec_(0,m4R`'m4_ht/2) \
        to tr_xy(0,m4s/2)+vec_(0,m4R`'m4_ht/2) ')')
  sc_draw(`dna_',D,
   `Dl: line from tr_xy(2,0) to tr_xy(2,m4s)')
  sc_draw(`dna_',E,
   `Channel: line invis from tr_xy(-2.5,m4s) to tr_xy(2.5,m4s)
    line from tr_xy(-2.5,m4s) to tr_xy(-1,m4s)
    line from tr_xy(-0.5,m4s) to tr_xy(0.5,m4s)
    line from tr_xy(1,m4s) to tr_xy(2.5,m4s)')
  sc_draw(`dna_',F,
   `Channel: line from ifinstr(dna_,Z,
     `tr_xy(-2,m4s) to tr_xy(2,m4s)',
     `tr_xy(-2.5,m4s) to tr_xy(2.5,m4s)')')
  sc_draw(`dna_',G,
   `G: tr_xy(-2,(m4s+3.5))
    ifelse(m4a,,`Gl: line from tr_xy(-2,m4s) to G',
           m4a,d,`Gl: arrow from G to tr_xy(-2,m4s) ht m4_ht wid m4_wd',
           m4a,u,`Gl: line from tr_xy(-2,m4s) to G; arrow ht m4_ht wid m4_wd \
             from tr_xy(-2,(m4s+3-m4_ht/m4_U)) to tr_xy(-2,(m4s+3))')')
  sc_draw(`dna_',H,
   `G: tr_xy(0,(m4s+4))
    ifelse(m4a,,`Hl: line from tr_xy(0,m4s) to G',
           m4a,d,`Hl: arrow from G to tr_xy(0,m4s) ht m4_ht wid m4_wd',
           m4a,u,`Hl: line from tr_xy(0,m4s) to G; arrow ht m4_ht wid m4_wd \
             from tr_xy(0,(m4s+3-m4_ht/m4_U)) to tr_xy(0,(m4s+3))')')
  sc_draw(`dna_',L,
   `G: tr_xy(-2,(m4s+3.5))
    Ll: line from tr_xy(2,(m4s+1)) to tr_xy(-2,(m4s+1)) then to G')
  sc_draw(`dna_',M,`ifelse(
    m4a,, `G: tr_xy(0,(m4s+4))
           Glh: line from tr_xy(2,(m4s+1)) to tr_xy(-2,(m4s+1))
           Glv: line from tr_xy(0,(m4s+1)) to G',
    m4a,u,`G: tr_xy(2,(m4s+3.5))
           Gl: line from tr_xy(-2,(m4s+1)) to tr_xy(2,(m4s+1)) then to G',
    m4a,d,`G: tr_xy(-2,(m4s+3.5))
           Gl: line from tr_xy(2,(m4s+1)) to tr_xy(-2,(m4s+1)) then to G')')
  sc_draw(`dna_',Pz,
   `define(`m4q',m4a)dnl
    Diode: diode(ifelse(m4q,d,to,from) tr_xy(-2,-2) dnl
                 ifelse(m4q,d,from,to) tr_xy( 2,-2),Z)
    ifelse(m4q,d,rp_ang = rp_ang + pi_)
    line from tr_xy(-2,0) to tr_xy(-2,-2) \
      then to tr_xy(2,-2) then to tr_xy(2,0)')
  sc_draw(`dna_',Q,
   `Ql: line from tr_xy(0,0)+vec_(0,m4R`'linethick pt__) to tr_xy(0,0) \
      then to tr_xy(-2,0) then to tr_xy(-2,0)+vec_(0,m4R`'linethick pt__)')
  sc_draw(`dna_',R,
   `Rl: line thick 2*linethick from tr_xy(-2,m4s)\
         -vec_(0,m4R`'linethick*3/2 pt__) \
          to tr_xy(2,m4s)-vec_(0,m4R`'linethick*3/2 pt__) ')
  sc_draw(`dna_',S,
   `Sl: line from tr_xy(-2,0) to tr_xy(-2,m4s)
    ifelse(m4a,,,`arrow ht m4_ht wid m4_wd ifelse(m4a,d,<-) \
      from tr_xy(-2,m4s/2)-vec_(0,m4R`'m4_ht/2) \
        to tr_xy(-2,m4s/2)+vec_(0,m4R`'m4_ht/2) ')')
  sc_draw(`dna_',T,
   `Tl: line from tr_xy(-2,(m4s+1)) to tr_xy(2,(m4s+1))
    ifelse(m4a,d,`Not: circle rad m4_U*2/3 with .c at tr_xy(0,(m4s+1+2/3))')
    Gl: line from tr_xy(0,`(m4s+1'`ifelse(m4a,d,+4/3))') \
           to tr_xy(0,ifelse(`$4',E,(m4s+4),(m4s+4))); G: Here')
  sc_draw(`dna_',X,dnl          From Matteo Agostinelli
   `B: tr_xy(0,0); Xv: line from B to tr_xy(0,m4s-1)
       Xh: line from tr_xy(-1.5,m4s-1) to tr_xy(1.5,m4s-1)')
  ifelse(`$4',E,`Env: circle rad 4*m4_U with .c at tr_xy(0,m4s)')
  `$5'; manhattan ] ')
                               `Macro-internal coordinates adjusted for L|R'
define(`ta_xy',`vec_(vscal_(m4_xyU,`$1',ifelse(`$2',0,0,m4_xyS`($2)')))')
                               `Relative adjusted macro-internal coordinates'
define(`tr_xy',`M4_xyO+vec_(vscal_(m4_xyU,`$1',ifelse(`$2',0,0,m4_xyS`($2)')))')
                               `Initialize tr_xy_init(origin,unit,-)'
define(`tr_xy_init',`M4_xyO: ifelse(`$1',,Here,`$1')
define(`m4_xyU',`$2')dnl
define(`m4_xyS',`$3')')

                               `Extract substring plus preceding char if u or d'
define(`m4_dna',`define(`m4I',index($1,`$2'))dnl
ifelse(m4I,-1,`define(`m4t',0)',`define(`m4t',eval(m4I+len($2)))dnl
define(`m4a',ifelse(substr($1,decr(m4I),1),u,`define(`m4I',decr(m4I))'u,
                    substr($1,decr(m4I),1),d,`define(`m4I',decr(m4I))'d))dnl
define(`$1',substr($1,0,m4I)`'substr($1,m4t))')')dnl
                               `Conditional subcomponent draw
                                sc_draw(dna string, chars, iftrue, iffalse)'
define(`sc_draw',`m4_dna(`$1',`$2')ifelse(m4I,-1,`$4',`$3')')

                               `Element labels to the left, right, centre of
                                the current direction.  Labels are spaced and
                                treated as math, but copied literally if double
                                quoted or defined by sprintf'
define(`rlabel',`m4label(`$1',`$2',`$3',.s_,below_,`$4')')
define(`llabel',`m4label(`$1',`$2',`$3',.n_,above_,`$4')')
define(`clabel',`m4label(`$1',`$2',`$3',,,`$4')')
                   labels at centre and both ends of an element `dimen_' long
define(`m4label',`dnl
 ifelse(`$1',,,
  `{m4lstring(`$1',"ifsvg(`svg_it(`$1')',`sp_$ `$1'$sp_')") \
     at last [].w_ $5 rjust_ $6};')dnl
 ifelse(`$2',,,
  `{m4lstring(`$2',"ifsvg(`svg_it(`$2')',`sp_$ `$2'$sp_')") \
     at last []$4 $5 $6};')dnl
 ifelse(`$3',,,
  `{m4lstring(`$3',"ifsvg(`svg_it(`$3')',`sp_$ `$3'$sp_')") \
     at last [].e_ $5 ljust_ $6};')dnl
 ')
                               `Oblique element label
                                dlabel(long,lateral,label,label,label)'
define(`dlabel',`dnl
 ifelse(`$3',,,
  `{m4lstring(`$3',"ifsvg(`svg_it(`$3')',`$ `$3'$')") \
      at last [].c+vec_(-(`$1'),`$2')};')dnl
 ifelse(`$4',,,
  `{m4lstring(`$4',"ifsvg(`svg_it(`$4')',`$ `$4'$')") \
      at last [].c+vec_(0,`$2')};')dnl
 ifelse(`$5',,,
  `{m4lstring(`$5',"ifsvg(`svg_it(`$5')',`$ `$5'$')") \
      at last [].c+vec_(`$1',`$2')};')
 ')
                               `eleminit_( linespec, default length )
                                compute element direction and length.
				                Eleminit_ defines the position, length,
				                and angle of two-terminal elements.  It calls
                                rpoint_ with its linespec or circuit-element
                                default. The rpoint_ macro draws the invisible
                                line determined by its argument, calculates the
                                length and angle, and gives the angle to the
                                point_ macro to set the rotation parameters used
                                by rvec_.'
define(`eleminit_',
 `rpoint_(ifelse(`$1',,`to rvec_(ifelse(`$2',,`elen_',`$2'),0)',`$1'))')

                               `par_( element, element, separation )
                                Parallel combination of two branches that have
                                the same direction and length. The
                                branch arguments must be quoted, e.g.
                                par_(`resistor',`capacitor',dimen_)'
define(`par_',`[Start: Here; r = ifelse(`$3',,`dimen_',`$3')
   { move to Start + vec_(0,r/2);  $1 }
   line from Start + vec_(0,r/2) to Start + vec_(0,-r/2) ; $2
   End: line to rvec_(0,r); `$4'] with .Start at Here
   move to last [].End')

                               `gpar_( element, element, separation )
                                Parallel combination of two branches that have
                                the same direction, e.g.:
                                down_; gpar_(
                                  resistor;llabel(,R_1);resistor;llabel(,R_2),
                                  capacitor;rlabel(,C))
                                This macro trades simplicity for generality
                                and robustness to gpic'
define(`gpar_',
 `[ M4_B1: Here; `$1'; M4_E1: Here
    M4_C: 0.5 between M4_B1 and M4_E1; eleminit_(from M4_B1 to M4_E1)
    E2:[ M4_B: Here; `$2';  M4_E: Here; `$4'] \
      with .c at M4_C + (rect_(ifelse(`$3',,`dimen_',`$3'),rp_ang-pi_/2))
    M4_B2: E2.M4_B; M4_E2: E2.M4_E
    s = distance(M4_B2,M4_E2)
    if rp_len*s == 0 then { r = 1 } else { r = (1+max(rp_len/s, s/rp_len))/2 }
    if rp_len < s then { Tmp:M4_B2; M4_B2:M4_B1; M4_B1:Tmp
                         Tmp:M4_E2; M4_E2:M4_E1; M4_E1:Tmp }
    line from M4_B2 to r between M4_E2 and M4_B2
  Start: 0.5 between Here and M4_B1; line to M4_B1
    line from M4_E2 to r between M4_B2 and M4_E2
  End: 0.5 between Here and M4_E1
  C: 0.5 between Start and End; line to M4_E1 ] with .Start at Here
    move to last [].End ')

                                `reversed(`macro name in quotes', macro args)
                                 reverse polarity of two-terminal element'
define(`reversed',`eleminit_(`$2')
  $1(from last line.end to last line.start,shift(shift($@)))
  rp_ht = -rp_ht; rp_wid = -rp_wid; rp_ang = rp_ang - pi_
  line invis to last line.start ')

                                `resized(factor,`macro name in quotes',args)
                                 multiply element body size by factor'
define(`resized',`define(`m4resiztmp',dimen_)define(`dimen_',(dimen_)*(`$1'))dnl
  $2(shift(shift($@))) define(`dimen_',m4resiztmp)')

                                `variable(`element', type, angle, length)
                                 overlaid arrow or line on two-terminal element
                                 to show variablility: type = [A|P|L|[u]N][C|S]
                                 A=arrow, P=preset, L=linear, N=nonlinear,
                                 C=continuous, S=setpwise'
define(`variable',`$1
 {[ define(`dna_',`ifelse($2,,A,$2)') ang = ifelse(`$3',,45,`$3')
 M4_T: Here+(Rect_(ifelse(`$4',,`dimen_*0.8',`$4'),ang))
   sc_draw(`dna_',P,`Line: line to M4_T
     [line to (Rect_(dimen_/6,ang-90))] at Line.end')
   sc_draw(`dna_',L,`Line: line to M4_T')
   sc_draw(`dna_',N,`Line: line to M4_T
     ifelse(m4a,u,`line up dimen_/6 from Line.end',
                  `line left dimen_/6 from Line.start')')
   sc_draw(`dna_',A,`Line: arrow to M4_T')
   sc_draw(`dna_',C,`move to Line.end+(dimen_*0.10,-dimen_*0.06)
     line to Here+(Rect_(dimen_/6,ang))')
   sc_draw(`dna_',S,`move to Line.end+(dimen_*0.10,-dimen_*0.12)
     line up dimen_*0.06 then right dimen_*0.12 then up dimen_*0.06')
   `$5'] with .Line.c at last [].c } ')

                                `Line hopping over named lines,
                                 diverting left or right:
                                crossover(linespec,L|R,line_name,line_name,...)'
define(`hoprad_',`dimen_/12')
define(`crossover',`eleminit_(`$1')dnl
  M4_Tmp: last line.end
  m4_xover_(shift($@))dnl
  line to M4_Tmp ')
define(`m4_xover_',`ifelse(`$2',,,`line to \
  intersect_(last line.start,M4_Tmp,`$2'.start,`$2'.end) chop 0 chop hoprad_
  arc ifelse(`$1',R,c)cw to rvec_(2*hoprad_,0) with .c at rvec_(hoprad_,0)
  m4_xover_(`$1',shift(shift($@)))')')

                                `relay(number of poles,O|C,R)
                                 Number of poles (max 10),
                                 double-throw (default) or normally open or
                                 closed, drawn left (default) or right'
define(`relay',`[define(`m4n',`ifelse(`$3',R,-,0+)')
V1: Here
  L1: line to rvec_(dimen_/5,0)
  lbox(dimen_/5,dimen_/2)
  L2: line to rvec_(2*dimen_/5,0)
V2: Here
  move to V2+vec_(dimen_/12,m4n (dimen_/4+dimen_/5))
  m4_contacts(1,ifelse(`$1',,1,`$1'),`$2',`$3')
  ifelse(`$1',,,`ifelse(eval(`$1'>1),1,`DL: line dashed \
    from P1+vec_(dimen_*0.261,-(m4n dimen_/10)) \
    to P`$1'+vec_(dimen_*0.261,m4n dimen_/10)')')
  `$4'] ')
define(`m4_contacts',`contact(`$3',`$4') with .O at Here
  define(`m4v',`ifelse(`$1',,1,`$1')')dnl
  P`$1': last [].P
  ifelse(`$3',O,,C`$1': last [].C)
  ifelse(`$3',C,,O`$1': last [].O)
  ifelse(m4v,`$2',,`move to last[].C+(0,m4n dimen_/5)
    ifelse(eval(m4v<11),1,`m4_contacts(incr(m4v),`$2',`$3',`$4')')')')

                                `Relay contact switch contact(O|C,R)
                                 double-throw (default) or normally open or
                                 closed, to left or right'
define(`contact',`[define(`m4n',`ifelse(`$2',R,-,0+)')dnl
  P:dot; line to rvec_(dimen_/2,0)
  T: P + vec_(dimen_/2-dimen_/18,0)
  O: P+vec_(dimen_*(1/2-1/18+1/5),-(m4n dotrad_))
  C: P+vec_(dimen_*(1/2-1/18+1/5),m4n dotrad_)
  ifelse(`$1',O,,`arrow <- ht dimen_/6 wid dimen_/6 \
    from T to T+vec_(0,m4n dimen_/4) then to T+vec_(dimen_/5,m4n dimen_/4)
  C: Here')
  ifelse(`$1',C,,`arrow <- ht dimen_/6 wid dimen_/6 \
    from T+vec_(0,-(m4n dimen_/8)) \
    to T+vec_(0,-(m4n (dimen_/4+dimen_/8))) \
    then to T+vec_(dimen_/5,-(m4n (dimen_/4+dimen_/8)))
  O: Here'); `$3'] ')

                    `nport(box specs; other commands,
                       nw,nn,ne,ns,space ratio,pin lgth,style,other commands)
                     Default is a standard-box twoport.  Args 2 to 5 are
                     the number of ports to be drawn on w, n, e, s sides.
                     The port pins are named by side, number, and by a or b pin,
                     e.g. W1a, W1b, W2a, ... .  Arg 6 specifies the ratio of
                     port width to interport space, and arg 7 is the pin length.
                     Set arg 8 to N to omit the dots on the port pins
                     Arguments 1 and 9 allow customizations'
define(`nport',`[Box: box `$1'
  r = ifelse(`$6',,2.0,`$6')
  plg = ifelse(`$7',,`dimen_/4',`$7')
#                           `West side'
  define(`m4n',`ifelse(`$2'`$3'`$4'`$5',,1,`$2',,0,`($2)')')
  d = Box.ht/(m4n*(r+1)+1)
  move to Box.nw+(0,-d); down_
  m4portpins(-plg,d*r,d,W,`$8')
#                           `North side'
  ifelse(`$3',,,`define(`m4n',`($3)')
  d = Box.wid/(m4n*(r+1)+1)
  move to Box.nw+(d,0); right_
  m4portpins(plg,d*r,d,N,`$8')')
#                           `East side'
  define(`m4n',`ifelse(`$2'`$3'`$4'`$5',,1,`$4',,0,`($4)')')
  d = Box.ht/(m4n*(r+1)+1)
  move to Box.ne+(0,-d); down_
  m4portpins(plg,d*r,d,E,`$8')
#                           `South side'
  ifelse(`$5',,,`define(`m4n',`($5)')
  d = Box.wid/(m4n*(r+1)+1)
  move to Box.sw+(d,0); right_
  m4portpins(-plg,d*r,d,S,`$8')')
  `$9']')
define(`m4portpins',`for_(1,m4n,1,
 `{ if (`$1' != 0) then { line to rvec_(0,`$1') }
   `$4'`'m4x`'a: ifelse(xtract(`$5',N),N,Here,`dot') }
  move to rvec_(`$2',0)
  { if (`$1' != 0) then { line to rvec_(0,`$1') }
   `$4'`'m4x`'b: ifelse(xtract(`$5',N),N,Here,`dot') }
  ifelse(m4x,m4n,,`move to rvec_(`$3',0)')')')

                          `gyrator(box specs,space ratio,pin lgth,style)
                           Gyrator two-port wrapper for nport
                           e.g. gyrator(ht boxwid invis,,0,N)'
define(`gyrator',
 `define(`m4dna_',ifelse(xtract(`$4',V)`'xtract(`$4',H),,H`$4',`$4'))dnl
  sc_draw(`m4dna_',H,
   `nport(ifelse(`$1',,wid boxht,`$1'),1,,1,,`$2',`$3',`$4',
      for i=-1 to 1 by 2 do { line from (Box,W1a)+(i*Box.wid/2,0) \
        left i*Box.wid/4 then down W1a.y-W1b.y then right i*Box.wid/4
        arcd(Box+(i*Box.wid/4,0),(W1a.y-W1b.y)/3,i*90,i*270) };`$5')')dnl
  sc_draw(`m4dna_',V,
   `nport(ifelse(`$1',,wid boxht,`$1'),,1,,1,`$2',`$3',`$4',
      for i=-1 to 1 by 2 do { line from (N1a,Box)+(0,i*Box.ht/2) \
        down i*Box.ht/4 then right N1b.x-N1a.x then up i*Box.ht/4
        arcd(Box+(0,i*Box.ht/4),(N1b.x-N1a.x)/3,i*90+90,i*90+270) ;`$5'})') ')

                                `nullator(linespec, wid, ht)'
define(`nullator',`eleminit_(`$1')
   define(`m4wd',ifelse(`$2',,`dimen_/2',`($2)'))dnl
   define(`m4ht',ifelse(`$3',,`dimen_/4',`($3)'))dnl
   {line to rvec_(max(0,rp_len/2-m4wd/2),0)
    move to rvec_(m4wd/2,0)
    { spline ifdpic(0.58) from rvec_(ifdpic(0,-m4wd/20),m4ht/2) \
      to rvec_(m4wd/20*3,m4ht/2) then to rvec_(m4wd/2,m4ht/4) \
      then to rvec_(m4wd/2,-m4ht/4) then to rvec_(m4wd/20*3,-m4ht/2) \
      then to rvec_(-m4wd/20*3,-m4ht/2) then to rvec_(-m4wd/2,-m4ht/4) \
      then to rvec_(-m4wd/2,m4ht/4) then to rvec_(-m4wd/20*3,m4ht/2) \
      then to rvec_(ifdpic(0,m4wd/20),m4ht/2)
      }
    line from rvec_(m4wd/2,0) to rvec_(max(0,rp_len/2),0)}
  {[box invis ht_ m4ht wid_ m4wd] at rvec_(rp_len/2,0)}
   line to rvec_(rp_len,0) invis ')
                                `norator(linespec, wid, ht)'
define(`norator',`eleminit_(`$1')
   define(`m4wd',ifelse(`$2',,`dimen_/2',`($2)'))dnl
   define(`m4ht',ifelse(`$3',,`dimen_/4',`($3)'))dnl
   {line to rvec_(max(0,rp_len/2-m4wd/2),0)
    move to rvec_(m4wd/2,0)
    for i=-1 to 1 by 2 do { {
      spline to rvec_(i*m4wd/4,m4ht/2) then to rvec_(i*m4wd/2,m4ht/2) \
      then to rvec_(i*m4wd/2,-m4ht/2) then to rvec_(i*m4wd/4,-m4ht/2) \
      then to Here } }
    line from rvec_(m4wd/2,0) to rvec_(max(0,rp_len/2),0)}
  {[box invis ht_ m4ht wid_ m4wd] at rvec_(rp_len/2,0)}
   line to rvec_(rp_len,0) invis ')

                    `n-terminal box
                     nterm(box specs; other commands,nw,nn,ne,ns,pin lgth,style)
                     The default is three-terminal.  Args 2 to 5 are
                     the number of pins to be drawn on W, N, E, S sides.
                     The pins are named by side and number, e.g. W1, W2, N1, ...
                     Arg 6 is the pin length.  Set arg 8 to N to omit the dots
                     on the pins. Arguments 1 and 8 allow customizations, e.g.
                     nterm(,,,,,,N,
                           "$a$" at Box.w ljust
                           "$b$" at Box.e rjust
                           "$c$" at Box.s above) '
define(`nterm',`[Box: box ifelse(`$1',,wid dimen_ ht dimen_*2/3,`$1')
  plg = ifelse(`$6',,`dimen_/4',`$6')
#                           `West side'
  define(`m4n',`ifelse(`$2'`$3'`$4'`$5',,1,`$2',,0,`($2)')')
  d = Box.ht/(m4n+1)
  move to Box.nw+(0,-d); down_
  m4termpins(-plg,d,W,`$7')
#                           `North side'
  ifelse(`$3',,,`define(`m4n',`($3)')
  d = Box.wid/(m4n+1)
  move to Box.nw+(d,0); right_
  m4termpins(plg,d,N,`$7')')
#                           `East side'
  define(`m4n',`ifelse(`$2'`$3'`$4'`$5',,1,`$4',,0,`($4)')')
  d = Box.ht/(m4n+1)
  move to Box.ne+(0,-d); down_
  m4termpins(plg,d,E,`$7')
#                           `South side'
  define(`m4n',`ifelse(`$2'`$3'`$4'`$5',,1,`$5',,0,`($5)')')
  d = Box.wid/(m4n+1)
  move to Box.sw+(d,0); right_
  m4termpins(-plg,d,S,`$7')
  `$8']')
define(`m4termpins',`for_(1,m4n,1,
 `{ if (`$1' != 0) then { line to rvec_(0,`$1') }
   `$3'`'m4x: ifelse(xtract(`$4',N),N,Here,`dot') }
  ifelse(m4x,m4n,,`move to rvec_(`$2',0)')')')

                          `speaker(U|D|L|R|degrees, vert size, type)
                           type=H horn'
define(`speaker',`[direction_($1)
  define(`m4v',`ifelse(`$2',,`dimen_/3',`($2)/4')')dnl
  define(`m4h',`m4v*sqrt(2)')dnl
 ifelse(`$3',H,
  `{H1: line from rvec_(m4h,m4v/2) to rvec_(m4h*3/2,m4v*7/8) \
    then to rvec_(m4h*3/2,-m4v*7/8) then to rvec_(m4h,-m4v/2)}',
  `{H1: line from rvec_(m4h,m4v) to rvec_(m4h*2,m4v*2) \
    then to rvec_(m4h*2,-m4v*2) then to rvec_(m4h,-m4v)}')
 {lbox(m4h,m4v*2)}
 {Box: box invis wid_ m4h ht_ m4v*2 at rvec_(m4h/2,0)}
  In1: rvec_(0,m4v/2)
  In2: Here
  In3: rvec_(0,-m4v/2)
  In4: rvec_(m4h/4,m4v)
  In5: rvec_(m4h*3/4,m4v)
  In6: rvec_(m4h/4,-m4v)
  In7: rvec_(m4h*3/4,-m4v); `$4']')

                                `bell(U|D|L|R|degrees, vert size)'
define(`bell',`[direction_($1)
  define(`m4h',`ifelse(`$2',,`dimen_/2',`($2)')')dnl
 {lbox(m4h,m4h)}
 {Box: box invis wid_ m4h ht_ m4h at rvec_(m4h/2,0)}
 {Circle: circle diameter m4h at rvec_(m4h*3/2,0)}
  In1: rvec_(0,m4h/4)
  In2: Here
  In3: rvec_(0,-m4h/4); `$3']')
                                `microphone(U|D|L|R|degrees, vert size)'
define(`microphone',`[direction_($1)
  define(`m4h',`ifelse(`$2',,`dimen_/2',`($2)')')dnl
 {L1: line from rvec_(m4h,-m4h/2) to rvec_(m4h,m4h/2)}
 {Circle: circle diameter m4h at rvec_(m4h/2,0)}
  In1: rvec_(m4h*(2-sqrt(3))/4,m4h/4)
  In2: Here
  In3: rvec_(m4h*(2-sqrt(3))/4,-m4h/4); `$3']')
                                `buzzer(U|D|L|R|degrees, vert size,[C])'
define(`buzzer',`[direction_($1)
 ifelse(`$3',,
 `define(`m4h',`ifelse(`$2',,`dimen_/2',`($2)')')dnl
   {L1: line from rvec_(m4h,0) to rvec_(m4h,m4h/2) \
     then to rvec_(0,m4h/2) then to rvec_(0,-m4h/2) \
     then to rvec_(m4h,-m4h/2) then to rvec_(m4h,0)}
   {Box: box invis wid_ m4h ht_ m4h at rvec_(m4h/2,0)}
   {L2: line from rvec_(m4h,m4h/2) to rvec_(m4h,m4h/2)+vec_(Rect_(m4h,-75))}
   In1: rvec_(0,m4h/4)
   In2: Here
   In3: rvec_(0,-m4h/4)',
 `$3',C,`define(`m4h',`ifelse(`$2',,`(dimen_/3)',`(($2)/2)')')dnl
   {Face: line from rvec_(m4h,-m4h) to rvec_(m4h,m4h)}
   {arc ccw from Face.end to Face.start with .c at Face.c}
   In1: rvec_(m4h-sqrt(m4h^2-(m4h/3)^2),m4h/3)
   In2: Here
   In3: rvec_(m4h-sqrt(m4h^2-(m4h/3)^2),-m4h/3)'); `$4']')
                                `earphone(U|D|L|R|degrees, size, [C][R])
                                 earphone pair if arg3 contains C'
define(`earphone',`[direction_($1)
  define(`m4h',`ifelse(`$2',,`dimen_',`($2)')')dnl
  ifinstr(`$3',C,
   `L: circle diam m4h*0.4
    R: circle diam m4h*0.4 at L+vec_(m4h,0)
    C: 0.5 between L and R
    N: C+(vscal_(m4h/2,Vperp(L,R)))
    Lx: cintersect(L,L.rad,C,m4h/2,`$3')
    Rx: cintersect(C,m4h/2,R,R.rad,`$3')
    arc ifinstr(`$3',R,c)cw rad m4h/2 from Lx to Rx with .c at C',
   `{lbox(m4h/3,m4h/2)}
    {Box: box invis wid_ m4h/3 ht_ m4h/2 at rvec_(m4h/6,0)}
    {L1: line thick 2*linethick from rvec_(m4h/3+linethick pt__,-m4h/3) to \
                                  rvec_(m4h/3+linethick pt__, m4h/3) }
    In1: rvec_(0,m4h/8)
    In2: Here
    In3: rvec_(0,-m4h/8)'); `$4']')
                               `Signal-flow graph macros: labeled node,
                                directed labeled chopped straight line,
                                directed labeled chopped arc, and a self
                                loop.  All are contained in [] blocks.'

                               `Signal-flow graph initialization macro
                         sfg_init(line len, node rad, arrowhd len, arrowhd wid)'
define(`sfg_init',`cct_init
  sfg_wid = ifelse(`$1',,`(linewid/0.75*(2.5+0.25)/4)',(`$1'))# default line len
  sfg_rad = ifelse(`$2',,(0.25/4/2),(`$2'))  # node radius
  sfg_aht = ifelse(`$3',,(0.25/4),(`$3'))    # arrow height (arrowhead length)
  sfg_awid = ifelse(`$4',,`sfg_aht',(`$4'))  # arrowhead width
  ')
                               `sfgline(linespec,text,
                                         sfgabove|sfgbelow|ljust|rjust)
                                Draw a straight line with linespec, chopped by
                                node radius, with optional label'
define(`sfgline',`eleminit_(`$1',sfg_wid)
[ L1: line to rvec_(rp_len,0) chop sfg_rad
Start: last line.start
End: Here
  move to last line.c
  { arrow ht sfg_aht wid sfg_awid from rvec_(-sfg_aht/2,0) \
      to rvec_(sfg_aht/2,0) }
  ifelse(`$2',,,`"ifsvg(`svg_it($2)',`$ `$2'$')" ifelse(`$3',,`sfgabove',`$3')')
  ] with .Start at rvec_(sfg_rad,0)
  move to last [].End
  ')
                               `Like above_ or below_ but adding extra space
                                to put text above or below arrowheads or nodes'
define(`sfgabove',`at Here+(0,sfg_awid/2) above')
define(`sfgbelow',`at Here+(0,-sfg_awid/2) below')

                               `sfgnode(at pos,text,above_,circle options)
                                Node: a white circle, possibly labelled. The
                                default label position is inside if the
                                diameter is bigger than textht and textwid'
define(`sfgnode',
 `[circle fill_(1) rad sfg_rad ifelse(`$4',,`thickness 0.5',`$4')] with .c \
    ifelse(`$1',,`at rvec_(sfg_rad,0)',`$1')
  move to last [].c
  ifelse(`$2',,,
   `if 2*sfg_rad > Max(textwid,textht,10pt__) \
       then { {"ifsvg(`svg_it($2)',`$ `$2'$')" `$3'} } \
    else { {"ifsvg(`svg_it($2)',`$ `$2'$')" ifelse(`$3',,`sfgabove',`$3')} }')
  ')
                               `sfgarc(linespec,label,above_,cw|ccw,sfact)
                                An arc between nodes at the endpoints of the
                                linespec.  The resulting positions Start, End,
                                C (arc center) and M (arc midpoint) are defined.
                                The fifth argument scales the arc height above
                                its chord.'
define(`sfgarc',`eleminit_(`$1',sfg_wid)
[ Start: Here
  End: Start + vec_(rp_len,0)
    chordht = (rp_len/sqrt(2)-rp_len/2)ifelse(`$5',,,`*($5)')
    arcrd = (chordht^2+(rp_len)^2/4)/chordht/2
  C: 0.5 between Start and End; C: C+vec_(0,ifelse(`$4',ccw,,-)(arcrd-chordht))
  M: C + vec_( 0,ifelse(`$4',ccw,-)arcrd)
    arc -> ifelse(`$4',ccw,ccw,cw) \
      from cintersect(Start,sfg_rad,C,arcrd,ifelse(`$4',ccw,R)) \
      to cintersect(C,arcrd,M,sfg_aht/2,ifelse(`$4',ccw,,R)) \
      ht sfg_aht wid sfg_awid with .c at C
    ifelse(`$2',,,`{move to M; "ifsvg(`svg_it($2)',`$ `$2'$')" ifelse(`$3',,
     `sfgabove',`$3')}')
    arc ifelse(`$4',ccw,ccw,cw) from M \
      to cintersect(C,arcrd,End,sfg_rad,ifelse(`$4',ccw,R)) with .c at C
  ] with .Start at last line.start
  move to last line.end
  ')
                `sfgself(at position,U|D|L|R|degrees,label,above_,cw|ccw,sfact)
                                A teardrop-shaped self-loop drawn at "angle"
                                degrees from "positon". The resulting Origin
                                and M (arc midpoint) are defined.  The sixth
                                argument scales the loop.'
define(`sfgself',`[ Origin: Here
  ang = rp_ang define(`m4td',m4dir)
  direction_(`$2',up_)
  d = ifelse(`$6',,,`($6)*') sfg_wid/2; d = d * max(1,sfg_rad/(0.3605*d))
  { m4sfgselfcurve(-)
    M: Here
    { arrow from rvec_(0,ifelse(`$5',cw,,-)sfg_aht/2) \
        to rvec_(0,ifelse(`$5',cw,-)sfg_aht/2) ht sfg_aht wid sfg_awid }
    ifelse(`$3',,,`"ifsvg(`svg_it($3)',`$ `$3'$')" ifelse(`$4',,`sfgabove',`$4')') }
  m4sfgselfcurve
  m4xpand(m4td`'_); point_(ang)
  ] with .Origin ifelse(`$1',,at Here,`$1')
  move to last [].Origin
  ')
define(`m4sfgselfcurve',`spline from rvec_(Rect_(sfg_rad,`$1'30)) \
  to rvec_(0.3*d,`$1'0.2*d) then to rvec_(0.6*d,`$1'0.35*d) \
  then to rvec_(0.9*d,`$1'0.35*d) \
  then to rvec_(d,`$1'0.2*d) then to rvec_(d,0)')

                     `winding(L|R,diam, pitch, nturns, core wid, core color )
                      The complete spline is drawn, then parts of it are
                      overwritten with the background color (default
                      white).  Arg 1 contains R for right-handed winding.
                      Arg 4 must be an integer.
                      Arg 6 must be compatible with the postprocessor.
                      Requires a recent version of dpic or gpic'
define(`winding',`[ define(`m4rt',`ifinstr(`$1',R,-)')
  d = ifelse(`$2',,`dimen_',`$2')
  p = ifelse(`$3',,d/4,`$3')
  define(`m4n0',`ifelse(`$4',,1,`eval(`$4'-1)')')dnl
  w = ifelse(`$5',,d*3/4,`$5')
  W: spline from 0,0 to 0,0 dnl
  for_(0,m4n0,1,
   `then to vec_(m4rt`'(m4x*p),d) \
    then to vec_(m4rt`'(m4x*p+p/2),d) \
    then to vec_(m4rt`'(m4x*p+p/2),0) \
    ifelse(m4x,m4n0,,`then to vec_(m4rt`'(m4x*p+p),0)')dnl')
  if w > 0 then {
    dx = sign(p)*(linethick+0.5)/2 bp__
    vx = p*min(w/d/8+dx/p,0.25)
    for i=0 to m4n0 do {
      ifmpost(ifdef(`r_',`command "def X=;enddef;"'))
      line color "ifelse(`$6',,ifpostscript(1 1 1,white),`$6')" \
        thick max(w/(1bp__)-linethick,0) \
        from vec_(m4rt`'(i*p-dx),d/2) to vec_(m4rt`'(i*p+vx),d/2)
      ifmpost(ifdef(`r_',`command "def X=lcolr;enddef;"'))
      dx = vx }
    }
  T1: W.ifinstr(`$1',R,end,start)
  T2: W.ifinstr(`$1',R,start,end)
  `$7']')

define(`m4cshade',`ifdef(`r_',
 `rgbfill(r_,g_,b_,',`shade(ifelse(`$1',,m4fill,`$1'),')')

`==============================================================================
                                Customizations:
                                The size and style parameters below can be
                                tweaked, and the cct_init macro modified.'

                                Size and style parameters:
define(`dimen_',`linewid')           Default element body size unit

define(`sourcerad_',`(0.25*dimen_)') Source element default radius
define(`csdim_',`(0.3*dimen_)')      Controlled Source width/2
define(`elen_',`(1.5*dimen_)')       Default element length
define(`delay_rad_',`(0.35*dimen_)') Delay elements
define(`dotrad_',`(0.04*dimen_)')    Redefine dot size for circuits
define(`m4fill',`0')                 Default fill for diode, fuse, ...
define(`em_arrowwid',`(dimen_/9)')  `em_arrows arrowhead width'
define(`em_arrowht',`(dimen_/7)')   `em_arrows arrowhead ht'
define(`em_arrowhead',1)            `em_arrows arrowhead style'

right_
                                Initialize global variables:
define(`cct_init',`dnl Do not change the format of the next line:
`#' `$0' Version 6.94: ifelse(m4picprocessor,gpic,`Gpic',
  m4postprocessor,pstricks,`PSTricks',
  m4postprocessor,pgf,`TikZ PGF',
  m4postprocessor,mfpic,`Mfpic',
  m4postprocessor,xfig,`Xfig',
  m4postprocessor,svg,`SVG',
  m4postprocessor,mpost,`MetaPost',`Default') m4 macro settings used.
gen_init dnl
psset_(arrowsize=1.1pt 4`,'arrowlength=1.64`,'arrowinset=0)
dnl                             Customizations can be put here
`# cct_init end'dnl
')

# These definitions are tentative and subject to change.
# ---------------------------------------------------------------------------#
# If either nosvgformat or Inkscape is defined then the svg formatting
# macros are turned off.  Text formatting can then be done in Inkscape.

dnl Put define(`Inkscape') in the source for Inkscape input
ifelse(ifdef(`Inkscape',T)`'ifdef(`nosvgformat',T),T,
`define(`svg_it',`$1')
define(`svg_norm',`$1')
define(`svg_bf',`$1')
define(`svg_small',`$1')
define(`svg_fsize',`$1')
define(`svg_sub',`$1')
define(`svg_sup',`$1')
',
`define(`m4IEdx',4)
define(`svg_txt',
`<tspan <!--[if gte IE 9]>dx=\"m4IEdx\"<![endif]-->>`$1'</tspan>')
define(`svg_ix',`<tspan font-style=\"italic>`$1'</tspan>')
define(`svg_it',
`<tspan font-style=\"italic\" dx=\"m4IEdx\">`$1'</tspan> ')
define(`svg_norm',`<tspan font-style=\"normal\">`$1'</tspan>')
define(`svg_bf',`<tspan font-style=\"bold\">`$1'</tspan>')
define(`svg_small',`<tspan font-size=\"ifelse(`$2',,66,`$2')%\">`$1'</tspan>')
define(`svg_fsize',`<tspan font-size=\"ifelse(`$2',,100,`$2')%\">`$1'</tspan>')
define(`svg_sub',`svg_small(<tspan baseline-shift=\"3\">`$1'</tspan>,`$2')')
define(`svg_sup',`svg_small(<tspan baseline-shift=\"-3\">`$1'</tspan>,`$2')')
')

define(`svgcolor',`sprintf("rgb(%g,%g,%g)",\
int(`$1'+0.5),int(`$2'+0.5),int(`$3'+0.5))')

define(`svgcolor255',`svgcolor((`$1')*255,(`$2')*255,(`$3')*255)')

define(`svgLink',`command "<a xlink:href=\"`$1'\"dnl
  ifelse(`$3',,`target=\"_blank\"',`$3',B,,`$3')>
  <g stroke=\"rgb(0,0,255)\">"
  `$2'
  command "</g></a>"')
define(`svgLinkString',`"<a xlink:href=\"`$1'\"dnl
  ifelse(`$3',,`target=\"_blank\"',`$3',B,,`$3') dnl
  stroke=\"rgb(0,0,255)\">dnl
 `$2' dnl
  </a>"')
# ---------------------------------------------------------------------------#
divert(0)dnl
.PS                            # Pic input begins with .PS
cct_init                       # Set defaults

elen = 0.75                    # Variables are allowed; default units are inches
move right 0.4
Origin: Here                   # Position names are capitalized
   svgLink(SourcesSVG.svg,source(up_ elen))
   llabel(-,v`'svg_sub(s),+)
   svgLink(CctTableSVG.svg,resistor(right_ elen));  rlabel(,R,)
   dot
   {                           # Save current position and direction
      capacitor(down_ to (Here,Origin))     #(Here,Origin) = (Here.x,Origin.y)
      rlabel(+,v,-); llabel(,,C)
      dot
      }                        # Restore position and direction
   line right_ elen*2/3
   {move right 0.2}
   inductor(down_ Here.y-Origin.y); rlabel(,L,); b_current("i")
   line to Origin
.PE                            # Pic input ends
