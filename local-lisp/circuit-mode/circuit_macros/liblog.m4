divert(-1)
   liblog.m4                    Elementary logic gates

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

`Notes: ==================================================================
  Gates other than BUFFER and NOT have an optional integer argument N
  that sets the number of input locations, which then have labels In1
  to InN.

  BUFFER and NOT gates have In1 only.  If there is a first argument, it
  is a line specification and the gate is drawn along the line as for a
  two-terminal element.

  NEGATED inputs are obtained if the second argument is N (uppercase n).
 ========================================================================='

define(`L_unit',`(linewid/10)')  `Grid mesh size'
                               `Dimensions in L_units, also for external use:'
define(`G_hht',3)              `gate half-height'
define(`AND_ht',`(2*G_hht)')     `gate heights and widths ...'
define(`AND_wd',7)
define(`BUF_ht',4)
define(`BUF_wd',3.5)
define(`OR_rad',7)             `OR input radius'
define(`XOR_off',1)            `XOR and NXOR parameter'
define(`N_diam',(3/2))           `not-circle diameter'
define(`N_rad',`(N_diam/2)')       `not-circle radius'
define(`NOT_rad',`(N_rad*L_unit)') `scaled radius eg line chop NOT_rad'
define(`H_ht',2)               `Hysteresis symbol dimen'
define(`Mx_pins',6)            `max number of gate input pins without extensions
                                Possibly 4 is better for negated inputs'
define(`FF_wid',12)            `Bistable'
define(`FF_ht',18)
define(`Mux_wid',8)            `Multiplexer'
define(`Mux_ht',18)
define(`lg_plen',4)            `Logic pin'
define(`lg_pintxt',
 `"ifxfig(`$1',`ifsvg(`svg_small(`$1',75)',`sp_{\scriptsize `$1'}sp_')')"')
define(`lg_bartxt',`ifxfig(`$1',`ifsvg(`$1',`$\overline{\hbox{`$1'}}$')')')

                               `Scale grid coordinates to world coordinates'
define(`grid_',`(vscal_(L_unit,`$1',`$2'))')
                               `Scale and rotate grid coords to world coords'
define(`svec_',`vec_(vscal_(L_unit,`$1',`$2'))')
                               `Relative svec_'
define(`rsvec_',`Here+svec_(`$1',`$2')')

                                `NOT_circle
                                 convenience for drawing NOT circles'
define(`NOT_circle',`circle diam N_diam*L_unit')

                                `LH_symbol(U|D|L|R|degrees)
                                 logical hysteresis symbol'
define(`LH_symbol',`[ m4tmp_ang = rp_ang; direction_(`$1')
  line from svec_(-H_ht*0.7,-H_ht/2) to svec_(H_ht*0.35,-H_ht/2) \
    then to svec_(H_ht*0.35,H_ht/2)
  line from svec_(H_ht*0.7,H_ht/2) to svec_(-H_ht*0.35,H_ht/2) \
    then to svec_(-H_ht*0.35,-H_ht/2)
  point_(m4tmp_ang); `$2'] ')

                                `LT_symbol(U|D|L|R|degrees)
                                 triangle_symbol'
define(`LT_symbol', `[ m4tmp_ang = rp_ang; direction_(`$1')
  line to svec_(0,H_ht*5/8) then to svec_(H_ht,0) \
    then to svec_(0,-H_ht*5/8) then to Here
  point_(m4tmp_ang); `$2'] ')

                                `BOX_gate(inputs,output,swid,sht,label)
                                 drawn in the current direction
                                 inputs=[P|N]* output=P|N'
define(`BOX_gate',`dnl
define(`m4m',`ifelse(`$1',,2,len(`$1'))')define(`m4a',`$1')dnl
define(`m4h',`ifelse(`$3',,AND_wd,`$3')')dnl
define(`m4v',`ifelse(`$4',,AND_wd,`$4')')dnl
[ {[line to svec_(0,m4v/2) then to svec_(m4h,m4v/2) then to svec_(m4h,-m4v/2) \
    then to svec_(0,-m4v/2) then to (0,0)]}
  ifelse(`$5',,,`{ move to last [].n+(0,-5pt__)
    m4lstring($5,"ifsvg(`svg_small($5,75)',`{\scriptsize$ $5 $}')") }')
  IOdefs(from svec_(0,m4v/2) to svec_(0,-m4v/2),In,`$1',R)
  Out: svec_(m4h,0) ifelse(`$2',N,`+svec_(N_diam,0)
    N_Out: NOT_circle at Out-svec_(N_rad,0)'); `$6']')

                                `AND_gate(n,[N][B],wid,ht)
                                 drawn in the current direction
                                 0 <= n <= 16; N=negated inputs, B=box shape'
define(`AND_gate',`ifinstr(`$2',B,
`m4_m4t(`$1',`$2') BOX_gate(m4t,P,`$3',`$4',ifsvg(`&```amp''';',`\&'),`$5')',
`AND_gen(ifelse(`$1',,2,`$1'),ifelse(`$2',N,N)IBAONESEC,`$3',`$4',`$5')')')

define(`m4_m4t',`define(`m4_t',`ifinstr(`$2',N,N,P)')define(`m4t',)dnl
for_(1,ifelse(`$1',,2,`$1'),1,`define(`m4t',m4t`'m4_t)dnl')')

                                `NAND_gate(n,[N][B],wid,ht)
                                 0 <= n <= 16; N=negated inputs, B=box shape'
define(`NAND_gate',`ifinstr(`$2',B,
`m4_m4t(`$1',`$2') BOX_gate(m4t,N,`$3',`$4',ifsvg(`&```amp''';',`\&'),`$5')',
`AND_gen(ifelse(`$1',,2,`$1'),ifelse(`$2',N,N)IBANONESEC,`$3',`$4',`$5')')')

                                `AND_gen(n,chars,[wid,[ht]])
                                 0 <= n <= 16;
                                 B=base and straight sides; A=Arc;
                                 [N]NE,[N]SE,[N]I,[N]N,[N]S=inputs or circles;
                                 [N]O=output; C=center location '
define(`AND_gen',
 `define(`m4h',`ifelse(`$3',,AND_wd,`($3)/(L_unit)')')dnl
  define(`m4v',`ifelse(`$4',,ifelse(`$3',,AND_ht,AND_ht/(AND_wd)*m4h),
   `($4)/(L_unit)')')define(`dna_',`$2')dnl
 [ sc_draw(`dna_',B,dnl
   `line from svec_(m4h-m4v/2,-m4v/2) to svec_(0,-m4v/2) then to svec_(0,m4v/2)\
     then to svec_(m4h-m4v/2,m4v/2)')
  sc_draw(`dna_',A, `Arc: arc cw rad m4v/2 to rsvec_(0,-m4v) \
     with .c at rsvec_(0,-m4v/2)')
  sc_draw(`dna_',NNE,
   `NNE: svec_(m4h-m4v/2,0)+svec_(Rect_(m4v/2+N_diam,45))
    N_NNE: NOT_circle at svec_(m4h-m4v/2,0)+svec_(Rect_(m4v/2+N_rad,45))')
  sc_draw(`dna_',NSE,
   `NSE: svec_(m4h-m4v/2,0)+svec_(Rect_(m4v/2+N_diam,-45))
    N_NSE: NOT_circle at svec_(m4h-m4v/2,0)+svec_(Rect_(m4v/2+N_rad,-45))')
  sc_draw(`dna_',NE, `NE: svec_(m4h-m4v/2,0)+svec_(Rect_(m4v/2,45))')
  sc_draw(`dna_',NI, `m4A_defs(ifelse(`$1',,2,`$1'),N)')
  sc_draw(`dna_',SE, `SE: svec_(m4h-m4v/2,0)+svec_(Rect_(m4v/2,-45))')
  sc_draw(`dna_',NN, `N_NN: NOT_circle at svec_((m4h-m4v/2)/2,m4v/2+N_rad)
                      NN: svec_((m4h-m4v/2)/2,m4v/2+N_diam)')
  sc_draw(`dna_',NS, `N_NS: NOT_circle at svec_((m4h-m4v/2)/2,-m4v/2-N_rad)
                      NS: svec_((m4h-m4v/2)/2,-m4v/2-N_diam)')
  sc_draw(`dna_',NO, `N_Out: NOT_circle at svec_(m4h+N_rad,0)
                      Out: svec_(m4h+N_diam,0)')
  sc_draw(`dna_',O, `Out: svec_(m4h,0)')
  sc_draw(`dna_',N, `N: svec_(0,m4v/2)')
  sc_draw(`dna_',I, `m4A_defs(ifelse(`$1',,2,`$1'))')
  sc_draw(`dna_',S, `S: svec_(0,-m4v/2)')
  sc_draw(`dna_',C, `C: svec_(m4h/2,0)')
  `$5']')

                                `m4A_defs(n,[N])
                                 Input locations, flat face'
define(`m4A_defs',
 `define(`m4m',`m4v/2/min(`$1',Mx_pins-1)*min(`$1',3*(Mx_pins-1))')dnl
  ifelse(eval(`$1'>Mx_pins),1,
   `line from svec_(0, m4m) to svec_(0,m4v/2)
    line from svec_(0,-m4m) to svec_(0,-m4v/2)')
  for_(1,`$1',1,`ifelse(`$2',N,
   `N_In`'m4x: NOT_circle at \
      svec_(-N_rad,m4v/min(`$1',Mx_pins-1)*((`$1'+1)/2-m4x))
    In`'m4x: svec_(-N_diam,m4v/min(`$1',Mx_pins-1)*((`$1'+1)/2-m4x)) ',
   `In`'m4x: svec_(0,m4v/min(`$1',Mx_pins-1)*((`$1'+1)/2-m4x))') ') ')

                                `OR_gate(n,[N][B],wid,ht)
                                 drawn in the current direction
                                 0 <= n <= 16; N=negated inputs, B=box shape'
define(`OR_gate',`ifinstr(`$2',B,
`m4_m4t(`$1',`$2') BOX_gate(m4t,P,`$3',`$4',ifsvg(`>=1',`\geq 1'),`$5')',
`OR_gen(ifelse(`$1',,2,`$1'),ifelse(`$2',N,N)IBAONESEC,`$3',`$4',`$5')')')

                                `NOR_gate(n,[N|B],wid,ht)
                                 0 <= n <= 16; N=negated inputs, B=box shape'
define(`NOR_gate',`ifinstr(`$2',B,
`m4_m4t(`$1',`$2') BOX_gate(m4t,P,`$3',`$4',ifsvg(`>=1',`\geq 1'),`$5')',
`OR_gen(ifelse(`$1',,2,`$1'),ifelse(`$2',N,N)IBANONESEC,`$3',`$4',`$5')')')

                                `XOR_gate(n,[N|B],wid,ht)
                                 0 <= n <= 16; N=negated inputs, B=box shape'
define(`XOR_gate',`ifinstr(`$2',B,
`m4_m4t(`$1',`$2') BOX_gate(m4t,P,`$3',`$4',=,`$5')',
`OR_gen(ifelse(`$1',,2,`$1'),P`'ifelse(`$2',N,N)IBAONESEC,`$3',`$4',`$5')')')

                                `NXOR_gate(n,[N|B],wid,ht)
                                 0 <= n <= 16; N=negated inputs, B=box shape'
define(`NXOR_gate',`ifinstr(`$2',B,
`m4_m4t(`$1',`$2') BOX_gate(m4t,P,`$3',`$4',ifsvg(`>=1',`\geq 1'),`$5')',
`OR_gen(ifelse(`$1',,2,`$1'),P`'ifelse(`$2',N,N)IBANONESEC,`$3',`$4',`$5')')')

                                `OR_gen(n,chars,[wid,[ht]])
                                 0 <= n <= 16;
                                 B=base and straight sides; A=Arcs;
                                 [N]NE,[N]SE,[N]I,[N]N,[N]S=inputs or circles;
                                 [N]P=XOR arc; [N]O=output; C=center '
define(`OR_gen',
 `define(`m4h',`ifelse(`$3',,AND_wd,`($3)/(L_unit)')')define(`m4o',0)dnl
  define(`m4v',`ifelse(`$4',,ifelse(`$3',,AND_ht,AND_ht/(AND_wd)*m4h),
   `($4)/(L_unit)')')define(`dna_',`$2')dnl
 [sc_draw(`dna_',P,`define(`m4o',XOR_off*m4v/(AND_ht))dnl
    arc cw from svec_(0,m4v/2) to svec_(0,-m4v/2) \
      with .c at svec_(-sqrt((OR_rad*m4v/(AND_ht))^2-(m4v/2)^2),0)')
  sc_draw(`dna_',B,dnl
   `line from svec_(m4o+m4h/3,m4v/2) to svec_(m4o,m4v/2)
    arc cw to svec_(m4o,-m4v/2) \
      with .c at svec_(m4o-sqrt((OR_rad*m4v/(AND_ht))^2-(m4v/2)^2),0)
    line to svec_(m4o+m4h/3,-m4v/2)')
  sc_draw(`dna_',A,`define(`m4m',`((m4h*2/3)^2-(m4v/2)^2)/(m4v)')dnl
   ArcN: arc  cw from svec_(m4o+m4h/3, m4v/2) to svec_(m4o+m4h,0) \
     with .c at svec_(m4o+m4h/3,-m4m)
   ArcS: arc ccw from svec_(m4o+m4h/3,-m4v/2) to svec_(m4o+m4h,0) \
     with .c at svec_(m4o+m4h/3,m4m)')
  sc_draw(`dna_',NNE,
   `N_NNE: NOT_circle at svec_(m4o+m4h/3,-m4m)+svec_(Rect_(m4v/2+m4m+N_rad,60))
    NNE: svec_(m4o+m4h/3,-m4m)+svec_(Rect_(m4v/2+m4m+N_diam,60))')
  sc_draw(`dna_',NSE,
   `N_NSE: NOT_circle at svec_(m4o+m4h/3,m4m)+svec_(Rect_(m4v/2+m4m+N_rad,-60))
    NSE: svec_(m4o+m4h/3,m4m)+svec_(Rect_(m4v/2+m4m+N_diam,-60))')
  sc_draw(`dna_',NE, `NE: svec_(m4o+m4h/3,-m4m)+svec_(Rect_(m4v/2+m4m,60))')
  sc_draw(`dna_',SE, `SE: svec_(m4o+m4h/3,m4m)+svec_(Rect_(m4v/2+m4m,-60))')
  sc_draw(`dna_',NI, `m4O_defs(ifelse(`$1',,2,`$1'),N)')
  sc_draw(`dna_',NN, `N_NN: NOT_circle at svec_(m4o+m4h/6,m4v/2+N_rad)
                      NN: svec_(m4o+m4h/6,m4v/2+N_diam)')
  sc_draw(`dna_',NS, `N_NS: NOT_circle at svec_(m4o+m4h/6,-m4v/2-N_rad)
                      NS: svec_(m4o+m4h/6,-m4v/2-N_diam)')
  sc_draw(`dna_',NO, `N_Out: NOT_circle at svec_(m4o+m4h+N_rad,0)
                      Out: svec_(m4o+m4h+N_diam,0)')
  sc_draw(`dna_',O, `Out: svec_(m4o+m4h,0)')
  sc_draw(`dna_',N, `N: svec_(m4o+m4h/6,m4v/2)')
  sc_draw(`dna_',I, `m4O_defs(ifelse(`$1',,2,`$1'))')
  sc_draw(`dna_',S, `S: svec_(m4o+m4h/6,-m4v/2)')
  sc_draw(`dna_',C, `C: svec_(m4o+m4h/2,0)')
  `$5']')

                                `m4O_defs(n,[N]) Input locations, curved face'
define(`m4O_defs',
 `define(`m4om',`m4v/2/min(`$1',Mx_pins-1)*min(`$1',3*(Mx_pins-1))')dnl
  ifelse(eval(`$1'>Mx_pins),1,
   `arc ccw from svec_(0,m4v/2) to svec_(0,m4v) + M4O_pos(m4om-m4v) \
      with .c at svec_(-M4O_dst,m4v)
    arc cw from svec_(0,-m4v/2) to svec_(0,-m4v) +M4O_pos(-(m4om-m4v))\
      with .c at svec_(-M4O_dst,-m4v) ')
  define(`m4on',`(eval((`$1'-Mx_pins+1)/2))')
  for_(1,`$1',1,
   `define(`m4oq',`m4v/2/min(`$1',Mx_pins-1)*(`$1'+1-2*m4x)')
    In`'m4x: ifelse(eval(m4x<=m4on),1,`svec_(0, m4v)+M4O_pos(m4oq-m4v)',
      eval(m4x>(`$1'-m4on)),1,        `svec_(0,-m4v)+M4O_pos(m4oq+m4v)',
      `M4O_pos(m4oq)')
    ifelse(`$2',N,
     `N_In`'m4x: NOT_circle at In`'m4x+svec_(-N_rad,0)
      In`'m4x: In`'m4x+svec_(-N_diam,0)')
    ')
  ')
define(`M4O_dst',`sqrt((OR_rad*m4v/(AND_ht))^2-(m4v/2)^2)')
define(`M4O_pos',`svec_(-M4O_dst+sqrt((OR_rad*m4v/(AND_ht))^2-(`$1')^2),`$1')')

                                `IOdefs(linespec,label,[P|N]*,L|R)
                                 Distribute named locations with optional NOT
                                 circles along a line
                                 eg IOdefs(up 1,Z,PNN,R) defines Z1 at 1/6
                                 along the line, NOT circles N_Z2 and N_Z3 to
                                 the right at 1/2 and 5/6 along the line with
                                 Z2 and Z3 labeled at their right edges'
define(`IOdefs',`define(`m4dm',`ifelse(`$3',,1,len(`$3'))')m4IOtmp = rp_ang
eleminit_(`$1')define(`m4da',`$3')define(`m4dv',`ifelse(`$2',,In,`$2')')
  for_(1,m4dm,1,
   `m4dv`'m4x: last line.start+vec_((m4x-1/2)*rp_len/m4dm,0)dnl
    ifelse(substr(m4da,0,1),N,`+svec_(0,ifelse(`$4',R,-)N_diam)
      {N_`'m4dv`'m4x: NOT_circle at m4dv`'m4x+svec_(0,ifelse(`$4',R,,-)N_rad)}')
    define(`m4da',substr(m4da,1))')  rp_ang = m4IOtmp')

                                `BUFFER_gate(linespec,[N|B],wid,ht)
                                 When linespec is blank then the element is
                                 composite and In1, Out, C, NE, and SE are
                                 defined; otherwise the element is drawn as
                                 two-terminal'
define(`BUFFER_gate',`ifinstr(`$2',B,
 `BOX_gate(ifinstr(`$2',N,N,P),P,`$3',`$4',1,`$5')',
 `ifelse(`$1',,
   `BUFFER_gen(ifelse(`$2',N,N)ITOCNESE,`$3',`$4',`$5')',
   `eleminit_(`$1')
    { BUFFER_gen(ifelse(`$2',N,N)ITOC,`$3',`$4',`$5') with .C at last line.c }
    { line to last [].In1; line from last [].Out to 2nd last line.end }
    line invis to rvec_(rp_len,0)')')')

                                `NOT_gate(linespec,[N|B],wid,ht)
                                 When linespec is blank then the element is
                                 composite and In1, Out, C, NE, and SE are
                                 defined; otherwise the element is drawn as
                                 two-terminal'
define(`NOT_gate',`ifinstr(`$2',B,
 `BOX_gate(ifinstr(`$2',N,N,P),N,`$3',`$4',1,`$5')',
 `ifelse(`$1',,
   `BUFFER_gen(ifelse(`$2',N,N)ITNOCNESE,`$3',`$4',`$5')',
   `eleminit_(`$1')
    { BUFFER_gen(ifelse(`$2',N,N)ITNOC,`$3',`$4',`$5') with .C at last line.c }
    { line to last [].In1; line from last [].Out to 2nd last line.end }
    line invis to rvec_(rp_len,0)')')')

                                `BUFFER_gen(chars,wd,ht,[N|P]*,[N|P]*,[N|P]*)
                                 chars: T=triangle, [N]O=output location Out
                                 (NO draws circle N_Out);
                                 [N]I,[N]N,[N]S,[N]NE,[N]SE input locations
                                 C=centre location.  Args 4-6 define In, NE,
                                 and SE argument sequences'
define(`BUFFER_gen',
 `define(`m4h',`ifelse(`$2',,BUF_wd,`($2)/(L_unit)')')define(`m4y',m4h)dnl
  define(`m4v',`ifelse(`$3',,BUF_ht,`($3)/(L_unit)')')dnl
  define(`dna_',`$1')define(`m4z',`N_rad/vlength(m4h,m4v/2)')dnl
 [sc_draw(`dna_',T,
   `line from svec_(m4h,0) to svec_(0,-m4v/2) then to svec_(0,m4v/2) \
     then to svec_(m4h,0)')
  sc_draw(`dna_',NNE,
   `N_NNE: NOT_circle at svec_(m4h/2,m4v/4)+svec_(m4v/2*m4z,m4h*m4z)
    NNE: svec_(m4h/2,m4v/4)+svec_(m4v*m4z,m4h*2*m4z)')
  sc_draw(`dna_',NSE,
   `N_NSE: NOT_circle at svec_(m4h/2,-m4v/4)-svec_(-m4v/2*m4z,m4h*m4z)
    NSE: svec_(m4h/2,-m4v/4)-svec_(-m4v*m4z,m4h*2*m4z)')
  sc_draw(`dna_',NI,`define(`m4y',m4y+N_diam) In1: svec_(-N_diam,0)
    N_In1: NOT_circle at svec_(-N_rad,0)')
  sc_draw(`dna_',NE, `NE: svec_(m4h/2,m4v/4)')
  sc_draw(`dna_',SE, `SE: svec_(m4h/2,-m4v/4)')
  sc_draw(`dna_',NN, `N_NN: NOT_circle at svec_(0,m4v/2+N_rad)
                      NN: svec_(0,m4v/2+N_diam)')
  sc_draw(`dna_',NS, `N_NS: NOT_circle at svec_(0,-m4v/2-N_rad)
                      NS: svec_(0,-m4v/2-N_diam)')
  sc_draw(`dna_',NO,`define(`m4y',m4y+N_diam) Out: svec_(m4h+N_diam,0)
    N_Out: NOT_circle at svec_(m4h+N_rad,0)')
  sc_draw(`dna_',N, `N: svec_(0,m4v/2)')
  sc_draw(`dna_',S, `S: svec_(0,-m4v/2)')
  sc_draw(`dna_',O, `Out: svec_(m4h,0)')
  sc_draw(`dna_',I, `In1: (0,0)')
  sc_draw(`dna_',C, `C: svec_(m4h/2,0)')
  ifelse(`$4',,,`IOdefs(from svec_(0,m4v/2) to svec_(0,-m4v/2),In,`$4',R)')
  ifelse(`$5',,,`IOdefs(from svec_(0,m4v/2) to svec_(m4h,0),NE,`$5')')
  ifelse(`$6',,,`IOdefs(from svec_(0,-m4v/2) to svec_(m4h,0),SE,`$6',R)')
  `$7']')

                                `The comprehensive logic pin:
   lg_pin(location, logicalname, Pinlabel, n|e|s|w[N|L|M][E], pinno, optlen)
     n|e|s|w=orientation;
     N=negated; L=active low out; M=active low in; E=edge trigger'
define(`lg_pin',`ifelse(`$1',,,`move to $1')
  define(`dna_',`substr(`$4',1)')define(`m4lE',)define(`m4lN',)dnl
  define(`m4ld',`ifelse(`$4',,e,`substr(`$4',0,1)')')dnl
  define(`m4lph',`ifelse(m4ld,n,0,m4ld,w,-1,m4ld,s,0,1)')dnl
  define(`m4lpv',`ifelse(m4ld,n,1,m4ld,w,0,m4ld,s,-1,0)')dnl
  define(`m4lpl',`ifelse(`$6',,`lg_plen',`$6')')dnl
  sc_draw(`dna_',E,`define(`m4lE',1)dnl
    { line from rsvec_(lp_xy(0,N_rad)) \
      to rsvec_(lp_xy(-N_diam*sqrt(3)/2,0)) then to rsvec_(lp_xy(0,-N_rad)) }')
  ifelse(`$2',,,
   `{ lg_pintxt(`$2') ifelse(m4ld,w,`ljust_', m4ld,n,`below_',
      m4ld,s,`above_',`rjust_') at Here dnl
      ifxfig(`+(lp_xy(-0.72bp__,0))') dnl
      ifelse(m4lE,1,`+svec_(lp_xy(-N_diam*sqrt(3)/2,0))') }')
  sc_draw(`dna_',N,`define(`m4lN',N)
    { NOT_circle at rsvec_(lp_xy(N_rad,0)) }')
  sc_draw(`dna_',L,`define(`m4lN',M)
    {line from rsvec_(lp_xy(0,
      ifelse(m4ld,w,-,m4ld,s,-)N_rad*3/2)) to rsvec_(lp_xy(N_rad*2.5,0)) }')
  sc_draw(`dna_',M,`define(`m4lN',M)
    { line to rsvec_(lp_xy(N_rad*2.5,
      ifelse(m4ld,w,-,m4ld,s,-)N_rad*3/2)) then to rsvec_(lp_xy(N_rad*2.5,0))}')
  {ifelse(`$3',,,`$3':) line to rsvec_(lp_xy(m4lpl,0))dnl
   ifelse(m4lN,N,`chop N_diam*L_unit chop 0')
   ifelse(`$5',,,`lg_pintxt(`$5') dnl
     at rsvec_(lp_xy(vscal_(1/(L_unit),1pt__,0))) dnl
     ifgpic(
      `ifelse(m4ld,n,`+svec_(lp_xy(4pt__/(L_unit),0)) rjust_ below_',
              m4ld,w,`+svec_(lp_xy(vscal_(1/(L_unit),1pt__,3pt__))) \
                      ljust_ above_',
              m4ld,s,`+svec_(lp_xy(2pt__/(L_unit),0)) rjust_ above_',
                     `+svec_(lp_xy(0,-3pt__/(L_unit))) rjust_ above_') ',
      `ifelse(m4ld,n,`rjust_ below_', m4ld,w,`ljust_ above_',
                     `rjust_ above_')')') dnl
   } ')
define(`lp_xy',`vrot_(`$1',`$2',m4lph,m4lpv)')

                                `Mux(inputs, label, [L][T])
                                 Binary multiplexer: L reverses pin numbering,
                                 T puts Sel at the top'
define(`Mux',`[
Chip: [line from svec_(-Mux_wid/2, 0) to svec_(-Mux_wid/2, Mux_ht/2) \
       then to svec_(Mux_wid/2, (Mux_ht/2)-2) \
       then to svec_(Mux_wid/2, -(Mux_ht/2)+2) \
       then to svec_(-Mux_wid/2, -Mux_ht/2) then to svec_(-Mux_wid/2, 0) ]
  ifelse(`$2',,,"ifsvg(`svg_small($2,75)',`\scriptsize $2')" at Chip.c)
  lg_pin(Chip.e_,,Out,e)
  ifinstr(`$3',T,
    `lg_pin(Chip.n_+svec_(0,-1),$\;$Sel,Sel,n)',
    `lg_pin(Chip.s_+svec_(0,1),$\;$Sel,Sel,s)')
  for_(1,ifelse(`$1',,2,`$1'),1,
   `lg_pin(Chip.w_+svec_(0,
     ifinstr(`$3',L,-)Mux_ht*(0.5+(0.5-m4x)/ifelse(`$1',,2,`$1'))),
     decr(m4x),In`'decr(m4x),w)')
  `$4']')

                                `FlipFlop( D|T|RS|JK, label, boxspec )'
define(`FlipFlop',
  `ifelse(`$1',D,`FlipFlop6(`$2',DCKQNQlb,`$3',`$4')',
  `$1',T,`FlipFlop6(`$2',TCKQNQlb,`$3',`$4')',
  `$1',RS,`FlipFlop6(`$2',RSQNQlb,`$3',`$4')',
  `$1',JK,`FlipFlopJK(`$2',JnCKKnCLRnPRQNQlb,`$3',`$4')',
  `FlipFlop6(`$2',.QNQlb,`$3',`$4')')dnl
  ')
                                `FlipFlop6( label, spec, boxspec )
                                 Customizable 6-pin flipflop:
                                 The spec string contains NQ, Q, CK, S, PR,
                                 CLR to include these pins and lb to print
                                 labels on them.  Preceding any of
                                 these with n negates the pin.  Any other
                                 substring applies to the top left pin, with
                                 . equating to a blank'
define(`FlipFlop6',`[ dnl
 Chip: box ifelse(`$3',,`wid_ FF_wid*L_unit ht_ FF_ht*L_unit',`$3')
   ifelse(`$1',,,"ifsvg(`svg_small($1,75)',`\scriptsize $1')" at Chip.c)
   define(`M4_spec',`ifelse(`$2',,.QnNQlb,$2)')dnl
   M4_ffs(`M4_spec',lb)ifelse(m4_f,-1,`undefine(`m4plb')',`define(`m4plb',T)')
   M4_ffs(`M4_spec',NQ)ifelse(m4_f,-1,,
    `lg_pin(Chip.se_+svec_(0,int(FF_ht/4)),
       ifdef(`m4plb',`lg_bartxt(Q)'),PinNQ,e`'m4N)')
   M4_ffs(`M4_spec',Q)ifelse(m4_f,-1,,
    `lg_pin(Chip.ne_-svec_(0,int(FF_ht/4)),ifdef(`m4plb',Q),PinQ,e`'m4N)')
   M4_ffs(`M4_spec',CK)ifelse(m4_f,-1,,
    `lg_pin(Chip.sw_+svec_(0,int(FF_ht/4)),ifdef(`m4plb',CK),PinCK,wE`'m4N)')
   M4_ffs(`M4_spec',S)ifelse(m4_f,-1,,
    `lg_pin(Chip.sw_+svec_(0,int(FF_ht/4)),ifdef(`m4plb',S),PinS,w`'m4N)')
   M4_ffs(`M4_spec',PR)ifelse(m4_f,-1,, dnl thanks to Louis Dupont
    `lg_pin(Chip.s_,ifdef(`m4plb',PR),PinPR,s`'m4N)')
   M4_ffs(`M4_spec',CLR)ifelse(m4_f,-1,,
    `lg_pin(Chip.n_,ifdef(`m4plb',CLR),PinCLR,n`'m4N)')
   ifelse(M4_spec,,,
    `define(`m4N',ifelse(substr(M4_spec,0,1),n,
      `define(`M4_spec',substr(M4_spec,1))'N,))dnl
     ifelse(M4_spec,.,`define(`M4_spec',)')dnl
     lg_pin(Chip.nw_-svec_(0,int(FF_ht/4)),
       ifdef(`m4plb',`M4_spec'),Pin`'M4_spec,w`'m4N) ')
  `$4']')

                                `FlipFlopJK( label, spec, boxspec )
                                 Customizable JK flipflop (see FlipFlop6)
                                 with pins NQ, Q, CK, PR, CLR, K, top-left.
                                 Include lb in the spec string to print
                                 the labels'
define(`FlipFlopJK',`[ define(`m4u',int(FF_ht/4))dnl
 Chip: box ifelse(`$3',,`wid_ FF_wid*L_unit ht_ FF_ht*L_unit',`$3')
   ifelse(`$1',,,"ifsvg(`svg_small($1,75)',`\scriptsize $1')" at Chip.c)
   define(`M4_spec',`ifelse(`$2',,JnCKKnCLRnPRQNQlb,$2)')dnl
   M4_ffs(`M4_spec',lb)ifelse(m4_f,-1,`undefine(`m4plb')',`define(`m4plb',T)')
   M4_ffs(`M4_spec',NQ)ifelse(m4_f,-1,,
    `lg_pin(Chip.se_+svec_(0,m4u),ifdef(`m4plb',`lg_bartxt(Q)'),PinNQ,e`'m4N)')
   M4_ffs(`M4_spec',Q)ifelse(m4_f,-1,,
    `lg_pin(Chip.ne_-svec_(0,m4u),ifdef(`m4plb',Q),PinQ,e`'m4N)')
   M4_ffs(`M4_spec',CK)ifelse(m4_f,-1,,
    `lg_pin(Chip.w_,ifdef(`m4plb',CK),PinCK,wE`'m4N)')
   M4_ffs(`M4_spec',PR)ifelse(m4_f,-1,,
    `lg_pin(Chip.s_,ifdef(`m4plb',PR),PinPR,s`'m4N)')
   M4_ffs(`M4_spec',CLR)ifelse(m4_f,-1,,
    `lg_pin(Chip.n_,ifdef(`m4plb',CLR),PinCLR,n`'m4N)')
   M4_ffs(`M4_spec',K)ifelse(m4_f,-1,,
    `lg_pin(Chip.sw_+svec_(0,m4u),ifdef(`m4plb',K),PinK,w`'m4N)')
   ifelse(M4_spec,,,
    `define(`m4N',ifelse(substr(M4_spec,0,1),n,
      `define(`M4_spec',substr(M4_spec,1))'n,))dnl
     ifelse(M4_spec,.,`define(`M4_spec',)')dnl
     lg_pin(Chip.nw_-svec_(0,int(FF_ht/4)),
       ifdef(`m4plb',`M4_spec'),Pin`'M4_spec,w`'m4N) ')
  `$4']')
define(`M4_ffs',`define(`m4_f',index($1,`$2'))dnl
  ifelse(m4_f,-1,,`define(`m4_ftmp',eval(m4_f+len($2)))dnl
  define(`m4N',ifelse(substr($1,decr(m4_f),1),n,
   `define(`m4_f',decr(m4_f))'N,))dnl
    define(`$1',substr($1,0,m4_f)`'substr($1,m4_ftmp)) ')')

define(`log_init',`gen_init')
divert(0)dnl
