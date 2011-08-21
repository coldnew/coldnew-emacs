divert(-1)
  darrow.m4                     Macros for double lines and arrows

* Circuit_macros Version 7.0, copyright (c) 2011 J. D. Aplevich, under    *
* the LaTeX Project Public License. The files of this distribution may    *
* be redistributed or modified, provided that this copyright notice is    *
* included and provided that modifications are clearly marked to          *
* distinguish them from this distribution.  There is no warranty          *
* whatsoever for these files.                                             *

                                Installation directory.  You can set HOMELIB_
                                to the null string if you use an environment
                                variable to tell m4 where to search:
ifdef(`HOMELIB_',,
`define(`HOMELIB_',`/home/coldnew/.kde4/share/apps/cirkuit/circuit_macros/')')

                                Default pic processor: gpic.  To make dpic -p
                                the default, change gpic to pstricks in the
                                line below. To make dpic -g and TikZ-PGF the
                                default, change gpic to pgf:
define(`m4defaultprocessor',pstricks)

ifdef(`m4picprocessor',,`include(HOMELIB_`'m4defaultprocessor.m4)divert(-1)')

=============================================================================

                                `Default length:'
ifdef(`elen_',,`define(`elen_',linewid*3/2)')

               `dline(linespec,start truncation,end truncation,width,
                  |-| or -| or |- )'
define(`dline',`m4dl_init(`$1',`$2',`$3',`$4')
  define(`m4lj',`ifelse(`$5',,0,`$5',`-|',0,`linethick bp__/2')')dnl
  define(`m4rj',`ifelse(`$5',,0,`$5',`|-',0,`linethick bp__/2')')dnl
  ifelse(m4postprocessor,pstricks,
   `command "\pscustom[linestyle=none,fillcolor=white,fillstyle=solid]{"
    {line from rvec_(m4lj, m4dlw/2) to rvec_(m4dll-m4rj, m4dlw/2)}
    {line from rvec_(m4dll-m4rj,-m4dlw/2) to rvec_(m4lj,-m4dlw/2)}
    command "}%"',
  m4postprocessor,pgf,
   `{line thick m4dlw/(1bp__) color "white" \
       from rvec_(m4lj,0) to rvec_(m4dll-m4rj,0)}',
  m4postprocessor,svg,
   `{line thick m4dlw/(1bp__) color "white" \
       from rvec_(m4lj,0) to rvec_(m4dll-m4rj,0)}',
  m4postprocessor,postscript,
   `{line thick m4dlw/(1bp__) color "1 1 1" \
       from rvec_(m4lj,0) to rvec_(m4dll-m4rj,0)}',
  m4picprocessor,gpic,, # No white fill for gpic
   `m4_tmpthick = linethick; linethick=0
    shade(1,{line from rvec_(m4lj, m4dlw/2) to rvec_(m4dll-m4rj, m4dlw/2)}
            {line from rvec_(m4dll-m4rj,-m4dlw/2) to rvec_(m4lj,-m4dlw/2)})
    linethick = m4_tmpthick')
  ifelse(m4lj,0,,`{line from rvec_(0,-m4dlw/2) to rvec_(0,m4dlw/2)}')
  {line from rvec_(    0, m4dlw/2) to rvec_(m4dll, m4dlw/2)}
  ifelse(m4rj,0,,`{line from rvec_(m4dll,m4dlw/2) to rvec_(m4dll,-m4dlw/2)}')
  {line from rvec_(m4dll,-m4dlw/2) to rvec_(    0,-m4dlw/2)}
   line invis to rvec_(m4dll,0)')

                                Turn left, leaving current location at exit
                                of corner 
define(`dleft',`dnl
  {line from rvec_(0,-dlinewid/2) to rvec_(dlinewid,-dlinewid/2) \
   then to rvec_(dlinewid,dlinewid/2)}
   line invis to rvec_(dlinewid/2,dlinewid/2)
   define(`dir_',up__) manhattan dir_')

                                Turn right, leaving current location at exit
                                of corner
define(`dright',`dnl
  {line from rvec_(0, dlinewid/2) to rvec_(dlinewid, dlinewid/2) \
   then to rvec_(dlinewid,-dlinewid/2)}
   line invis to rvec_(dlinewid/2,-dlinewid/2)
   define(`dir_',dn_) manhattan dir_')

                                Turn arg1 degrees ccw
define(`dturn',`[S: Here
  c = cosd(`$1'); s = sind(`$1'); sgn = sign(`$1')
  if `$1'==0 then {t = 0} else { t = (1-c)/s }
  move to rvec_(dlinewid/2,0)
  A: rvec_(vscal_(dlinewid/2,-1,sgn))
  B: rvec_(vscal_(sgn*dlinewid/2,-t,1))
  C: rvec_(vscal_(dlinewid/2,-1,-sgn))
  D: rvec_(vscal_(sgn*dlinewid/2,t,-1))
  E: B+vec_(vscal_(sgn*dlinewid,s,-c))
  X: 0.5 between B and E
  ifelse(m4postprocessor,pstricks,
   `command "\pscustom[linestyle=none,fillcolor=white,fillstyle=solid]{"
    line from A to B then to E then to D then to C then to A
    command "}%"',
  m4postprocessor,pgf,
   `line thick dlinewid/(1bp__) color "white" \
       from S to Here then to X',
  m4postprocessor,postscript,
   `line thick dlinewid/(1bp__) color "1 1 1" \
       from S to Here then to X',
  m4postprocessor,svg,
   `line thick dlinewid/(1bp__) color "1 1 1" \
       from S to Here then to X',
  m4picprocessor,gpic,, # No white fill for gpic
   `m4_tmpthick = linethick; linethick=0
    shade(1,line from A to B; line from E to D then to C)
    linethick = m4_tmpthick')
  line from A to B
  line from E to D then to C
  `$2'] with .S at Here
  move to last [].X
  Point_((`$1'+rp_ang*rtod_)) ')

                `darrow(linespec, start truncation, end truncation, width,
                   arrow wid,  arrow ht, <- or <-| or |)'
define(`darrow',`m4dl_init(`$1',`$2',`$3',`$4');{
  ifelse(substr(`$7',0,2),<-,
   `move to rvec_(m4dll,0); rp_ang = rp_ang+twopi_/2 dnl
    define(`m4m',`substr(`$7',2)')',`define(`m4m',`$7')')
  ifinstr(m4m,|,,
  `{ line from rvec_(0,m4dlw/2) to rvec_(linethick bp__/2,m4dlw/2) }
   { line from rvec_(0,-m4dlw/2) to rvec_(linethick bp__/2,-m4dlw/2) }')
  define(`m4h',`ifelse(`$5',,m4dlw,(`$5')/2)')dnl
  define(`m4v',`ifelse(`$6',,m4dlw*3/2,(`$6'))')dnl
  ifelse(ifpstricks(T),T,
   `command "\pscustom[linestyle=solid,fillcolor=white,fillstyle=solid]{"',
  ifpgf(T),T,
   `command "\global\let\dpicshdraw=\dpicdraw\global\def\dpicdraw{}"
    command "\dpicshdraw[fill=white]"',
  ifgpic(T),T,,
   `shade(1,')
  {line from rvec_(linethick bp__/2,m4dlw/2) to rvec_(m4dll-m4v,m4dlw/2) \
    then to rvec_(m4dll-m4v,m4h) then to rvec_(m4dll,0) \
    then to rvec_(m4dll-m4v,-m4h) then to rvec_(m4dll-m4v,-m4dlw/2)\
    then to rvec_(linethick bp__/2,-m4dlw/2) ifinstr(m4m,|,
     `then to rvec_(0,-m4dlw/2) then to rvec_(0,m4dlw/2) \
      then to rvec_(linethick bp__/2,m4dlw/2)')}
  ifelse(ifpstricks(T),T,`command "}%"',
    ifpgf(T),T,`command "\global\let\dpicdraw=\dpicshdraw"',
    ifgpic(T),T,,`)')
  ifelse(substr(`$7',0,2),<-,`move to rvec_(m4dll,0); rp_ang=rp_ang+twopi_/2')
    }; line invis to rvec_(m4dll,0)')

                          `dtee([L|R]) Construct tee with tail right, left,
                           or back along current direction, leaving current
                           location at tee centre; eg
                           dline(right_,,t); dtee(R);
                           { darrow(down_,t) }; darrow(right_,t)'
define(`dtee',`rpoint_(to rvec_(dlinewid,0)); move to last line.c
  ifelse(`$1',L,`rp_ang = rp_ang-pi_/2',`$1',R,`rp_ang = rp_ang+pi_/2',`$1')
  {line from rvec_(dlinewid/2,-dlinewid/2) to rvec_(dlinewid/2,dlinewid/2)}
  {line from rvec_(-dlinewid/2,-dlinewid/2) to rvec_(-dlinewid/2,-dlinewid/2)}
  {line from rvec_(-dlinewid/2, dlinewid/2) to rvec_(-dlinewid/2, dlinewid/2)}')

                                Close off line end: `dend([at position])'
define(`dend',`ifelse(`$1',,,`move to substr(`$1',eval(index(`$1',at)+2))')
 {line from rvec_(0,-dlinewid/2) to rvec_(0,dlinewid/2) chop -linethick/2pt__}')

                               `above, below, ljust, rjust but displaced
                                by dlinewid/2 eg "string" dabove(at position)'
define(`dabove',`above ifelse(`$1',,`at Here',`$1')+(0,dlinewid/2)')
define(`dbelow',`below ifelse(`$1',,`at Here',`$1')-(0,dlinewid/2)')
define(`dljust',`ljust ifelse(`$1',,`at Here',`$1')+(dlinewid/2,0)')
define(`drjust',`rjust ifelse(`$1',,`at Here',`$1')-(dlinewid/2,0)')

                               `dline initialization'
define(`deleminit_',`rpoint_(ifelse(`$1',,`to rvec_(linewid,0)',`$1')) ')

                               `m4dl_init_( linespec,T,T,dlinewid )'
define(`m4dl_init',`deleminit_(`$1')
   m4dlw=ifelse(`$4',,dlinewid,`$4')
   m4dll=rp_len ifelse(`$3',,,`-m4dlw/2') ifelse(`$2',,,
    `-m4dlw/2; move to rvec_(m4dlw/2,0)') ')

                               `Standard width of lines'
define(`dlinewid',`0.08*scale')

                                Sets global variables.
define(`darrow_init',`dnl
   ifdef(`if_rpoint__',,`gen_init')
   ')
divert(0)dnl
