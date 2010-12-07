divert(-1)
   pstricks.m4                  Initialization for PSTricks.

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

define(`m4picprocessor',dpic)
define(`m4postprocessor',pstricks)

ifdef(`up_',,`include(HOMELIB_`'libgen.m4)divert(-1)')dnl

define(`thinlines_',`linethick = 0.4
  arrowwid = 0.04*scale; arrowht = 0.2/3*scale
  command "\psset{linewidth=0.4pt}%";')
define(`thicklines_',`linethick = 0.8
  arrowwid = 0.05*scale; arrowht = 0.1*scale
  command "\psset{linewidth=0.8pt}%";')
                                `linethick_(x) set line width to x pt'
define(`linethick_',`linethick = ifelse(`$1',,`0.8',`$1'); dnl
  arrowwid = ifelse(`$1',,`0.05',linethick/16)*scale; dnl
  arrowht = ifelse(`$1',,`0.1',linethick/8)*scale;')

divert(0)dnl
