divert(-1)
   pgf.m4                  Initialization for pgf.

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
define(`m4postprocessor',pgf)

ifdef(`up_',,`include(HOMELIB_`'libgen.m4)divert(-1)')dnl

define(`ultrathinlines_',`linethick = 0.1
  arrowwid = 0.03*scale; arrowht = 0.04*scale ')
define(`verythinlines_',`linethick = 0.2
  arrowwid = 0.04*scale; arrowht = 0.05*scale ')
define(`thinlines_',`linethick = 0.4
  arrowwid = 0.04*scale; arrowht = 0.2/3*scale ')
define(`semithicklines_',`linethick = 0.6
  arrowwid = 0.045*scale; arrowht = 0.8*scale ')
define(`thicklines_',`linethick = 0.8
  arrowwid = 0.05*scale; arrowht = 0.1*scale ')
define(`verythicklines_',`linethick = 1.2
  arrowwid = 0.05*3/2*scale; arrowht = 0.1*3/2*scale ')
define(`ultrathicklines_',`linethick = 1.6
  arrowwid = 0.05*2*scale; arrowht = 0.1*2*scale ')
                                `linethick_(x) set line width to x pt'
define(`linethick_',`linethick = ifelse(`$1',,`0.8',`$1'); dnl
  arrowwid = ifelse(`$1',,`0.05',linethick/16)*scale; dnl
  arrowht = ifelse(`$1',,`0.1',linethick/8)*scale;')

divert(0)dnl
