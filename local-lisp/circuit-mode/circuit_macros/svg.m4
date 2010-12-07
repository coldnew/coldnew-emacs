divert(-1)
   svg.m4                       Initialization for SVG output.

* Circuit_macros Version 6.71, copyright (c) 2010 J. D. Aplevich, under    *
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
define(`m4postprocessor',svg)
                                Default is to produce svg for web documents
ifdef(`m4textprocessor',,`define(`m4textprocessor',xml)')

ifdef(`up_',,`include(HOMELIB_`'libgen.m4)divert(-1)')dnl

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
`define(`svg_it',`<tspan font-style=\"italic\">`$1'</tspan>')
define(`svg_norm',`<tspan font-style=\"normal\">`$1'</tspan>')
define(`svg_bf',`<tspan font-style=\"bold\">`$1'</tspan>')
define(`svg_small',`<tspan font-size=\"ifelse(`$2',,66,`$2')%\">`$1'</tspan>')
define(`svg_fsize',`<tspan font-size=\"ifelse(`$2',,100,`$2')%\">`$1'</tspan>')
define(`svg_sub',`svg_small(<tspan baseline-shift=\"sub\">`$1'</tspan>,`$2')')
define(`svg_sup',`svg_small(<tspan baseline-shift=\"super\">`$1'</tspan>,`$2')')
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

define(`thinlines_',`linethick = 0.4
  arrowwid = 0.04*scale; arrowht = 0.2/3*scale')
define(`thicklines_',`linethick = 0.8
  arrowwid = 0.05*scale; arrowht = 0.1*scale')
                                `linethick_(x) set line width to x pt'
define(`linethick_',`linethick = ifelse(`$1',,`0.8',`$1'); dnl
  arrowwid = ifelse(`$1',,`0.05',linethick/16)*scale; dnl
  arrowht = ifelse(`$1',,`0.1',linethick/8)*scale;')

divert(0)dnl
