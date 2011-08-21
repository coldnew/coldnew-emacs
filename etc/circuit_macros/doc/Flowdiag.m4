% `Flowdiag.m4'
.PS
define(`shadellipse',`{ellipse $1 fill invis
  define(`m4h',`1.0 pt__')dnl
  ellipse invis fill 0.5 wid last ellipse .wid+linethick pt__ \
    ht last ellipse .ht + linethick pt__ at last ellipse+(m4h,-m4h)}
  ellipse fill 1 $1')

  shadellipse "{\tt .m4}" "diagram" 
  arrow right linewid*2/3

  {shadellipse(with .b at Here+(-linewid*0.5,boxht/3+linewid/3)) \
    "{\tt .m4}" "macros"}
  arrow from last ellipse.b down linewid/3 then right linewid*0.5
  move down boxht/6; right

  shadebox(box wid boxht "{\bf m4}")
  arrow right linewid/2

  shadebox(box "{\bf pic}" "interpreter")
  arrow right from Here-(0,boxht/6)

  {shadellipse(with .b at Here+(-linewid*0.5,boxht/3+linewid/3)) \
    "{\tt .tex}" "files"}
  {arrow from last ellipse.b down linewid/3 then right linewid*0.5}
  move up boxht/6; right

  shadebox(box "\LaTeX" "or" "PDFlatex")
  arrow right linewid*2/3

  shadebox(box wid boxht "{\tt .dvi}" "or" "{\tt .pdf}")

.PE
