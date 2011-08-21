% Flow.m4
divert(-1)
                                `Some experimental flow-chart macros.  
                                In the following, every subchart is in
                                [] brackets, with .N, .S, .E and .W
                                internally defined.  Follow this convention
                                if you define compound constructs.

                                For flowchart examples using pic only, see
                                "Creating Flowcharts in LaTeX Using the pic
                                Language," Miguel Torres-Torriti, 2008
                                http://www2.ing.puc.cl/~mtorrest/downloads
                                  /latex/flowchart.pdf'

                                `Default sizes:'
define(`fboxwid',linewid*2)
define(`fboxht',linewid*0.75)
define(`farrowht',linewid*0.5)

                                `The basic task box:
                                fbox( "contents", [wid x ht y] )
                                The first argument is the text string,
                                the second is an optional size specification.
                                To adjust the box size to the size of the
                                typeset text, see the discussion of the
                                \boxdims macro in the manual.'
define(`fbox',`[B:box ifelse(`$2',,`wid fboxwid ht fboxht',`$2') \
  fill_(fillval) `$1'
  N: B.n; S:B.s; E:B.e; W: B.w
  ]')

                                `If-then
                                ftest( "test text", [wid x ht y] )'
define(`ftest',`[B:box ifelse(`$2',,`wid fboxwid*0.8 ht fboxht*1.2',`$2') invis
  N: B.n; S: B.s; E: B.e; W: B.w 
  shade(fillval,line from 0.5 between last box.n and last box.e \
    to last box.e then to last box.s then to last box.w then to last box.n \
    then to 0.5 between last box.n and last box.e)
  ifelse(`$1',,,`$1' at B)
  ]')

                                `case statement
     fcase([wid x ht y (of tests)], "Test 1","task 1", ..., "Test n","task n")'
define(`fcase',`[ down; S: Here; xe = S.x
  fcaseloop_(1,$@)
  ifelse(`$2',,`E:S; W:S; N:S',`E:(xe+linewid/2,B1.E.y); W:T1.W; N:T1.N')
  fcasearrow_(1,$@)
  arrow from E to (E,S) then to S ]')

  define(`fcaseloop_',`ifelse(`$3',,,
   `T`$1': ftest(`$3',`$2') with .N at S
    B`$1': `$4' with .nw at T`$1'.E+(linewid/2,min(T`$1'.ht,fboxht)/2)
      arrow right linewid/4 from T`$1'.E then down T`$1'.E.y-B`$1'.W.y \
        then to B`$1'.W
    S: (T`$1'.S.x,min(T`$1'.S.y,B`$1'.S.y)-linewid/3)
      ifelse(`$5',,`line',`arrow') from T`$1'.S to S
      xe = max(xe,B`$1'.E.x)
      fcaseloop_(incr($1),`$2',shift(shift(shift(shift($@)))))')')
  define(`fcasearrow_',`ifelse(`$3',,,
   `arrow from B`$1'.E to (E,B`$1'.E)
    fcasearrow_(incr($1),`$2',shift(shift(shift(shift($@)))))')')
  
                                `while statement with explicit test
                   fwhiledo("test text", [wid x ht y], "task text", ["T"|"F"] )'
define(`fwhiledo',`[ down
  T: ftest(`$1',`$2')
    ifelse(`$4',,"T",`$4') above ljust at T.E
    arrow right linewid/2 from T.E
  B: `$3' with .W at Here
  E: B.E; W: T.w; S: T.S
    arrow up max(linewid/4,T.n.y-B.n.y+arrowht*1.5) from B.N then left B.x-T.x
  N: Here
    arrow to T.n ]')

                                `repeat statement with explicit test
                         frepeatuntil("test", [wid x ht y], "task", ["F"|"T"] )'
define(`frepeatuntil',`[
  N: Here
  B: `$3' with .N at N
  W: B.W
    arrow down linewid/3
  T: ftest(`$1',`$2')
  E: B.E+(linewid/2+max(0,T.e.x-B.e.x),0)
    arrow from T.e to (E,T) then to E then to B.E
    ifelse(`$4',,"F",`$4') above ljust at T.e
  S: T.S ]')

                                `if-then-else
                fifthenelse( "test", [wid x ht y], [left tasks], [right tasks],
                             ["F"|"left label"], ["T"|"right label"])'
define(`fifthenelse',`[
  T: ftest(`$1',`$2')
  N: T.N
    ifelse(`$5',,"F",`$5') below rjust at T.W
    ifelse(`$6',,"T",`$6') below ljust at T.E
  L: ifelse(`$3',,`T.S; W:T.W; LS:L',`$3 with .ne at ((T.W.x+T.x)/2,T.S.y)
    W: L.W; LS:L.S
    arrow from T.W to (L.N,T.W) then to L.N')
  R: ifelse(`$4',,`T.S; E:T.E; RS:R',`$4 with .nw at ((T.E.x+T.x)/2,T.S.y)
    E: R.E; RS: R.S
    arrow from T.E to (R.N,T.E) then to R.N')
  S: (T.x,min(LS.y,RS.y)-linewid/3)
    arrow from LS to (LS,S)
    arrow from RS to (RS,S)
    line to (LS,Here)
  ]')
divert(0)dnl


% Here is a test file exercising the above definitions
.PS
  linethick_(1.0)
  arrowwid = 0.05
  arrowht = 0.1
  fillval = 0.8
  down

Case: fcase(,
  "Case 1",[fbox("Task 1"); W:last [].W; N:last [].N
            arrow down linewid/3 from last [].S
            fbox("Task 1.5") with .N at Here; E:last [].E; S:last [].S],
  "Case 2",fbox("Task 2" "is bigger",wid fboxwid*1.2 ht fboxht*3/2),
  "Case 3",fbox("Task 3")dnl
  )

   "\sl .N" at Case.N rjust above; "\sl .S" at Case.S rjust below
   "\sl .E" at Case.E ljust; "\sl .W" at Case.W rjust
   "\sl Case statement" below at Case.s+(0,-0.2)

Whiledo: fwhiledo("$i < n$",,fbox("Task")) with .nw at Case.ne+(0.5,0)

  "\sl .N" at Whiledo.N rjust; "\sl .S" at Whiledo.S rjust below
  "\sl .E" at Whiledo.E ljust; "\sl .W" at Whiledo.W rjust
  "\sl While-do" below at Whiledo.s+(0,-0.2)

Repeatuntil: frepeatuntil("$n \geq 5$",,fbox("Task")) \
  with .sw at Case.se + (0.5,0.2)

  "\sl .N" at Repeatuntil.N rjust above; "\sl .S" at Repeatuntil.S rjust below
  "\sl .E" at Repeatuntil.E ljust; "\sl .W" at Repeatuntil.W rjust
  "\sl Repeat-until" below at Repeatuntil.s+(0,-0.2)

Ifthenelse: fifthenelse("$A<B$",,
     fbox("False"),
     fbox("True",ht fboxht*1.2 wid fboxwid)dnl
     ) with .nw at Case.sw +(3,-0.6)

  "\sl .N" at Ifthenelse.N rjust above; "\sl .S" at Ifthenelse.S below rjust
  "\sl .E" at Ifthenelse.E ljust; "\sl .W" at Ifthenelse.W rjust
  "\sl If-then-else" below at Ifthenelse.s+(0,-0.3) \
  "\sl (the True and False tasks are optional)"


# Compound statement:
[ right 
  ellipse fill_(fillval) "Start"
  arrow right linewid/2

  fbox(,"First task") with .W at Here
  arrow down linewid/2 from last [].S

  fifthenelse("Test 1",,
    frepeatuntil("$i \geq 5$",,fbox("Task")),
    fifthenelse("$A < B$",,
      fbox("Left"),
      fbox("Right")dnl
      )dnl
    ) with .N at Here

  A: arrow down 0.25 from last [].S ] with .nw at Case.sw + (0,-1.5)

  "\sl Compound statement" below at last [].A.end
.PE
