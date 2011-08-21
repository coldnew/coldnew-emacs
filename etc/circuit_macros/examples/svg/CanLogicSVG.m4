.PS
#CanLogic
ifdef(`L_unit',,`include(HOMELIB_`'liblog.m4)')
log_init

divert(-1)
###########################################################################
                                Macros for automatically drawing 2-layer
                                Boolean functions

                                Style parameters
define(`dotrad_',(0.01*scale))
define(`gatelineth',1)
define(`lineth',0.5)
define(`inputsep',`2*jog')
define(`jog',`(AND_ht/2)*L_unit')

                               `CanLogic(layer gate type,[N],
                                         output gate type,[N],
                                         term,term,...)
                                This is the driver macro.  Terms are
                                strings of variables or of variables
                                preceded by the ~ character. Each
                                variable is one letter e.g.
                                CanLogic(NAND,,OR,N,abcd,a~b,c,~ad)'
define(`CanLogic',
 `define(`terms',`shift(shift(shift(shift($@))))')
#                               Determine required input variables and negated
#                               variables
  Loopover_(`t_',`varloop(`v_',`define(X`'v_)',`define(XN`'v_)',t_)',terms)
#                               Draw the inputs with NOT gates as necessary
  LastInput: Here-(inputsep,0)
  Loopover_(`t_',`varloop(`v_',
   `ifdef(D`'v_,,`ifdef(XN`'v_,`DrawInNotIn(v_)',`DrawIn(v_)')')',
   `ifdef(D`'v_,,`ifdef(X`'v_,`DrawInNotIn(v_)',`DrawNotIn(v_)')')', t_)',
   terms)
#                               Draw 2nd-layer gates
  right_
  LastGateSE: LastInput+(5*jog,-(AND_wd*L_unit*1.5))
  define(`termcount',0)
  Loopover_(`t_',
   `define(`termcount',incr(termcount))DrawLayerGate(G`'termcount,$1,$2,t_)',
    terms)
#                               Draw output gate
  linethick = gatelineth
  OP: m4xpand(`$3'_gate(termcount,$4)) with .Out at \
    0.5<G1.Out,G`'termcount.Out> + (jog/2*(termcount+3)+AND_wd*L_unit,0)
  Out: Here
  linethick = lineth
#                               Connect 2nd-layer gates to the output gate
  VectorConnect(G,termcount,OP)
#                               Connect the inputs and negated inputs to
#                               2nd-layer gates
  define(`termnum',0)
  Loopover_(`t_',
   `define(`termnum',incr(termnum)) ConnectInputs(G`'termnum,t_)',
    terms)
#                               Clean up
  Loopover_(`t_',`DeleteLogDefs(t_)',terms)
 ')
                               `VectorConnect(number of 2nd layer gates,
                                              common 2nd layer gate name,
                                              output gate name)
                                Connect the 2nd-layer gate outputs to the
                                output gate inputs'
define(`VectorConnect',
 `for_(1,`$2',1,
  `line from `$1'm4x.Out right `$3'.In1.x-`$1'm4x.Out.x \
    - jog/2*(`$2'+1-abs(2*m4x-`$2'-1)) \
    then down `$1'm4x.Out.y - `$3'.In`'m4x.y then to `$3'.In`'m4x ')')

                                Draw and label a non-inverted input
define(`DrawIn',
 `LastInput: LastInput+(inputsep,0)
  In`'$1: LastInput
  "svg_it($1)" ljust at LastInput     # Maybe labels should be done externally
  Int`'$1: LastInput
  define(D`'$1)')

                                Draw and label an inverted input
define(`DrawNotIn',
 `LastInput: LastInput+(inputsep,0)
  InN`'$1: LastInput
  "svg_it($1)" ljust at LastInput     # Maybe labels should be done externally
  line down_ 2*jog from LastInput
  linethick = gatelineth
  NOT_gate
  InNt`'$1: Here
  linethick = lineth
  define(D`'$1)')
                                Draw and label an input that is required both
                                inverted and uninverted.
define(`DrawInNotIn',
 `LastInput: LastInput+(inputsep,0)
  In`'$1: LastInput
  "svg_it($1)" ljust at LastInput     # Maybe labels should be done externally
  line from LastInput down jog
  Int`'$1: dot
  LastInput: LastInput+(inputsep,0)
  line to (LastInput,Here) then down_ jog
  linethick = gatelineth
  NOT_gate
  linethick = lineth
  InNt`'$1: Here
  define(D`'$1)')
                               `varloop(`var',ifnotnegated,ifnegated,term)
                                Loop over term variables performing actions'
define(`varloop',`ifelse(`$4',,,substr(`$4',0,1),~,
   `define(`$1',substr($4,1,1)) $3
    varloop(`$1',`$2',`$3',substr($4,2))',
   `define(`$1',substr($4,0,1)) $2
    varloop(`$1',`$2',`$3',substr($4,1))')')')

                                Count gate inputs and mark last appearance
define(`Countinputs',`varloop(`v_',
 `define(`incount',incr(incount)) define(Last`'v_,`$1')',
 `define(`incount',incr(incount)) define(LastN`'v_,`$1')',$2)')

                                Draw a 2nd layer gate
define(`DrawLayerGate',
 `define(`incount',0)
  Countinputs($1,$4)
  ifelse(incount,1,
   `LastGateSE: LastGateSE-(0,jog)
    $1: [ In1:Here; line right AND_wd*L_unit; Out: Here] \
     with .Out at LastGateSE',
   `LastGateSE: LastGateSE-(0,jog+AND_ht*L_unit)
    linethick = gatelineth
    $1: m4xpand(`$2'_gate(incount,$3)) with .se at LastGateSE
    linethick = lineth ')')

                                Connect this gate to its input lines
define(`ConnectInputs',`define(`innum',0) varloop(`v_',
 `define(`innum',incr(innum))
  line from `$1'.In`'innum to (In`'v_,`$1'.In`'innum)dnl
    ifelse(`$1',m4xpand(Last`'v_),`then to In`'v_',`; dot')',
 `define(`innum',incr(innum))
  line from `$1'.In`'innum to (InNt`'v_,`$1'.In`'innum)dnl
    ifelse(`$1',m4xpand(LastN`'v_), `then to InNt`'v_', `; dot')',$2)')

                                Delete definitions to allow more than one
                                circuit per diagram
define(`DeleteLogDefs',`varloop(`v_',
   `undefine(Last`'v_) undefine(D`'v_) undefine(X`'v_)',
   `undefine(LastN`'v_) undefine(D`'v_) undefine(XN`'v_)',$1)')

                                Thanks to Alexander Ray for suggesting the
                                need for something like these macros
###########################################################################
divert(0)dnl

  linethick = lineth

[ CanLogic(AND,,OR,,abcd,a~b,c,~ad) #; line right jog from Out "svg_it(f)" above
  ]
{`"CanLogic(AND,,OR,,abcd,a~b,c,~ad)"' at last [].s -(0,11bp__)}

[ CanLogic(OR,N,NAND,,ab~c,a~bc,ac,~d) #; line right jog from Out "$f$" above
  ] with .sw at last [].se+(0.5,0)
{`"CanLogic(OR,N,NAND,,ab~c,a~bc,ac,~d)"' at last [].s - (0,11bp__)}

.PE
