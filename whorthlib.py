# WHORTH - the ugly duckling of forth
# Open sourceish - licence still to be chosen
# Therapy project so far only usefull for play - but feel free to play!
# (c) Lars Hallberg, WIP-FL: lah2180@gmail.com

"""WHORTH: Canon pedia aka std lib.

This is first incomplete draft of a cannon pedia defining basic
words for Whorth. It is not in any form stable in ether api or
implementation! Largely completely untested an in experimental form.

Uses Pythons duck-typing in part because it almost imposable not to
in Python and in part because it neat. Goal is to keep part of
duck typing going forward - No, Whorth is not and will not be a
standard FORTH - it will remain an ugly duckling.
"""

__whorth_name__ = 'WHORTH CANON'

#
#  Constant state of mess!
#
#  Be warned.
#

import whorth as whr
import time as tm
import math
import random as rnd

def whrp_pycomp(env):
	"""Python 'asm' compile words and some compile support."""
	D = env.pydef
	d = env.addoc
	D('py:',  '>',  _pycolon);   d(_pycolon)
	D('py>',  '>',  _py_gt);     d(_py_gt)
	D('pyd>', '>',  _py_doc);    d(_py_doc)
	D("D#'",  '>',  _wo_doc);    d(_wo_doc)
	D('pystacktrace', '>', 'st.append(env.err_pyst)')
	d('Get python stack trace of latest error as a string')

def whrp_basic(env):
	"""Basic whorth words."""
	D = env.pydef
	d = env.addoc
	# Stack manipulations
	D('drop',  'x >',         'st.pop()')
	d('Drop top of stack.')
	D('nip',   'x _ > _',     'st[-2]=st[-1]\nst.pop()')
	d('Drop second value of stack (swap drop).')
	D('dup',   'x0 > x0 x0',     'st.append(st[-1])')
	d('Duplicate top value of stack.')
	D('dupu',   'x1 _ > x1 x1 _',     'st.append(st[-1]); st[-2] = st[-3]')
	d('Duplicate second value of stack (over swap).')
	D('swap',  'x1 x0 > x0 x1',   'st[-2], st[-1] = st[-1], st[-2]')
	d('Swap the two top values on stack.')
	D('swapu',  'x2 x1 _ > x1 x2 _',   'st[-3], st[-2] = st[-2], st[-3]')
	d('Swap the two values under top on stack (swap rot rot).')
	D('over',  'x1 x0 > x1 x0 x1', 'st.append(st[-2])')
	d('Copy second value of stack to the top.')
	D('tuck',  'x1 x0 > x0 x1 x0',
		'st[-2],st[-1]=st[-1],st[-2]; st.append(st[-2])')
	d('Copy top value of stack under the second one (swap over).')
	D('rot',   'x2 x1 x0 > x1 x0 x2',
		'st[-3],st[-2],st[-1]=st[-2],st[-1],st[-3]')
	d('Rotate third value on stack to top.')
	D('rott',   'x2 x1 x0 > x0 x2 x1',
		'st[-3],st[-2],st[-1]=st[-1],st[-3],st[-2]')
	d('Rotate top value on stack to third (rot rot).')

	# Basic arithmetics
	D('neg',   'x > x',    'st[-1]=0-st[-1]')
	D('abs',   'x > x',    'st[-1]=abs(st[-1])')
	D('inv',   'x > x',    'st[-1]=1/st[-1]')
	D('int',   'x > i',    'st[-1]=int(st[-1])')
	D('float', 'x > i',    'st[-1]=float(st[-1])')
	D('min',   'x x > x',  'a=st.pop(); st[-1]=min(st[-1], a)')
	D('max',   'x x > x',  'a=st.pop(); st[-1]=max(st[-1], a)')

	D('+',     'x x > x',  'a=st.pop()\nst[-1]+=a')
	D('-',     'x x > x',  'a=st.pop()\nst[-1]-=a')

	D('1+',    'x > x',    'st[-1]+=1')
	D('1-',    'x > x',    'st[-1]-=1')
	D('2+',    'x > x',    'st[-1]+=2')
	D('2-',    'x > x',    'st[-1]-=2')
	D('4+',    'x > x',    'st[-1]+=4')
	D('4-',    'x > x',    'st[-1]-=4')
	D('8+',    'x > x',    'st[-1]+=4')
	D('8-',    'x > x',    'st[-1]-=4')
	D('16+',    'x > x',    'st[-1]+=4')
	D('16-',    'x > x',    'st[-1]-=4')

	D('*',     'x x > x',  'a=st.pop()\nst[-1]=st[-1]*a')
	D('**',    'x x > x',  'a=st.pop()\nst[-1]=st[-1]**a')
	d('Exponetial.')
	D('/',     'x x > x',  'a=st.pop()\nst[-1]=st[-1]/a')
	d('Division - give float result.')
	D('//',    'i i > i',  'a=st.pop()\nst[-1]=st[-1]//a')
	d('Integer division - give integer result.')

	D('%',     'x2 x1 > x',  'a=st.pop()\nst[-1]=st[-1]*a/100')
	d('Persent - x1% of x2 (or wise versa).')
	D('%+',    'x2 x1 > x',  'a=st.pop()\nst[-1]+=st[-1]*a/100')
	d('Increse x2 with x1%.')
	D('%-',    'x2 x1 > x',  'a=st.pop()\nst[-1]-=st[-1]*a/100')
	d('Decrese x2 with x1%.')
	D('%of',   'x2 x1 > x',  'a=st.pop()\nst[-1]=(st[-1]/a)*100')
	d('x2 is x % of x1.')

	D('mod',   'x x > x',  'a=st.pop()\nst[-1]=st[-1]%a')
	D('/mod',  'x2 x1 > y2 y1','st[-2],st[-1]=divmod(st[-2],st[-1])')
	d('y2 is int div of x2 and x1, y1 is the reminder.')
	D('round', 'x > x',    'st[-1]=round(st[-1])')
	D('roundd','x i > x',  'a=st.pop()\nst[-1]=round(st[-1],a)')
	d('Round x to i significant digits.')

	# Comparison and logic.
	D('~=',  'f f > b', 'a=st.pop()\nst[-1]=math.isclose(st[-1],a)')
	d('Approximately equal.')
	D('=',   'x x > b', 'a=st.pop()\nst[-1]=st[-1]==a')
	d('Equal.')
	D('!=',  'x x > b', 'a=st.pop()\nst[-1]=st[-1]!=a')
	d('Not equal')

	D('>=',  'x x > b', 'a=st.pop()\nst[-1]=st[-1]>=a')
	d('Greater then equal.')
	D('<=',  'x x > b', 'a=st.pop()\nst[-1]=st[-1]<=a')
	d('Less then equal.')
	D('>',   'x x > b', 'a=st.pop()\nst[-1]=st[-1]>a')
	d('Greater then.')
	D('<',   'x x > b', 'a=st.pop()\nst[-1]=st[-1]<a')
	d('Less than.')

	D('?AND',  'x1 x2 > x', 'a=st.pop()\nst[-1]=st[-1] and a')
	d('x1 if x1 eval false else x2.')
	D('?OR',  'x1 x2 > x', 'a=st.pop()\nst[-1]=st[-1] or a')
	d('x1 if x1 eval true else x2.')

	D('AND',  'x x > b', 'a=st.pop()\nst[-1]=-bool(st[-1] and a)')
	D('NAND', 'x x > b', 'a=st.pop()\nst[-1]=-(not(st[-1] and a))')
	D('OR',   'x x > b', 'a=st.pop()\nst[-1]=-bool(st[-1] or a)')
	D('NOR',  'x x > b', 'a=st.pop()\nst[-1]=-(not(st[-1] or a))')
	D('XOR',  'x x > b', 'a=st.pop()\nst[-1]=-(bool(st[-1])!=bool(a))')
	D('SAME', 'x x > b', 'a=st.pop()\nst[-1]=-(bool(st[-1])==bool(a))')

	D('NOT', '> b',     'st[-1]=-(not(st[-1]))')
	D('BOOL','> b',     'st[-1]=-bool(st[-1])')
	#D('T',   '> b',     'st.append(True)')
	#D('F',   '> b',     'st.append(False)')

	# Bitwise
	D('and',  'x x > x', 'a=st.pop()\nst[-1]= st[-1]&a')
	D('nand', 'x x > x', 'a=st.pop()\nst[-1]= ~(st[-1]&a)')
	D('or',  'x x > x', 'a=st.pop()\nst[-1]= st[-1]|a')
	D('nor', 'x x > x', 'a=st.pop()\nst[-1]= ~(st[-1]|a)')
	D('xor',  'x x > x', 'a=st.pop()\nst[-1]= st[-1]^a')
	D('same', 'x x > x', 'a=st.pop()\nst[-1]= ~(st[-1]^a)')
	D('not',  '> f',     'st[-1]= ~st[-1]')

	# printing
	D('ss.',      '>',
		'print(" ".join(whr.whr_s(x) for x in env.sst))')
	D('m.',      '>',
		'print(" ".join(whr.whr_rs(x) for x in env.w_mem))')
	D('sh.', 'x x >', 'm=st.pop()\nprint(whr.shdots(st.pop(),m), end=" ")')

#wod_comp = #D: D""""Whorth interpreter and compiler words."""
whrp_comp = r"""
#" compiler words "#
imp' pycomp     imp' input

#" Whorth compiler "#
py> IM (>) whr.Whorth.setIM
D#"Mark prev word as immediate, so it is called also during compile."#
py> ; (>) _scolon IM    D#"End compile and store word to word list."#
py> : (>) _colon
D<<<#"Start compile of word. following word is name of word being defined. That
is followed by optional signature in ( ) ... space is not needed inside the
parentheses. Following words are compiled to memory forming a list that is
executed in order later when the word is run. Numbers are stored as
literals and pop on the stack when the word execute.

Immediate (IM) words are run during compile, Ex 'IF' 'compile>' 'call>' '""'

';' is the word ending the compile. The new word is not added until then
so there is special IM words to do recursive calls in lib.recur"#>>>

#" Here - compile position. "#
py: here (> adr) st.append(len(env.w_mem)) ;
D#"Current compile position in memory."#
py: >here  (x >) env.w_mem.append(st.pop()) ;
D#"push to current compile position as a stack."#
py: here>  (> x) st.append(env.w_mem.pop()) ;
D#"pop from current compile position as a stack."#

#" Nil - Whorths None "#
py: Nil (> nil) st.append(None) ; D#"Put Nil (Whorths None) on stack."#
py: isNil (x > b) st.append(st.pop() is None) ; D#"Test if Nil."#

py: callword (>) env.call(enw.w) ;      D#"Call word in wordbuf"#
###"
py: compword (>) _compw(env, env.w) ;   D#"Compile word in wordbuf"#
"###
py: mkword (> a) env._addic(env.w, len(env.w_mem))
env.dicd[-1] = whr.Wmeta(env.w, len(env.w_mem)) ;
   D#"Make an empty word from name in wordbuf"#

py: w@ (> s) st.append(env.w) ;        D#"Fetch current word as str."#
py: w! (s >) env.w = st.pop() ;        D#"Store string as current word."#
py: c_w@ (> s) st.append(env.c_w) ;    D#"Fetch name of word being compiled."#
py: c_w! (s >) env.c_w = st.pop() ;    D#"Store str to current compile name."#
py: c_mp@ (> i) st.append(env.c_mp) ;
D#"Fetch memory position of word currently being compiled."#
py: c_mp! (i >) env.c_mp = st.pop() ;
D#"Store i as memory position of word currently being compiled."#

#" Look up word information "#
py: look (s > idx) st.append(env.dic[st.pop()]) ;
 D#"Look up index of word named by s."#
py: func@ (idx > i) st.append(env.dicl[st.pop()]) ;
 D#"Fetch function from word idx."#
py: meta@ (idx > m) st.append(env.dicd[st.pop()]) ;
 D#"Fetch meta info from word idx."#

: s' (> s) word w@ ; D#"(s-tick) Get next word as string."#
: ' (> idx) s' look ;
D#"(tick) Look up index (exec token) of following word."#
: f' (> fnc) ' func@ ; D#"(f-tick) look up func for following word."#
: m' (> m) ' meta@ ; D#"(m-tick) Look upp meta info for following word."#

: IM!   (idx >)  func@ >here ;  D#"Compile word by idx."#
#": fIM!  (f >)    >here ;  D#"Compile func f."#

: IM!!  (idx >)  lit> lit> >here   func@ >here   lit> >here >here ;
D#"Make word being compiled compile word by idx."#
#": fIM!! (f >)    lit> lit> >here   >here   lit> >here >here ;
D#"Make word being compiled compile func f."#

: IM!'  (>)  ' IM!  ;  IM  D#"Compile next word regardless of IM"#
: IM!!' (>)  ' IM!! ;  IM
#" : IM!!' (>) lit> lit> >here   f' >here   lit> >here >here ; IM "#
D#"Make the word being compiled compile next word regardless of IM"#
: IM' (>) word callword ; IM  D#"Call next word immediately"#
#" More IM* words in lib.intrp "#
"""
# #S: #py: #D: #:
whrp_box = r"""
#" boxes and constants "#
imp' comp

py: _box (> a) st.append(env.r[-1]+1); env.r[-1] = 0 ; D#"Implement box'"#
py: _const (> a) st.append(env.w_mem[env.r[-1]+1]); env.r[-1] = 0 ;
D#"Implement const'"#

: ebox' (>) word mkword IM!!' _box ;
D##"Empty box. Make a named storage place, a word returning its address
when called for storing values. Do not reserve space - se allot and >here
for ways to reserve and set memory. Or use box' for single value."##

: box' (x >) ebox' >here ; 
D##"Make a named storage place, a word returning its address when called
for storing values. Initiate to the value on the stack reserving space
for that value."##

: const' (c >) word mkword IM!!' _const >here ;
D##"Make a named constant, a word returning its value when called. Set
it's value from the top value on stack."##

py: allot (i >) env.w_mem.extend([0] * st.pop()) ;
D#"Allots (reserve) i cells of memory. see box'"#

<###"
py: var> (> a) env.word(); env._addic(env.w, len(env.w_mem))
env.dicd[-1] = whr.Wmeta(env.w, len(env.w_mem)); _compw(env, '_box') ;
py: var@> (> a) env.word(); env._addic(env.w, len(env.w_mem))
env.dicd[-1] = whr.Wmeta(env.w, len(env.w_mem)); _compw(env, '_box') ;
"###>
"""

whrp_input = r"""
#" Input environment "#
imp"pycomp"

#" Word scanner "#
py: skip (>) env.skip() ;
D#"Skip to next word. If 'inp' is set may trigger a read to the input buffer."#
py: scan (>) env.scan() ; D#"Scan to end of word, newer past input buffer"#
py: word (>) env.word() ;
D##"Parse next word from input to wordbuf. If 'inp' is set may trigger a
read to the input buffer (actually done by skip)."##

#" Manual input "#
py> input (prmpt > inp) whr.Whorth.input

py: prompt@ (> i1 i0) st.append(env.pn); st.append(env.pnn) ;
D##"Fetch interactive prompt settings."##
py: prompt! (i i >) env.pnn=st.pop(); env.pn=st.pop() ;

#" input buffer "#
py: ip@ (> s) st.append(env.ip) ;        D#"Fetch input pointer."#
py: ip! (s >) env.ip = st.pop() ;        D#"Store to input pointer."#
py: ib@ (> s) st.append(env.ib) ;        D#"Fetch input buffer."#
py: ib! (s >) env.ib = st.pop() ;        D#"Store to input buffer."#

py: w_in@ (> f) st.append(env.w_in) ; #"Fetch callback - see w_in! for more."#
py: w_in! (f >) env.w_in = st.pop() ;
##"Store a callback that is called by word (skip actually) when the input
buffer run empty. Should refill the buffer."##

py: mty@ (> f) st.append(env.i_mty) ; #"Fetch callback - se mty! for more."#
py: mty! (f >) env.i_mty = st.pop() ;
##"Store mty (empty) callback - should be func or Nil. The mty callback is
called on input of empty buffer. Make it possible to set response to empty
lines in interactive use."##
"""

whrp_pst = r"""
#" Parser stack - new sidestack notation"#
imp' comp

py: pstlen (> i) st.append(len(env.pst)) ; D#"Get len (depth) of parse stack."#
py: pst@ (idx > pst) i=st.pop(); assert(i>=0); st.append(env.pst[i]) ;
D#"Fetch n:th pst (parse stack) element indexed from bottom."#
py: pst@c (n > pst) st.append(env.pst[st.pop() % len(env.pst)]) ;
D#"Fetch n:th pst (parse stack) element circularly indexed from bottom."#
py: pst0> (> pst) st.append(env.pst[-1]) ;
D#"Get copy of top pst element new side stack style"#
py: pst@> (n > pst) i=st.pop(); assert(i>=0); st.append(env.pst[-1-i]) ;
D#"Get copy of n:th pst element from top (new side stack notation)"#

#"Assuming pst top below, word wanting pst element on stack starts with -"#
py: -mkpstl (pst >) p = st.pop();
if p.lst is None: p.lst = [] ; D#"Make pst lst (if needed)."#
: mkpstl (>) pst0> -mkpstl ;   D#"Make pst lst (if needed)."#

py: -psttok@ (pst > tok) st.append(st.pop().tok) ;
D#"Fetch token from pst element on stack."#
py: -psttok! (tok pst >) p=st.pop(); p.tok = st.pop() ;
D#"Store token to pst element on stack."#
py: -pstptxt@ (pst > s) st.append(st.pop().ptxt) ;
D#"Fetch post text from pst element on stack."#
py: -inpstptxt (s pst > s) p=st.pop(); st.append(-(st.pop() in p.ptxt)) ;
D#"True if s in ptxt (post text) of pst element on stack."#
py: -pstptxt! (s pst >) p=st.pop(); p.ptxt = st.pop() ;
D#"Store post text to pst element on stack."#
py: -pstl@ (pst > l) st.append(st.pop().lst) ;
D#"Fetch list from pst element on stack."#
py: -pstfnc@ (pst > fnc) st.append(st.pop().efnc) ;
D#"Fetch end func from pst element on stack."#
py: -pstfnc! (fnc pst >) p=st.pop(); p.efnc = st.pop() ;
D#"Store end func to pst element on stack."#

#" Shortcut for pst top item."#
: inpstptxt  (s > flag)  pst0> -inpstptxt ;
D#"True if s in ptxt (post text) of top pst element."#
: psttok@  (> tok)  pst0> -psttok@ ;
D#"Fetch token from top pst element."#
: psttok!  (tok >)  pst0> -psttok! ;
D#"Store token to top pst element."#
: pstl@   (> l)    pst0> -pstl@ ;
D#"Fetch list from top pst element."#
: pstfnc@ (> fnc)  pst0> -pstfnc@ ;
D#"Fetch end func from top pst element."#
: pstfnc! (fnc >)  pst0> -pstfnc! ;
D#"Store end func to top pst element."#

#" Shortcuts for pst top items list."#
: pstl>   (> l)    pst0> -pstl@ l> ;
: >pstl   (> l)    pst0> -pstl@ >l ;
: pstl0>  (> l)    pst0> -pstl@ l0> ;
: >0pstl  (> l)    pst0> -pstl@ >0l ;
: pstl1>  (> l)    pst0> -pstl@ l1> ;
: >1pstl  (> l)    pst0> -pstl@ >1l ;
: pstl2>  (> l)    pst0> -pstl@ l2> ;
: >2pstl  (> l)    pst0> -pstl@ >2l ;

"""

whrp_interp = r"""
imp' comp      imp' meta      imp' if

#" interpreter / compiler envirionment. "#
py: cmpl@ (> flag) st.append(env.compiling()) ;
D#"Fetch compiling flag. True if we are currently compiling."#

#" Call whorth words "#
py: call_s   (s >)   env.call(st.pop()) ;      D#"Call word named by s."#
py: call     (idx >) env.call_idx(st.pop()) ;  D#"Call word with idx."#
py: call_fnc (fnc >) env.call_fnc(st.pop()) ;  D#"Call function."#

: IM@ (idx > IMflag) meta@ m_IM@ ; D#"Fetch IM flag for word by idx"#
: IM? (idx >)  dup IM@ if{ call }el{ IM! }then ; D#"See IM?'"#
: IM?'    (>)  ' IM? ; IM
D##"Run IM words and compile other words. This is what the compiler do and
this word is for writing compilers rather then IM words."##

: IM?! (idx >)  dup meta@ m_IM@ if{ IM! }el{ IM!! }then ; D#"See IM?!'"#
: IM?!'    (>)  ' IM?! ; IM
D##"Make the word being compiled compile the following word if it is not
immediate (IM) and run it if it is. Effectively moving the compiling
behaviour of following word to the word being compiled."##

: .' (>) s'   cmpl@ if{ IM?!' lit>   >here
                        "f" inpstptxt if{ IM?!' sfrmt }then   IM?!' _.
                   }el{ _. }then ; IM
D#"Convenient printing of strings ie: ."Hello World""#

py> interp (>) whr.Whorth.interp
#"py> compl (>) compl"#
py: sh (>) env.sh() ;          D#"Start an interpreter shell."#
"""

whrp_mem = r"""
#" Memory access "#
imp' pycomp

py: mem@ (i > x) st.append(env.w_mem) ; D#"Push memory as list onto stack."#
py: @ (adr > x) a=st.pop(); assert(a>=0); st.append(env.w_mem[a]) ;
 D#"Fetch value from memory pos adr."#
py: ! (x adr >) a=st.pop(); assert(a>=0); env.w_mem[a]=st.pop() ;
 D#"Store x in memory by pos adr."#
py: mlen (> i) st.append(len(env.w_mem)) ;  D#"get length of memory."#

<<<#"Not much use treating all mem as circular buffer but alow indexing
from end of mem by negative indexes."##
py: @c (i > x) st.append(env.w_mem[st.pop() % len(env.w_mem)]) ;
 D#"Fetch value from memory by circular index i."#
py: !c (x i >) a=st.pop(); env.w_mem[a % len(env.w_mem)]=st.pop() ;
 D#"Store x in memory by circular index i."#


#" Questionable word much assuming mem being a list like in pyWhorth "#
py: m0> (> x) st.append(env.w_mem[-1]) ;
 D#"Copy value from end of memory. Deprecated?"#>>>
"""

whrp_str = r"""
#" String words - mostly superseded by str/train parsing."#
imp' interp      imp' mem

<<<#"
: word' (> s) word w@ ; D#"Scan next word from input and return as str."#
: >word' (> s) lit> lit> >here word' >here ; IM
D#"Scan next word from input and compile it as a literal string."#

py> '""' (> s) _str IM
D##'String literal. Compile literal if compiling else return the str. Need
as usual space round word like: "" this is a str ""'##
py: ' (> s) _str(env, st, "'") ; IM
D##"String literal. Compile literal if compiling else return the str.
Need as usal space round word like: ' this is a str '"##

py> /"" (> s) _str                D#'Non immediate string literal. se ""'#
py: /' (> s) _str(env, st, "'") ; D#"Non immediate string literal. se '"#
py: st"" (> s) _str(env, st, '""', false) ; IM
D#'Stack str, return str even when compiling. se ""'#
py: st' (> s) _str(env, st, "'", false) ; IM
D#"Stack str, return str even when compiling. se '"#
"#>>>

py: SP (> s) st.append(' ') ;   D#"Return a string with one space."#
py: NL (> s) st.append('\n') ;  D#"Return a string with a new line."#

"""

whrp_meta = r"""
#" Meta info "#
imp' comp

##" : meta' (>) ' meta@ ;   D#"Get meta info for following word."##

py: m_w@ (m > s) st.append(st.pop().w) ;
 D#"Get word name from meta info."#
py: m_idx@ (m > idx) st.append(st.pop().idx) ;
 D#"Get word index from meta info."#
py: m_sig@ (m > s) st.append(st.pop().sig) ;
 D#"Get word signature from meta info."#
py: m_doc@ (m > s) st.append(st.pop().doc) ;
 D#"Get meta info's doc string."#
py: m_doc! (s m >) d = st.pop(); st.pop().doc = d ;
 D#"Store string as meta info's doc string."#
py: m_flag@ (m > flag) st.append(st.pop().flag) ;
 D#"Get flags from meta info."#
: m_IM@ (m > IMflag) m_flag@ 1 and ;
 D#"Get flags from meta info and select the immediate bit."#
py: m_wsrc@ (m > s) st.append(st.pop().wsrc) ;
 D#"Get Whorth source from meta info (if defined with :)."#
py: m_pybdy@ (m > s) st.append(st.pop().pybdy) ;
 D#"Get python body from meta info (if defined with py:)."#
py: m_pyfn@ (m > s) st.append(st.pop().pyfn) ;
 D#"Get the python function from meta info."#
py: m_pyname@ (m > s) st.append(getattr(st.pop().pyfn, '__name__', '')) ;
 D#"Get python func name from meta info (if defined with py>)."#
"""

whrp_help = r"""
#" Help "#
imp' meta      imp' str      imp' if

: help (>) <<"
Whorth is a ugly duckling of Forth. It use duck typing and don't adhere to
standard Forth in a fair bit of ways. It use postfix notation like Forth
so You write (spaces are important):

            '1 + 2 * 3'  as:  '2 3 * 1 +'
    '(1.0 - 3.4) * 5.6'  as:  '1.0 3.4 - 5.6 *'

Values is taken and stored on a stack. That is also how Valus goes in and
out of words (think functions). The top of the stack is shown in the
prompt and You can non-destructively print the whole stack with the word
's.' or destructively print the top value with '.' (dot).

Experiment a little and then press enter on an empty line to continue...">>
"help2" look func@ mty! . ;      D#"Print general help info."#

: help2 (>) <<"
Spaces are important in Forth, they separate words and need to go between
everything. That make the parser dead simple and easy to extend. Whorth add
word breaking on the inside of "" strings and parentheses (train parsing)
and check that they are balanced. That make more 'normal' literals
possible and it is made in a way that is at least equally easy to extend.

Whorth's if is an example of a train using {}:

   flag if{do this if true}else{do this}then continue here in both cases

If can only be used in compiled words see ':' further down. Whorth's if also
have a lot of speacial features - se "help' if" or "lsi' lib.if".

Welcome back to the prompt and use an empty line to continue...">>
"help3" look func@ mty! . ;

: help3 (>) <<"
The words 'dup drop nip swap over rot' are the main means to manipulate the
stack. >sst and sst> also move values to/from the side stack
witch make juggling values easier (stand in for Forth return stack).

The word "lsw" list all loaded words, "help' word" print help for a word
(including it's source). You can import words with "imp' path.to.pack" To
look around You can use "lsi' path" Standard words are in lib so "lsi' lib"
show the packages (already loaded packages is marked with *). "imp' lib.time"
as an example import time words. "lsi' lib.time" print the source for it.

Welcome back to the prompt and use an empty line to continue...">>
"help4" look func@ mty! . ;

: help4 (>) <<"
The word ':' is the compiler used to define new words - try "help' :" to
learn more. "py:" and "py>" is for defining new words in python - check help
there to! "help'" also show the source for the words so You can figure it
out. Here is an simplified example of a Forth word (.") defined in Whorth:

            : .' (>) s' . ;

":" is the compiler, ".'" is the word being defined, "(>)" is the stack
signature, "s'" fetch the next word from input and put it on the stack as
a string, "." print it and ";" end the compile and store the new word to
the dictionary. How do .' become ."? see next page.

Welcome back to the prompt and use an empty line to continue...">>
"help5" look func@ mty! . ;

: help5 (>) <<"
Ex:   ."Hello World"   .' for_one_word   .<|"fancy"|>

In FORTH You would need a space after '."' but WHORTH don't need that
(making hello world one char shorter). Whorth turn "text" into string, and
if there is a prefix (like ."str") the prefix is called with ' added. That's
how we used .' to define ." above. The last fancy example uses decorators,
any string of "<>|*#" reversed on ether side of the string. That make it possible to quote anything. If You use at least one # the string will turn
into a comment and disappear from the word stream unless it have a prefix.
The prefix is then called with "#'" added. The prefix "D#'" is defined and
add documentation to the latest word. Use it like below and the
documentation will turn up when using help'.

   : .' (>)  s' . ; D#"Print a string but no compiling."#

Yes, the real .' also compiles so its usable in words...

Welcome back to the prompt and use an empty line to continue...">>
"help6" look func@ mty! . ;

: help6 (>) <<"
Immediate words are word that run during compile rather then being compiled.
This make it possible to add things to the language itself, this is how if
statements and loops are written and this is how we have to make a .' that
works during compile. This is the one defined in lib.interp:

: .' (>) s'   cmpl@ if{ IM?!' lit>   >here   IM?!' . }el{ . }then ; IM

The IM at the end is what flag the word as immediate. Sometimes You do want
to compile immidiate words "IM!' imword" will compile imword whether it is
immediate or not. IM!!' will make the word we writing compile the following
word into the word that are compiling when the word we writing are executed.
That is useful when writing immediate word. IM?!' is even more useful as it
make the right thing whether the word we want to compile are immediate or
not. More to be written about this, don't worry if You don't get it all - it
is far to short!">>
"help7" look func@ mty! . ;

: help7 (>) <<"
Whorth is far from complete and a moving target. It use python as its
assembler but the long plan is to also support wasm, js and possible more.
Error messages are horrible and more for debugging Whorth then Whorth code
(and not even good for that). It is a therapy project recovering from long
illness and progress will be slow and unsteady. To get a python stack trace
of latest error use:

     pystacktrace .

This is it for this help. You go back to the prompt and empty lines will
do nothing. But You can call 'help' again. More help like this and with
more functions are planed. I do believe 'at the prompt' is the right
place for documentation and tutorials. Have fun playing around!

And Yhea, You exit by entering 'q' on an empty line - but who wanna quit?
">> Nil mty! . ;

: helpidx (idx >) meta@ dup m_wsrc@ if{NL over m_wsrc@ +
   }el{dup m_pybdy@}if{NL "py: " + over m_w@ +
      " (" + over m_sig@ + ")  " + over m_pybdy@ + "  ; " +
   }el{NL "py> " + over m_w@ + " (" + over m_sig@ + ")  " +
      over m_pyname@ +
   }then over m_IM@ if{" IM " +}then NL + .
   dup m_doc@ if{m_doc@ NL + . }el{drop}then ;
D#"Print help for word with index idx."#

: help' ' helpidx ;    D#"Print help for following word."#
"""
# is
whrp_stack = r"""
#" Stacks and access "#

imp' pycomp

py: st@ (> l) st.append(st) ;       D#"Fetch stack as list."#
py: sst@ (> l) st.append(env.sst) ; D#"Fetch the side stack as list."#

py: s@ (n > x) n=st.pop(); st.append(st[-n-1]) ;
D##"Fetch the n:ts element from stack, indexed from top that is 0. Index
as if the argument don't exist."##
py: s! (x n >) a=st.pop(); v=st.pop(); st[-a-1]=v ;
D##"Store x to the n:ts element of the stack, indexed from top that is 0.
Index as if the arguments don't exist."##
<###"
py: s>rem (n > x) a=st.pop(); st.append(st.pop(-a-1)) ;
py: ins>s (x n >) a=st.pop(); v=st.pop(); st.insert(-a-1,v) ;
"###>
py: slen (> i) st.append(len(st)) ;       D#"Length (depth) of stack."#
py: sslen (> i) st.append(len(env.sst)) ; D#"Length (depth) of side stack."#

py: >sst (x >) env.sst.append(st.pop()) ; D#"Push value to side stack."#
py: sst> (> x) st.append(env.sst.pop()) ; D#"Pop value from the side stack."#
py: >0sst (x >) env.sst[-1] = st.pop() ;
D#"Stor value to (overwite) the top value of side stack."#
py: >1sst (x >) env.sst[-2] = st.pop() ;
D#"Stor value to (overwite) the first under value of side stack."#
py: >2sst (x >) env.sst[-3] = st.pop() ;
D#"Stor value to (overwite) the first under value of side stack."#
py: >!sst (x i >) i=st.pop(); assert(i>=0); env.sst[-i-1] = st.pop() ;
D#" Store value to side stack at stack position i."#
py: sst0> (> x) st.append(env.sst[-1]) ;
D#"Fetch top value from side stack."#
py: sst1> (> x) st.append(env.sst[-2]) ;
D#"Fetch first under value from side stack."#
py: sst2> (> x) st.append(env.sst[-3]) ;
D#"Fetch first under value from side stack."#
py: sst@> (i > x) i=st.pop(); assert(i>=0); st.append(env.sst[-i-1]) ;
D#" Fetch value at pos i on the side stack."#

#" Old notation "#
###"
py: push (x >) env.sst.append(st.pop()) ; D#"Push value to side stack."#
py: pop (> x) st.append(env.sst.pop()) ;  D#"Pop value from the side stack."#

py: pushcp (x > x) env.sst.append(st[-1]) ;
D#" Push value to side stack leaving a copy on the stack."#
py: popcp (> x) st.append(env.sst[-1]) ;
D#" Pop value from the side stack leaving a copy on the side stack."#

py: ss@ (n > x) a=st.pop(); st.append(env.sst[-a-1]) ;
D##"Fetch the n:ts element from side stack, indexed from top that is 0 -
negative numbers index from the bottom."##
py: ss! (x n >) a=st.pop(); env.sst[-a-1]=st.pop() ;
D##"Store x to the n:ts element of the side stack, indexed from top that is 0
- negative numbers index from the bottom."##
###"
<###"
py: ss@r (n > x) a=st.pop(); st.append(env.sst.pop(-a-1)) ;
py: ss!i (x n >) a=st.pop(); env.sst.insert(-a-1,st.pop()) ;
"###>
"""
whrp_list = r"""
#" Lists "#
imp' comp      imp' box      imp' mem

py: aList (l > l) assert(isinstance(st[-1], list)) ; D#"Assert List."#
py: isList (l > flag) st[-1] = -isinstance(st[-1], list) ;
D#"True (-1) if l is a List."#
py: List (> l) st.append([]) ; D#"Create an empty list"#
py: s/l (... n > l) n=st.pop(); l=st[-n:]; st[-n:]=(); st.append(l) ;
D#"Slash off n values from the stack and re-add them as a list."#
py: l/l (l n > l l) n=st.pop(); l=st[-1][-n:]; st[-1][-n:]=(); st.append(l) ;
D#"Slash off n values from list as new list (split list, mod orig list)."#
: List' (>) List const' ;      D#"Make an empty named list."#
: >List' (l >) aList const' ;  D#"Turn l in to a named List."#

#" New notation, experimental: These are stack ops"#

##" Storing values to top (end) of list as a stack."##
py: >l   (x l >)   l=st.pop(); l.append(st.pop()) ;
D#"push x to top (end) of list as a stack."#
py: >0l  (x l >)   l=st.pop(); l[-1]=st.pop() ;
D#"store x in top (end) pos of list."#
py: >1l  (x l >)   l=st.pop(); l[-2]=st.pop() ;
D#"store x to 1:st under top (end) pos of list."#
py: >2l  (x l >)   l=st.pop(); l[-3]=st.pop() ;
D#"store x to 2:nd under top (end) pos of list."#
py: >!l  (x n l >) l=st.pop(); n=st.pop(); assert(n>=0); l[-n-1]=st.pop() ;
D#"store x to n:th under top (end) pos of list."#

#" These are list/mem only. index from start of list "#
py: l!   (x i l >)  l=st.pop(); i=st.pop(); assert(i>=0); l[i]=st.pop() ;
D#"store x to 0:rot index i in list."#
py: l!c  (x i l >)  l=st.pop(); i=st.pop(); l[i%len(l)]=st.pop() ;
D#"store x to 0:rot circular index i in list."#

#" Extracting values from top (end) of list as a stack."#
py: l>   (l > x)    x=st[-1].pop(); st[-1] = x ;
D#"Pop x off top (end) of list as a stack."#
py: l0>  (l > x)    x=st[-1][-1]; st[-1] = x ;
D#"Fetch x from top (end) pos of list."#
py: l1>  (l > x)    x=st[-1][-2]; st[-1] = x ;
D#"Fetch x from 1:st under top (end) pos of list."#
py: l2>  (l > x)    x=st[-1][-3]; st[-1] = x ;
D#"Fetch x from 2:nd under top (end) pos of list."#
py: l@>  (n l > x)  l=st.pop(); assert(st[-1]>=0); st[-1] = l[-st[-1]-1] ;
D#"Fetch x from n:th under top (end) of list as a stack."#

#" These are list/mem only. index from start of list "#
py: l@   (i l > x)  l=st.pop(); assert(st[-1]>=0); st[-1]=l[st[-1]] ;
D#"Fetch x from 0:rot index i in list l."#
py: l@c  (i l > x)  l=st.pop(); st[-1] = st[-1][st[-1]%len(l)] ;
D#"Fetch x from 0:rot circular index i in list l."#

#" Not a list specific function in a duck universe!!! "#
py: len (l > i) st[-1] = len(st[-1]) ; D#"Get length of top stack item."#

#" Old notation - depricated "#
py: <push (l x > l) st[-2].append(st.pop()) ; D#"push x to end of list."#
py: <pop (l > l x) st.append(st[-1].pop()) ; D#"Pop x of end of list."#
py: <len (l > l i) st.append(len(st[-1])) ; D#"Get the len of list l."#

py: <ext (l1 l0 > l)    l = st.pop(); st[-1].extend(l) ;
D#"Extend list l1 with list l0."#
py: <top (l n > l) i=st.pop(); st[-1] = st[-1][-i:] ;
D#"Extract the top (last) n element from l as a new list."#
py: <bot (l n > l) i=st.pop(); st[-1] = st[-1][:i] ;
D#"Extract the bottom (first) n element from l as a new list."#
"""

whrp_listlit = r"""
#" List literals "#
imp' list      imp' if      imp' stack

: "[" slen >sst ; IM D#"Start List literal."#
: "]" slen sst> - dup 0 > if{s/l}el{drop List}then ; D#"End List literal."#
"# TODO! [ and ] need to be IM to work in words! #"
<###"
: py: <rem (l i > l x)   st[-1] = st[-2].pop(-st[-1]-1) ;
: py: <ins (l x i > l)   st[-3].insert(-st.pop()-1, st.pop()) ;
: py: @len <len (l > i) st.append(len(st.pop())) ;
"###>
"""

whrp_dict = r"""
#" Dictionaries "#
imp' comp

py: aDict (d > d) assert(isinstance(st[-1], dict)) ; D#"Assert Dict."#
py: isDict (d > flag) st[-1] = -isinstance(st[-1], dict) ;
D#"True (-1) if d is a Dict."#
py: Dict (> d) st.append({}) ;
: Dict' (>) Dict const' ;      D#" Create an empty named Dict."#
: >Dict' (d >) aDict const' ;  D#" Make d an named Dict."#

py: dic@ (s d > e) d = st.pop();  e = d[st.pop()];  st[-1] = e ;
D#"Fetch value at key s from dictionary d"#
: dic@' (d > e)  s' dic@ ;
D#"Fetch value from dictionary taking key from following word."#
py: dic! (e s d >) d = st.pop();  k = st.pop();  d[k] = st.pop() ;
D#"Store e in dictionary d with key s."#
: dic!' (e d >)  s' dic! ;
D#"Store e in dictionary taking key from following word."#
py: keys@ (d > l) st[-1] = list(st[-1].keys()) ;
D#"Get keys of dictionary as list."#
"""

# #: sslen (> i) st.append(len(env.w_mem)) ; ppy:

whrp_print_extra = r"""
imp' comp

py: mt. (i >) e=max(1,st.pop());print(len(env.w_mem)-e,':',env.w_mem[-e:]) ;
py: .r (x > x) st[-1] = whr.whr_rs(st[-1]) ;
: r. (x >) .r . ;

py: .s (x > x) st[-1] = whr.whr_s(st[-1]) ;
py: s.s (> x) st.append(env.sdots()) ;

py: sh.s (x x > s) m=st.pop();st[-1] = whr.shdots(st[-1],m) ;
py: sh.r (x x > s) m=st.pop();st.append(whr.shdotr(st.pop(),m)) ;
: rsh.   (x x > s) sh.r . ;
"""

whrp_comp_extra = r"""
imp' comp

py: skips (s >) env.skip(st.pop()) ;
py: scans (s >) env.scan(st.pop()) ;
py: find (s > i) st[0]=env.find(st[0]) ;

#"sfunc (s > i) slook func@"#
#": func' (> i) ' func@ ;"#
#"smeta (s > m) slook meta@"#
: mklit> (x >) lit> lit> >here >here ; IM
"""

whrp_jmp = r"""
#"Jump, call and return words."#
imp' comp    imp' mem

py: jmp (>) env.r[-1] += env.w_mem[env.r[-1]+1] ;
D#"Jump - add value after instruction to the execution pointer."#
py: ?jmp (i >) env.r[-1] += env.w_mem[env.r[-1]+1] if st.pop() else 1 ;
D##"Conditional jump - add value after instruction to the xp (execution
pointer) if i is true, else add 1 to xp (step over the jump addr)."##
py: !?jmp (i >) env.r[-1] += 1 if st.pop() else env.w_mem[env.r[-1]+1] ;
D##"Conditional jump - add value after instruction to the xp (execution
pointer) if i is false, else add 1 to xp (step over the jump addr)."##

: jmp! (frmadr toadr >) over - swap ! ;
D##" Calculate the relative addr and store it in frmadr so a jmp
instruction directly before frmadr will jump to toadr."##
: jmphere (frmadr >) here jmp! ;
D#"Store rel adr to here in frmadr (assuming a jmp instr before that)."#
: jmpthere (toadr >) here swap  0 >here  jmp! ;
D#"Store rel adr to toadr in here (assuming a jmp instr before that)."#

py: ?CALL (flag >) env.r[-1] += not st.pop() ;
D#"Conditional call - Call next word if flag is true, else step over it."#
py: !?CALL (flag >) env.r[-1] += bool(st.pop()) ;
D#"Conditional call - Call next word if flag is false, else step over it."#

py: ?LCALL (flag >) env.r[-1] += (not st.pop()) << 1 ;
D#"Conditional literal call - Call following literal if flag is true, else
step over the literal."#
py: !?LCALL (flag >) env.r[-1] += bool(st.pop()) << 1 ;
D#"Conditional literal call - Call following literal if flag is false, else
step over the literal."#

py: RET (>) env.r[-1] = 0 ;   D#"Return from word."#
py: ?RET (b >) if st.pop(): env.r[-1] = 0 ; D#"Return from word if b is true."#
py: !?RET (b >) if not st.pop(): env.r[-1] = 0 ;
D#"Return from word if b is false."#
"""

whrp_recur = r"""
##"Recursion. Whorth words are not callable until after the definition. So
You need special words to do recursion."##
imp' jmp

: RECUR (>) c_mp@ >here ; IM   D#"Recursion, Call the word being defined."#
: ?RECUR (b >) lit> ?LCALL >here IM!' RECUR ; IM
: ?RECUR (b >) lit> ?LCALL >here IM!' RECUR ; IM
D#"Conditional recursion, Call the word being defined if b is true."#
: !?RECUR (b >) lit> !?LCALL >here IM!' RECUR ; IM
: !?RECUR (b >) lit> !?LCALL >here IM!' RECUR ; IM
D#"Conditional recursion, Call the word being defined if b is false."#

: TRECUR (>) lit> jmp >here c_mp@ mlen - >here ; IM
D#"Tail recursion, Jump to start of word being defined."#
: ?TRECUR (b >) lit> ?jmp >here c_mp@ mlen - >here ; IM
D##"Conditional tail recursion, Jump to start of word being defined if b
is true."##
: !?TRECUR (b >) lit> !?jmp >here c_mp@ mlen - >here ; IM
D##"Conditional tail recursion, Jump to start of word being defined if b
is false."##
"""

whrp_if = r"""
imp' recur      imp' list      imp' pst

: help-if (>) <<"
Words impl 'if{ - }el{ - }then' statement. 'if{' and '}el{' (else) can be
followed by '}if{' (and if) creating a shortcut test without nesting 'if{'.
An '}if{' can be followed by a '}br{' (break) turning it in to an abort if
with cleanup before joining '}el{'. An '}if{' following an '}el{' is
effectively an elif. }fin{ (finalize) collect all previews 'successful'
tests for a common exit code. The whole if statement is ended with '}then'.

Example: 'flag' if{'expensive test'}if{'cleanup'}br{'do stuff on success ...'
         }el{'Get here both if first if fail or after cleanup, ie all but
              success.'
         }then 'continue merged control flow after if statement.'
">> . ;

##"Implemented by storing state in two list on pst, the top one jump
locations for }el{ and the second one jump locations for '}fin{'. '}br{'
also use the top one. '}then' sets all remaining jump locations and
drop state."##

: _ljmp! (l > l)   dup len !?RET   dup l> jmphere  TRECUR ;
D#"Consume list of addr and store relative addr to here in them."#

: ljmp! (l >)   _ljmp!   drop ; D#"Store addrs with _ljmp! and drop list."#

: "if{" (b >) IM!!' !?jmp   mkpstl   List >pstl   List >pstl
              here pstl0> >l   1 >here ; IM
D#"Starting a 'if{ - }el{ - }then' statement. see: help-if"#

: "}el{" (>) IM!!' jmp   here pstl1> >l   1 >here   pstl0> ljmp! ; IM
D#"Collect control flow from failed 'if{ / }if{' and '}br{'. see help-if"#

: "}then" (>) pstl> ljmp!    pstl> ljmp! ; IM
D#"Collect all remaining control flow and end if statement. see help-if"#

: "}if{" (b >) IM!!' !?jmp   here pstl0> >l   1 >here ; IM
D##"Extra conditions on each 'if{' / '}el{' leg. Each 'if{' / '}el{' can
have several '}if{' with or without '}br{'. see: help-if"##

: "}br{" (>) IM!!' jmp   1 >here   pstl0> l> jmphere   here 1- pstl0> >l ; IM
D#"Turn an '}if{' into 'abort if'. see: help-if"#

: "}fin{" (>) IM!!' jmp   here pstl0> >l   1 >here   pstl1> ljmp! ; IM
D##"'finalize': collect control flow from succeeding if{ / }el{ }if{ for
common exit code. see: help-if"##
"""
whrp_IF = r"""
imp' stack      imp' jmp

: help-IF (>) <<"
Forth style IF -- ELSE -- THEN - statement. The WHORTH selection that don't
relay on trains or fancy types like List. Mainly for defining low level words
before train and fancy types are available (see: "help-if"). The words are:

 flag IF     Start IF statement. Run following code block if flag true.
      ELSE   End IF block and start a block run if IF is false (optional).
      THEN   End if statement. Merge control flow.
">> . ;

: IF (flag >) lit> !?jmp >here  here >sst  1 >here ; IM
D#"Start IF statement. Continue if flag true. see: help-IF"#

: ELSE (>) lit> jmp >here   here  1 >here  sst> jmphere  >sst ; IM
D#"jump to THEN and make IF jump to following code block. see: help-IF"#

: THEN (>) sst> jmphere ; IM
D#"End IF statement. Merge control flow. see: help-IF"#
"""

whrp_ITER = r"""
imp' IF

: help-ITER (>) <<"
ITER is WHORTH looping structure that don't relay on trains or fancy types
like List. Mainly for defining low level words before train and fancy types
are available. The words are:

      ITER     Iterate. ITER or FOR start a loop.
      FOR      Iterate with a increment head not run first turn.
      DO       Start body - join first and repeating flow in an FOR loop.
 flag WHILE    Continue while true. jump to after REPEAT if flag is false.
 flag UNTIL    Continue until true. jump to after REPEAT if flag is true.
      LOOP     Loop back to ITER / FOR (use as many You like).
 flag ?LOOP    Take a flag and loop back if true (use as many You like).
      REPEAT   End a loop, loop back to ITER / FOR
 flag ?REPEAT  End a loop, take a flag and loop back if true.

ITER .body. REPEAT is an endless loop. You may add as many LOOP as You
like and only one of WHILE or UNTIL.

FOR .head. DO .body. REPEAT is a variant where head is skipped on the first
turn (mainly for increment code). You may add as many LOOP as You like to
both head and body and only one of WHILE or UNTIL to head or body.
">> . ;

: ITER (>) 0 >sst   here >sst ; IM
D#"Start a iteration (loop), see: help-ITER"#

: FOR (>) lit> jmp >here   1 >here   0 >sst   here >sst ; IM
D#"Start a iteration (loop) with a head, see: help-ITER"#
: DO (>) sst0> 1- jmphere ; IM
D#"End head and start body in a FOR iteration (loop). see: help-ITER"#

: UNTIL   (flag >)  lit>   ?jmp >here   here >1sst   1 >here ; IM
D#"Break if flag true. MAX one UNTIL or WHILE - see: help-ITER"#

: WHILE  (flag >)   lit>   !?jmp >here  here >1sst   1 >here ; IM
D#"Break if flag false. MAX one UNTIL or WHILE - see: help-ITER"#

: LOOP        ( >)  lit>   jmp >here   sst0> jmpthere ; IM
D#"Loop back to start of iteration (ITER / FOR). see: help-ITER"#
: ?LOOP   (flag >)  lit>  ?jmp >here   sst0> jmpthere ; IM
D#"Loop back to start of iteration if flag true (ITER / FOR). see: help-ITER"#

: REPEAT        (>) lit> jmp >here     sst> jmpthere
             sst> dup  IF  jmphere  ELSE  drop  THEN ; IM
D#"End a iteration (loop). Loop back to ITER / FOR. see: help-ITER"#
: ?REPEAT  (flag >) lit> ?jmp >here    sst> jmpthere
             sst> dup  IF  jmphere  ELSE  drop  THEN ; IM
D#"End a iteration. Loop if flag true. see: help-ITER"#
"""
# : qq ITER dup 10 < WHL dup 1+ dup 9 < WHL dup 3 + RPT 42 ELSE 666 THEN 9 ;

def whrp_rand(env):
	"""Add random functions to a WHORTH env."""
	D = env.pydef
	d = env.addoc
	# Random numbers
	D('rnd',   '> f',     'st.append(rnd.random())')
	d(rnd.random)
	D('rndu',  'f f > f', 'e=st.pop()\nst[-1]=rnd.uniform(st[-1],e)')
	d(rnd.uniform)
	D('bvrnd', 'f f > f', 'e=st.pop()\nst[-1]=rnd.betavariate(st[-1],e)')
	d(rnd.betavariate)

	D('gvrnd', 'f f > f', 'e=st.pop()\nst[-1]=rnd.gammavariate(st[-1],e)')
	d(rnd.gammavariate)
	D('vvrnd', 'f f > f', 'e=st.pop()\nst[-1]=rnd.vonmisesvariate(st[-1],e)')
	d(rnd.vonmisesvariate)
	D('wvrnd', 'f f > f', 'e=st.pop()\nst[-1]=rnd.weibullvariate(st[-1],e)')
	d(rnd.weibullvariate)

	D('lvrnd', 'f f > f', 'e=st.pop()\nst[-1]=rnd.lognormvariate(st[-1],e)')
	d(rnd.lognormvariate)
	D('nvrnd', 'f f > f', 'e=st.pop()\nst[-1]=rnd.normalvariate(st[-1],e)')
	d(rnd.normalvariate)
	D('trirnd','f f > f', 'e=st.pop()\nst[-1]=rnd.triangular(st[-1],e)')
	d(rnd.triangular)

	D('evrnd', 'f > f',   'st[-1]=rnd.expovariate(st[-1])')
	d(rnd.expovariate)
	D('pvrnd', 'f > f',   'st[-1]=rnd.paretovariate(st[-1])')
	d(rnd.paretovariate)
	D('gauss', 'f f > f', 'e=st.pop()\nst[-1]=rnd.gauss(st[-1],e)')
	d(rnd.gauss)

	D('rndi',  'i i > i', 'e=st.pop()\nst[-1]=rnd.randint(st[-1],e)')
	d(rnd.randint)
	D('rndb',  'i > i',   'st[-1]=rnd.getrandbits(st[-1])')
	d(rnd.getrandbits)

	D('rseed', '>',       'rnd.seed()')
	d(rnd.seed)
	D('seed',  'i >',     'rnd.seed(st.pop())')
	d(rnd.seed)

def whrp_math(env):
	"""Add math functions to a WHORTH env."""
	D = env.pydef
	d = env.addoc
	# Math
	D('cp-',   'x x > x',  'a=st.pop()\nst[-1]=math.copysign(st[-1],a)')
	d(math.copysign)
	D('rem',   'x x > x',  'a=st.pop()\nst[-1]=math.remainder(st[-1], a)')
	d(math.remainder)

	D('ceil',  'x > x',    'st[-1]=math.ceil(st[-1])')
	d(math.ceil)
	D('floor', 'x > x',    'st[-1]=math.floor(st[-1])')
	d(math.floor)
	D('prec',  'x > x',    'st[-1]=math.ulp(st[-1])')
	d(math.ulp)

	# Trigometry.
	D('acos',     'x > x',   'st[-1]=math.acos(st[-1])')
	d(math.acos)
	D('acosh',    'x > x',   'st[-1]=math.acosh(st[-1])')
	d(math.acosh)
	D('asin',     'x > x',   'st[-1]=math.asin(st[-1])')
	d(math.asin)
	D('asinh',    'x > x',   'st[-1]=math.asinh(st[-1])')
	d(math.asinh)
	D('atan',     'x > x',   'st[-1]=math.atan(st[-1])')
	d(math.atan)
	D('atanh',    'x > x',   'st[-1]=math.atanh(st[-1])')
	d(math.atanh)
	D('atan/',    'x x > x', 'a=st.pop()\nst[-1]=math.atan2(st[-1],a)')
	d(math.atan2)

	D('cos',      'x > x',   'st[-1]=math.cos(st[-1])')
	d(math.cos)
	D('cosh',     'x > x',   'st[-1]=math.cosh(st[-1])')
	d(math.cosh)
	D('sin',      'x > x',   'st[-1]=math.sin(st[-1])')
	d(math.sin)
	D('sinh',     'x > x',   'st[-1]=math.sinh(st[-1])')
	d(math.sinh)
	D('tan',      'x > x',   'st[-1]=math.tan(st[-1])')
	d(math.tan)
	D('tanh',     'x > x',   'st[-1]=math.tanh(st[-1])')
	d(math.tanh)

	D('rad->d',   'x > x',   'st[-1]=math.degrees(st[-1])')
	d(math.degrees)
	D('rad->rev', 'x > x',   'st[-1]=math.degrees(st[-1])/360')
	d('Radians to revolutions via degrees:'); d(math.degrees)
	D('d->rad',   'x > x',   'st[-1]=math.radians(st[-1])')
	d(math.radians)
	D('rev->rad', 'x > x',   'st[-1]=math.radians(st[-1]*360)')
	d('Revolutions to radians:'); d(math.radians)

	D('dist',     'x > x',   'st[-1]=math.dist(st[-1])')
	d(math.dist)
	D('pi',       '> x',     'st.append(math.pi)')
	d('The famus delisius constant pi 3.14...')

	# Exponetials and logarithms + some.
	D('exp',   'x > x',   'st[-1]=math.exp(st[-1])')
	d(math.exp)
	D('exp1-', 'x > x',   'st[-1]=math.expm1(st[-1])')
	d(math.expm1)
	D('e',     '> x',     'st.append(math.e)')
	d('The constant e 2.72...')

	D('m:2log','x > x i', '(st[-1],a)=math.frexp(st[-1])\nst.append(a)')
	d(math.frexp)
	D('10log', 'x > x',   'st[-1]=math.log10(st[-1])')
	d(math.log10)
	D('2log',  'x > x',   'st[-1]=math.log2(st[-1])')
	d(math.log2)
	D('log',   'x > x',   'st[-1]=math.log(st[-1])')
	d(math.log)
	D('1+log', 'x > x',   'st[-1]=math.log1p(st[-1])')
	d(math.log1p)

	D('logam', 'x > x',   'st[-1]=math.lgamma(st[-1])')
	d(math.lgamma)
	D('gam',   'x > x',   'st[-1]=math.gamma(st[-1])')
	d(math.gamma)
	D('*2**',  'x i > x', 'a=st.pop()\nst[-1]=math.ldexp(st[-1], a)')
	d(math.ldexp)

	D('sqrt',  'x > x',   'st[-1]=math.sqrt(st[-1])')
	d(math.sqrt)
	D('sqrti', 'x > i',   'st[-1]=math.isqrt(st[-1])')
	d(math.isqrt)

	# Combinatorics
	D('n/k', 'i i > i', 'a=st.pop()\nst[-1]=math.comb(st[-1],a)')
	d(math.comb)

def whrp_fuzlog(env):
	"""Fuzzy logic, shorthands. 2:trueish 4:halftrue 9:true."""
	D = env.pydef

	# Fuzzy logic - handles unknown values somewhat.
	# positive value is truthiness, negative falseness and 0 is unknown.
	D('fAND', 'x x>x', 'a=st.pop()\nst[-1]=min(st[-1],a)')
	D('fOR',  'x x>x', 'a=st.pop()\nst[-1]=max(st[-1],a)')
	D('fXOR', 'x x>x', 'a=st.pop()\nst[-1]=min(max(st[-1],a),-min(st[-1],a))')
	D('fNAND','x x>x', 'a=st.pop()\nst[-1]=-min(st[-1],a)')
	D('fNOR', 'x x>x', 'a=st.pop()\nst[-1]=-max(st[-1],a)')
	D('fSAME','x x>x', 'a=st.pop()\nst[-1]=-min(max(st[-1],a),-min(st[-1],a))')
	D('fNOT', 'x > x', 'st[-1]=-st[-1]')

	# Threshold words to get truth flag out of a fuzzy logic value.
	D('2T',  'x > b',   'st[-1]=-(st[-1] >= 2)')
	D('2F',  'x > b',   'st[-1]=-(st[-1] <= 2)')
	D('2U',  'x > b',   'st[-1]=-(abs(st[-1]) < 2)')

	D('4T',  'x > b',   'st[-1]=-(st[-1] >= 4)')
	D('4F',  'x > b',   'st[-1]=-(st[-1] <= 4)')
	D('4U',  'x > b',   'st[-1]=-(abs(st[-1]) < 4)')

	D('9T',  'x > b',   'st[-1]=-(st[-1] >= 9)')
	D('9F',  'x > b',   'st[-1]=-(st[-1] <= -9)')
	D('9U',  'x > b',   'st[-1]=-(abs(st[-1]) < 9)')

whrp_time = """
#"Time functions."#
imp> pycomp

py: sleep    (f >)   tm.sleep(st.pop()) ; pyd> tm.sleep

py: unclk    (> i)   st.append(tm.time_ns()) ; pyd> tm.time_ns
py: ufclk    (> f)   st.append(tm.time()) ; pyd> tm.time

py: nclk     (> i)   st.append(tm.monotonic_ns()) ; pyd> tm.monotonic_ns
py: fclk     (> f)   st.append(tm.monotonic()) ; pyd> tm.monotonic

py: tnclk    (> i)   st.append(tm.thread_time_ns()) ; pyd> tm.thread_time_ns
py: tfclk    (> f)   st.append(tm.thread_time()) ; pyd> tm.thread_time

py: procnclk (> i)   st.append(tm.process_time_ns()) ; pyd> tm.process_time_ns
py: procfclk (> f)   st.append(tm.process_time()) ; pyd> tm.process_time

py: pnclk    (> i)   st.append(tm.perf_counter_ns()) ; pyd> tm.perf_counter_ns
py: pfclk    (> f)   st.append(tm.perf_counter()) ; pyd> tm.perf_counter
"""

def whrp_silly(env):
	"""Some silly and testing stuff, probably broken a lot of the time."""
	D = env.pydef;
	# Word Zero... Raise exception, it should not be called
	D('WordZero', '>', w_zero)
	D('test', '>', _test)

	# number to from str id. not actually used for something but useful.
	D('nstr', 'i > s', 'st[-1] = nstr(st[-1])')
	D('strn', 's > i', 'st[-1] = strn(st[-1])')

def _compw(env, w):
	env.w_mem.append(env.dicl[env.dic[w]])

def _wstartcomp(env, flag=0):
	txt = env.ip - len(env.w)
	env.word()
	env.c_w = env.w
	env.c_flag = flag # | __IW
	env.c_txt = env.ib[txt:env.ip]
	env.skip()
	if env.ib[env.ip] != '(':
		env.c_sig = ''
	else:
		env.word()
		sp = env.ip
		l = len(env.pst)
		while (env.w != ')') or (len(env.pst) > l):
			env.word()
		env.c_sig = env.ib[sp:env.ip-1].strip()
	env.c_txt = env.ib[txt:env.ip]

def _pycolon(env, st):
	"""Compile a python word. It ends with ';'"""
	env.po = 'py: '
	_wstartcomp(env)
	env.skip()
	stxt = len(env.c_txt)
	etxt = stxt
	env.word()
	c_w = env.c_w
	while env.w or (env.pst and (env.pst[-1].es[0] in '\'"')):
		if env.w == ';':
			env.po = ''
			txt = env.c_txt[stxt:etxt]
			if not txt:
				env.err.append("no python code in word {}\n{}".format(c_w,
					env.mk_err()))
				raise SyntaxError("no python code in word {}".format(c_w))
			env.pydef(c_w, env.c_sig, txt, env.c_flag)
			env.c_w = ''
			return
		etxt = len(env.c_txt)
		env.word()
	env.po = ''
	env.err.append("Input end py compiling word {}\n{}".format(c_w,
		env.mk_err()))
	raise SyntaxError("Input end py compiling word {}".format(c_w))

def _scolon(env, st):
	"""End compiling word."""
	c_w, env.po, env.c_w = env.c_w, '', ''
	if c_w:
		env.w_mem.append(None);
		env._addic(c_w, env.c_mp)
		env.dicd[-1] = whr.Wmeta(
			c_w, env.c_mp, flag=env.c_flag, sig=env.c_sig, wsrc=env.c_txt)

def _colon(env, st):
	"""Compile a Whorth word. Ends with ';'"""
	_wstartcomp(env);
	env.c_mp = len(env.w_mem);

def _py_gt(env, st):
	"""Tie a word to a python function."""
	env.po = 'py> ';
	_wstartcomp(env);
	env.word()
	try:
		env.pydef(env.c_w, env.c_sig, eval(env.w, env.glob))
	except:
		env.err.append("Error pydef {} {} {}\n{}".format(env.c_w,
			env.c_sig, env.w, env.mk_err()))
		raise
	env.c_w = '';
	try:
		env.dicd[-1].doc = eval('getattr({}, "__doc__", "")'.format(env.w),
			env.glob)
	except:
		env.err.append("Error eval py {}.__doc__\n{}".format(env.w,
			env.mk_err()))
		raise
	env.po = ''

def _py_doc(env, st):
	"""Copy doc from a python func __doc__ to latest word."""
	env.po = 'pyd> ';
	env.word()
	try:
		env.addoc(eval(env.w))
	except:
		env.err.append("Error addoc {}\n{}".format(env.w,
			env.mk_err()))
		raise
	env.po = ''

def _wo_doc(env, st):
	"""Copy doc from a whorth comment to latest word."""
	env.word()
	if env.w:
		d = env.dicd[-1].doc
		env.dicd[-1].doc = d + '\n\n' + env.w if d else env.w

def nstr(n):
	"""Convert int to gobly str."""
	s = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#'
	xs = s[n & 63]
	y = (n >> 6)
	while y:
		xs = xs + s[y & 63]
		y = y >> 6
	return xs

def strn(gs):
	"""Convert gobly str to int."""
	s = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#'
	r = 0
	for c in reversed(gs):
		r = (r << 6) + s.index(c)
	return r

def w_zero(env, _=None):
	env.err.append("WordZero called\n{}".format(env.mk_err()))
	raise Exception('WordZero called')

def _test(env, st):
	"""Dummy for tmp testcod runable from whorth."""
	l = []
	d = {}
	for i in range(100000):
		s = nstr(i)
		s1 = nstr(100000-i)
		l.append(s)
		d[s] = s1
	t = time.time()
	for i in range(100000):
		l[i]
	t = time.time() - t
	print('list:', 100000 / t)
	for i in range(100000):
		d.values [i]
	t = time.time() - t
	print('list:', 100000 / t)


