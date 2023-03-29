# WHORTH - the ugly duckling of forth
# Open sorceish - licence still to be chosen
# Terapy project so far only usefull for play - but feel free to play!
# (c) Lars Hallberg, WIP-FL: lah2180@gmail.com

"""WHORTH: Canon pedia aka std lib.

This is first incomplete draft of a cannon pedia defining basic
words for Whorth. It is not in any form stable in ether api or
implementation! Largly completly untested an in exprimental form.

Uses Pythons ductyping in part because it almost inpossble not to
in Python and in part because it neet. Goal is to keeep part of
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
	"""Pyton 'asm' compile words and some compile suport."""
	D = env.pydef
	d = env.addoc
	D('py:',  '>',  _pycolon);   d(_pycolon)
	D('py>',  '>',  _py_gt);     d(_py_gt)
	D('pyd>', '>',  _py_doc);    d(_py_doc)
	D("D#'",  '>',  _wo_doc);    d(_wo_doc)
	D('pystacktrace', '>', 'st.append(env.err_pyst)')
	d('Get python stacktrace of latest error as a string')

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
	d('Aproximatly equal.')
	D('=',   'x x > b', 'a=st.pop()\nst[-1]=st[-1]==a')
	d('Equal.')
	D('!=',  'x x > b', 'a=st.pop()\nst[q-1]=st[-1]!=a')
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
	D('sh.', 'x x >', 'm=st.pop()\nprint(whr.shdots(st.pop(),m))')

#wod_comp = #D: D""""Whorth intepreter and compiler words."""
whrp_comp = r"""
#" compiler words "#
imp' pycomp     imp' input

#" Whorth compiler "#
py> IM (>) whr.Whorth.setIM
D#"Mark prev word as Imidiate, so it is called also during compile."#
py> ; (>) _scolon IM    D#"End compile and store word to wordlist."#
py> : (>) _colon
D<<<#"Start compile of word. folowing word is name of word being defined. That
is folowed by optional signature in ( ) ... This is an exeption - space is
not neded inad the parranteses. Folowing words are compiled to memory
forming a list that is executed in order later when the word is run. Numbers
are stored as literalls and pop on the stack when the word excecute.

Immidiate IM words are run during compile, Ex 'IF' 'compile>' 'call>' '""'

';' is the word ending the compile. The new word is not added untill then
so there is special IM words to do recursive calls in lib.recur"#>>>

#" Here - compile position. "#
py: here (> adr) st.append(len(env.w_mem)) ;
D#"Current compile position in memory."#
py: >here  (x >) env.w_mem.append(st.pop()) ;
D#"push to curent compile position as a stack."#
py: here>  (> x) st.append(env.w_mem.pop()) ;
D#"pop from curent compile position as a stack."#

#" Nil - Whorts None "#
py: Nil (> nil) st.append(None) ; D#"Put Nil (Whorts None) on stack."#
py: isNil (x > b) st.append(st.pop() is None) ; D#"Test if Nil."#

py: callword (>) env.call(enw.w) ;      D#"Call word in wordbuf"#
py: compword (>) _compw(env, env.w) ;   D#"Compile word in wordbuf"#
py: mkword (> a) env._addic(env.w, len(env.w_mem))
env.dicd[-1] = whr.Wmeta(env.w, len(env.w_mem)) ;
   D#"Make an empty word from name in wordbuf"#

: comp' (>) word compword ; IM     D#"Compile next word"#
: IM' (>) word callword ; IM       D#"Call next word imidiatly"#
"""
# #S: #py: #D: #:
whrp_var = r"""
#" Basic variables "#
imp' comp

py: _var (> a) st.append(env.r[-1]+1); env.r[-1] = 0 ; D#"Implement var>"#
py: _const (> a) st.append(env.w_mem[env.r[-1]+1]); env.r[-1] = 0 ;
D#"Implement const>"#

: var' word mkword comp' _var ;
D##"Make a named variabel, a word returning its address when called for
storing values. Do not reserve space - se allot and >here for ways to
reserv and set memory."##
: const' word mkword comp' _const ;
D##"Make a named constant, a word returning its folowing value when called.
Do not set a value - se >here for way to set memory."##

py: allot (i >) env.w_mem.extend([0] * st.pop()) ;
D#"Allots (reserve) i cells of memory. see var>"#

<###"
py: var> (> a) env.word(); env._addic(env.w, len(env.w_mem))
env.dicd[-1] = whr.Wmeta(env.w, len(env.w_mem)); _compw(env, '_var') ;
py: var@> (> a) env.word(); env._addic(env.w, len(env.w_mem))
env.dicd[-1] = whr.Wmeta(env.w, len(env.w_mem)); _compw(env, '_var') ;
"###>
"""

whrp_input = r"""
#" Input envirionment "#
imp"pycomp"

#" Word scaner "#
py: scip (>) env.scip() ;
D#"Scip to next word. If 'inp' is set may trigger a read to the input buffer."#
py: scan (>) env.scan() ; D#"Scan to end of word, newer past input buffer"#
py: word (>) env.word() ;
D##"Parse next word from input to wordbuf. If 'inp' is set may trigger a
read to the input buffer."##

#" Manuall input "#
py> input (prmpt > inp) whr.Whorth.input

py: prompt@ (> i i) st.append(env.pn); st.append(env.pnn) ;
py: prompt! (i i >) env.pnn=st.pop(); env.pn=st.pop() ;

#" input buffer "#
py: ip@ (> s) st.append(env.ip) ;        D#"Fetch input pointer."#
py: ip! (s >) env.ip = st.pop() ;        D#"Store to input pointer."#
py: ib@ (> s) st.append(env.ib) ;        D#"Fetch input buffer."#
py: ib! (s >) env.ib = st.pop() ;        D#"Store to input buffer."#

py: w_in@ (> f) st.append(env.w_in) ; #"Fetch callbak - se w_in! for more."#
py: w_in! (f >) env.w_in = st.pop() ;
##"Store a callback that is called by word (scip actually) when the input
buffer run empty. Should refill the buffer."##

py: mty@ (> f) st.append(env.i_mty) ; #"Fetch callbak - se mty! for more."#
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
D#"Fetch n:th pst (parse stack) element circulary indexed from bottom."#
py: pst0> (> pst) st.append(env.pst[-1]) ;
D#"Get copy of top pst element new side stack style"#
py: pst@> (n > pst) i=st.pop(); assert(i>=0); st.append(env.pst[-1-i]) ;
D#"Get copy of n:th pst element from top (new side stack notation)"#

py: mkpstl (pst >) p = st.pop();
if p.lst is None: p.lst = [] ; D#"Make pst lst (if needed)."#

py: psttok@ (pst > tok) st.append(st.pop().tok) ;
D#"Fetch token from pst element on stack."#
py: psttok! (tok pst >) p=st.pop(); p.tok = st.pop() ;
D#"Store token to pst element on stack."#
py: pstl@ (pst > l) st.append(st.pop().lst) ;
D#"Fetch context list from pst element on stack."#
py: pstfnc@ (pst > fnc) st.append(st.pop().efnc) ;
D#"Fetch end func from pst element on stack."#
py: pstfnc! (fnc pst >) p=st.pop(); p.efnc = st.pop() ;
D#"Store end func to pst element on stack."#

#" Shortcut for pst top item. <0psttok@ ???"#
: psttok0>  (> tok)  <0pst psttok@ ;
D#"Fetch token from top pst element. <0psttok! ???"#
: >0psttok  (tok >)  <0pst psttok! ;
D#"Store token to top pst element."#
: pstl0>   (> l)    <0pst pstl@ ;
D#"Fetch context list top from pst element."#
: pstfnc0> (> fnc)  <0pst pstfnc@ ;
D#"Fetch end func top from pst element."#
: >0pstfnc (fnc >)  <0pst pstfnc! ;
D#"Store end func top to pst element."#
"""

whrp_interp = r"""
imp' comp

#" interpreter / compiler envirionment. "#
py: w@ (> s) st.append(env.w) ;        D#"Fetch curent word as str."#
py: w! (s >) env.w = st.pop() ;        D#"Store string as curent word."#
py: c_w@ (> s) st.append(env.c_w) ;    D#"Fetch name of word being compiled."#
py: c_w! (s >) env.c_w = st.pop() ;    D#"Store str to current compilename."#
py: c_mp@ (> i) st.append(env.c_mp) ;
D#"Fetch memory position of word curently being compiled."#
py: c_mp! (i >) env.c_mp = st.pop() ;
D#"Stor i as memory position of word curently being compiled."#

#" Look up word information "#
: ' (> s) word w@ ; D#"Get next word as string."#
py: look (s > idx) st.append(env.dic[st.pop()]) ;
 D#"Look up index of word named by s."#
: look' (> idx) ' look ;   D#"look up index of following word."#

py: func@ (idx > i) st.append(env.dicl[st.pop()]) ;
 D#"Fetch function from word idx."#
: func' (> fnc) look' func@ ; D#"look up func for following word."#
py: meta@ (idx > m) st.append(env.dicd[st.pop()]) ;
 D#"Fetch meta info from word idx."#

#" Call whorth words "#
py: call_s (s >) env.call(st.pop()) ;           D#"Call word named by s."#
py: call_idx (idx i >) env.call_idx(st.pop()) ; D#"Call word with idx."#
py: call_fnc (i >) env.call_fnc(st.pop()) ;     D#"Call function."#

py> interp (>) whr.Whorth.interp
#"py> compl (>) compl"#
py: sh (>) env.sh() ;          D#"Start an interpreter shell."#
"""

whrp_mem = r"""
#" Memory access "#
imp' pycomp

py: mem@ (i > x) st.append(env.w_mem) ; D#"Push memory as list onto stack."#
py: m@ (i > x) a=st.pop(); assert(a>=0); st.append(env.w_mem[a]) ;
 D#"Fetch value from memory pos i."#
py: m! (x i >) a=st.pop(); assert(a>=0); env.w_mem[a]=st.pop() ;
 D#"Store x in memory by index i."#
py: mlen (> i) st.append(len(env.w_mem)) ;  D#"get length of memory."#

##"Not much use treating all mem as circular buffer but alow indexing
from end of mem by negative indexes."##
py: m@c (i > x) st.append(env.w_mem[st.pop() % len(env.w_mem)]) ;
 D#"Fetch value from memory by circular index i."#
py: m!c (x i >) a=st.pop(); env.w_mem[a % len(env.w_mem)]=st.pop() ;
 D#"Store x in memory by circular index i."#


#" Questnable word much asuming mem being a list like in pyWhorth "#
py: m0> (> x) st.append(env.w_mem[-1]) ;
 D#"Copy value from end of memory. Depricated?"#
"""

whrp_str = r"""
#" String words "#
imp' interp      imp' mem

: word' (> s) word w@ ; D#"Scan next word from input and return as str."#
: >word' (> s) lit> lit> >here word' >here ; IM
D#"Scan next word from input and compile it as a literal string."#

<<<#"
py> '""' (> s) _str IM
D##'String literal. Compile literal if compiling else return the str. Need
as usal space round word like: "" this is a str ""'##
py: ' (> s) _str(env, st, "'") ; IM
D##"String literal. Compile literal if compiling else return the str.
Need as usal space round word like: ' this is a str '"##

py> /"" (> s) _str                D#'Non imidiate string literal. se ""'#
py: /' (> s) _str(env, st, "'") ; D#"Non imidiate string literal. se '"#
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
imp' str      imp' interp

: meta' (>) look' meta@ ;   D#"Get meta info for following word."#

py: m_w@ (m > s) st.append(st.pop().w) ;
 D#"Get word name from meta info."#
py: m_idx@ (m > idx) st.append(st.pop().idx) ;
 D#"Get word index from meta info."#
py: m_sig@ (m > s) st.append(st.pop().sig) ;
 D#"Get word signature from meta info."#
py: m_doc@ (m > s) st.append(st.pop().doc) ;
 D#"Get meta infos doc string."#
py: m_doc! (s m >) d = st.pop(); st.pop().doc = d ;
 D#"Store string as meta infos doc string."#
py: m_flag@ (m > flag) st.append(st.pop().flag) ;
 D#"Get flags from meta info."#
: m_IM@ (m > IMflag) m_flag@ 1 and ;
 D#"Get flags from meta info and select the imidiate bit."#
py: m_wsrc@ (m > s) st.append(st.pop().wsrc) ;
 D#"Get Whorth source from meta info (if defined with :)."#
py: m_pybdy@ (m > s) st.append(st.pop().pybdy) ;
 D#"Get python body from meta info (if defined with py:)."#
py: m_pyfn@ (m > s) st.append(st.pop().pyfn) ;
 D#"Get the python function from meta info."#
py: m_pyname@ (m > s) st.append(getattr(st.pop().pyfn, '__name__', '')) ;
 D#"Get python funcname from meta info (if defined with py>)."#
"""

whrp_help = r"""
#" Help "#
imp' meta      imp' str      imp' if

: help (>) <<"
Whorth is a ugly duckling of Forth. It use ducktyping and don't adher to
standard Forth in a fair bit of ways. It use postfix notation like Forth
so You write (spaces are important):

            '1 + 2 * 3'  as:  '2 3 * 1 +'
    '(1.0 - 3.4) * 5.6'  as:  '1.0 3.4 - 5.6 *'

Values is taken and stored on a stack. That is also how Valus goes in and
out of words (think functions). The top of the stack is shown in the
prompt and You can nondestructively print the whole stack with the word
's.' or destructivly print the top value with '.' (dot).

Experement a litle and then press enter on an empty line to continue...">>
"help2" look func@ mty! . ;      D#"Print generall help info."#

: help2 (>) <<"
Spaces are important in Forth, they seperate words and need to go between
everything. That make the parser dead simple and easy to extend. Whorth add
wordbreaking on the inside of "" strings and paranteses and check that they
are ballanced. That make more 'normal' literals possible and it is made
in a way that is at least equally easy to extend. Look at 'lib.str',
'lib.listlit' and 'lib.if' for some examples howe parsing work.

Speaking of if... Whorth if is different from Forth in that it uses curlys:

   flag if{do this if true}else{do this}then continue here in both cases

If can only be used in compiled words see ':' further down. Whorths if also
have a lot of speacial features - se help' if or help"if".

Welcome back to the prompt and use an empty line to continue...">>
"help3" look func@ mty! . ;

: help3 (>) <<"
The words 'dup drop nip swap over rot' are the main means to manipulate the
stack. push and pop (may be renamed) also move values to/from the sidestack
witch make jugling values esier (standin for Forth return stack).

The word "lsw" list all loaded words, "help' word" print help for a word
(including it's source). You can import words with "imp' path.to.pack" To
look around You kan use "lsi' path" Standard words are in lib so "lsi' lib"
show the packages (already loaded packages is marked with *). "imp' lib.time"
as an example import time words. "lsi' lib.time" print the source for it.

Welcome back to the prompt and use an empty line to continue...">>
"help4" look func@ mty! . ;

: help4 (>) <<"
The word ':' is the compiler used to define new words - try "help' :" to
learn more. "py:" and "py>" is for defining new words in python - check help
there to! "help'" also show the source for the words so You can figure it
out. Here is an simplified example of a Forth word (.") defined in Whorth:

            : .' (>) word w@ . ;

":" is the compiler, ".'" is the word being defined, "(>)" is the stack
signature, "word" fetch the next word from input to wordbuffer (the string),
"w@" (word fetch) fetch the wordbuffer to the stack, "." print it and ";"
end the compile and store the new word to the dictonary. How do .' become ."?

Welcome back to the prompt and use an empty line to continue...">>
"help5" look func@ mty! . ;

: help5 (>) <<"
Ex:   ."Hello World"   .' for_one_word   .<|"fancy"|>

In Forth You would need a space after '."' but Whorth don't need that
(making hello world one char shorter). Whorth turn "text" into string, and
if thers a prefix (like ."str") the prefix is called with ' added. That's
how we used .' to define ." above. The last fancy exampel uses decorators,
any string of "<>|*#" reversed on ether side of the string. That make it possible to quote anything. If You use at least one # the string will turn
into a comment and disapere from the word stream unless it have a prefix.
The prefix is then called with "#'" added. The prefix "D#'" is defined and
add documentation to the latest word. Use it like below and the
documentation will turn up when using help'.

   : .' (>)  word w@ . ; D#"Print a string but no compiling."#

Yes, Forths ." also compiles so its usable in words. "hello" . works when
compiling Whorth words so ." is not so needed and don't exist yet.

Welcome back to the prompt and use an empty line to continue...">>
"help6" look func@ mty! . ;

: help6 (>) <<"
Whorth is far from complete and a moving target. It use python as its
assambler but the long plan is to also suport wasm, js and possible more.
Error messages are horroble and more for debugging Whorth then Whorth code
(and not even good for that). It is a terapy project recovering from long
ilness and progress will be slow and unsteaddy. To get a python stacktrace
of latest error use:

     pystacktrace .

This is it for this help. You go back to the prompt and empty lines will
do nothing. But You can call 'help' again. More help like this and with
more functions are planed. I do belevie 'at the prompt' is the right
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

: help' look' helpidx ;    D#"Print help for following word."#
"""
# is
whrp_stack = r"""
#" Stacks and acces "#

imp' pycomp

py: st@ (> l) st.append(st) ;       D#"Fetch stack as list."#
py: sst@ (> l) st.append(env.sst) ; D#"Fetch the side stack as list."#

py: s@ (n > x) n=st.pop(); st.append(st[-n-1]) ;
D##"Fetch the n:ts element from stack, indexed from top that is 0 - negative
numbers index from the bottom. Index as if the argument don't exist."##
py: s! (x n >) a=st.pop(); v=st.pop(); st[-a-1]=v ;
D##"Store x to the n:ts element of the stack, indexed from top that is 0
- negative numbers index from the bottom. Index as if the arguments don't
exist."##
<###"
py: s>rem (n > x) a=st.pop(); st.append(st.pop(-a-1)) ;
py: ins>s (x n >) a=st.pop(); v=st.pop(); st.insert(-a-1,v) ;
"###>
py: slen (> i) st.append(len(st)) ;       D#"Length of stack."#
py: sslen (> i) st.append(len(env.sst)) ; D#"Length of side stack."#

py: >sst (x >) env.sst.append(st.pop()) ; D#"Push value to side stack."#
py: sst> (> x) st.append(env.sst.pop()) ;  D#"Pop value from the side stack."#
py: >0sst (x >) env.sst[-1] = st.pop() ;
py: >1sst (x >) env.sst[-2] = st.pop() ;
py: >2sst (x >) env.sst[-3] = st.pop() ;
py: >!sst (x i >) i=st.pop(); assert(i>=0); env.sst[-i-1] = st.pop() ;
D#" Push value to side stack leaving a copy on the stack."#
py: sst0> (> x) st.append(env.sst[-1]) ;
py: sst1> (> x) st.append(env.sst[-2]) ;
py: sst2> (> x) st.append(env.sst[-3]) ;
py: sst@> (i > x) i=st.pop(); assert(i>=0); st.append(env.sst[-i-1]) ;
D#" Pop value from the side stack leaving a copy on the side stack."#

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
imp' comp      imp' var      imp' mem

py: List (> l) st.append([]) ; D#"Create an empty list"#
py: s/l (... n > l) n=st.pop(); l=st[-n:]; st[-n:]=(); st.append(l) ;
D#"Slash off n values from the stack and readd them as a list."#
py: l/l (l n > l l) n=st.pop(); l=st[-1][-n:]; st[-1][-n:]=(); st.append(l) ;
D#"Slash off n values from list as new list (split list, mod orig list)."#
: list' const' List >here ;

#" New notation, exprimentiall: These are stack ops"#

##" Storing values to top (end) of list as a stack."##
py: >l   (x l >)   l=st.pop(); l.append(st.pop()) ;
D#"push x to top (end) of list as a stack. Keep list."#
py: >0l  (x l >)   l=st.pop(); l[-1]=st.pop() ;
D#"store x in top (end) of list as a stack. Keep list."#
py: >1l  (x l >)   l=st.pop(); l[-2]=st.pop() ;
D#"store x to 1:st under top (end) of list as a stack. Keep list."#
py: >2l  (x l >)   l=st.pop(); l[-3]=st.pop() ;
D#"store x to 2:nd under top (end) of list as a stack. Keep list."#
py: >!l  (x l n >) n=st.pop(); assert(n>=0); l=st.pop(); l[-n-1]=st.pop() ;
D#"store x to n:th under top (end) of list as a stack. Keep list."#

#" These are list/mem only. index from start of list "#
py: l!   (x l i >)  i=st.pop(); assert(i>=0); l=st.pop(); l[i]=st.pop() ;
D#"store x to 0:rot index i in list. Keep list on stack."#
py: l!c  (x l i >)  i=st.pop(); l=st.pop(); l[i%len(l)]=st.pop() ;
D#"store x to 0:rot circular index i in list. Keep list on stack."#

##" Extracting values from top (end) of list as a stack, As the source, the
list is 'consumed'. "##
py: l>   (l > x)    x=st[-1].pop(); st[-1] = x ;
D#"Pop x off top (end) of list as a stack. Drop list."#
py: l0>  (l > x)    x=st[-1][-1]; st[-1] = x ;
D#"Fetch x from top (end) of list as a stack. Drop list."#
py: l1>  (l > x)    x=st[-1][-2]; st[-1] = x ;
D#"Fetch x from 1:st under top (end) of list as a stack. Drop list."#
py: l2>  (l > x)    x=st[-1][-3]; st[-1] = x ;
D#"Fetch x from 2:nd under top (end) of list as a stack. Drop list."#
py: l@>  (l n > x)  i=st.pop(); assert(i>=0); x=st[-1][-i-1]; st[-1] = x ;
D#"Fetch x from n:th under top (end) of list as a stack. Drop list."#

#" These are list/mem only. index from start of list "#
py: l@   (l i > x)  i=st.pop(); assert(i>=0); st[-1]=st[-1][i] ;
D#"Fetch x from 0:rot index i in list. Drop list."#
py: l@c  (l i > x)  i=st.pop(); st[-1] = st[-1][i%len(st[-1])] ;
D#"Fetch x from 0:rot circular index i in list. Drop list."#

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

: "[" slen >sst ; D#"Start List literal."#
: "]" slen sst> - dup 0 > if{s/l}el{drop List}then ; D#"End List literal."#
<###"
: py: <rem (l i > l x)   st[-1] = st[-2].pop(-st[-1]-1) ;
: py: <ins (l x i > l)   st[-3].insert(-st.pop()-1, st.pop()) ;
: py: @len <len (l > i) st.append(len(st.pop())) ;
"###>
"""

whrp_dict = r"""
#" Dictionaries "#
imp' comp

py: Dict (> l) st.append({}) ;
: dic' const' Dict >here ;

py: dic@ (d s > e) e = st[-2][st.pop()]; st[-1] = e ;
: dic@' (d > e) ' dic@ ;
py: dic! (d e k > d) k=st.pop(); e=st.pop(); st[-1][k]=e ;
: dic!' (d e > d) ' dic! ;
py: keys@ (d > l) st[-1] = list(st[-1].keys()) ;
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

py: scips (s >) env.scip(st.pop()) ;
py: scans (s >) env.scan(st.pop()) ;
py: find (s > i) st[0]=env.find(st[0]) ;

#"sfunc (s > i) slook func@"#
: func' (> i) look' func@ ;
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

: jmp! (frmadr toadr >) over - swap m! ;
D##" Calculate the relative addr and store it in frmadr so a jmp
instruction directly before frmadr will jump to toadr."##
: jmphere (frmadr >) here jmp! ;
D#"Store rel adr to here in frmadr (assuming a jmp instr befor that)."#
: jmpthere (toadr >) here swap  0 >here  jmp! ;
D#"Store rel adr to toadr in here (assuming a jmp instr befor that)."#

py: ?CALL (i >) env.r[-1] += not st.pop() ;
D#"Conditional call - Call next word if i is true, else step over it."#
py: !?CALL (i >) env.r[-1] += bool(st.pop()) ;
D#"Conditional call - Call next word if i is false, else step over it."#

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
: ?RECUR (b >) lit> ?CALL >here comp' RECUR ; IM
D#"Conditional recursion, Call the word being defined if b is true."#
: !?RECUR (b >) lit> !?CALL >here comp' RECUR ; IM
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
#" ? if{ ? }if{ - }br{ -- }el{ ? }if{ -- }el{ - }fin{ - }then - statement"#
imp' stack      imp' recur      imp' list

: _ljmp! (l > l)   dup len !?RET   dup l> jmphere  TRECUR ;
D#"Consume list of addr and store relative addr to here in them."#

: ljmp! (l >)   _ljmp!   drop ; D#"Store addrs with _ljmp! and drop list."#

: here>l (l >)   here swap >l ; D#"Push here to list and drop it"#

: "if{" (b >) lit> !?jmp >here   List >sst   List >sst
              here sst0> >l   1 >here ; IM
D##"Words impl 'if{ - }el{ - }then' statment. 'if{' and '}el{' (else) can be
followed by '}if{' (and if) creating shortcuted test without nesting 'if{'.
An '}if{' can be folowed by a '}br{' (break) turning it in to an abort with
clenup before joining '}el{'. An '}if{' following an '}el{' is efectivly an
elif. }fin{ (finalize) collect all previus 'succesfull' tests for a common
exit code. The whole if statment is ended with '}then'.

Implemented by storing state in two list on sst, the top one jump locations
for }el{ and the second one jump locations for '}fin{'. '}br{' also use the
top one. '}then' sets all remaining jump locations."##

: "}el{" (>) lit> jmp >here   sst1> here>l   1 >here   sst0> ljmp! ; IM
D#"Collect controll flow from faild 'if{ / }if{' and '}br{' (see 'if{')."#

: "}then" (>) sst> ljmp!    sst> ljmp! ; IM
D#"Collect all remaining controllflow and end if statment (drop state)."#

: "}if{" (b >) lit> !?jmp >here   sst0> here>l   1 >here ; IM
D##"Extra conditions on each 'if{' / '}el{' leg. Each 'if{' / '}el{' can
have several '}if{' with or whithout '}br{'."##

: "}br{" (>) lit> jmp >here  sst0>  dup l> jmphere  here>l  1 >here ; IM
D#"Turn an '}if{' into 'abort if' (see 'if{')."#

: "}fin{" (>) lit> jmp >here   sst0> here>l   1 >here   sst1> ljmp! ; IM
D##"'finalize': collect controll flow from succeding IF / ELSE AIF for
common exit code. "##
"""
whrp_IF = r"""
##" Forth style IF -- ELSE -- THEN - statement and whorth simpl iter"##
imp' stack      imp' jmp

: IF (b >) lit> !?jmp >here  here >sst  1 >here ; IM
D#"Words impl forth style IF - ELSE - THEN statment."#

: ELSE (>) lit> jmp >here   here  1 >here  sst> jmphere  >sst ; IM
D#"Words impl forth style IF - ELSE - THEN statment."#

: THEN (>) sst> jmphere ; IM
D#"Words impl forth style IF - ELSE - THEN statment."#

#"Odd naming below to not interfer white atempt to do std forth stuff."#

: ITER (>) 0 >sst   0 >sst   here >sst ; IM
D##"ITER .. [flag WHL] .. [flag WHL] .. RPT .. [[ELSE] .. THEN] statment.

ITER (iterate) .. RPT (repeat) is an endless loop. Optional WHL (while)
break the loop if flag is false. Optional second WHL do the same but
jump to ELSE/THEN insted of to RPT. That way You can have different
exit code depending on wich while breaking. If You have two WHL You
MUST have a THEN and may have a ELSE. If You dont have two WHL You
MUST NOT have ELSE or THEN!"##

: WHL (flag >) lit> !?jmp >here
               here  sst1> IF  >2sst  ELSE  >1sst  THEN  1 >here ; IM
D##" Break the loop if flag false. You may have two WHL and then, and only
then, You MUST have a THEN and may have an ELSE. First WHL break to RPT and
second to ELSE or THEN."##

: RPT (>) lit> jmp >here   sst> jmpthere
          sst> dup  IF  jmphere  ELSE  drop  THEN
          sst> dup  IF  >sst  ELSE  drop  THEN ; IM
D##"Loop by jumping back to ITER. End an ITER statment unles it have two
WHL in witch case, and only in that case, You need an THEN."##
"""
# : qq ITER dup 10 < WHL dup 1+ dup 9 < WHL dup 3 + RPT 42 ELSE 666 THEN 9 ;

def whrp_rand(env):
	"""Add random functions to a WHORTH env. example of using pydef."""
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
	"""Add math functions to a WHORTH env. example of using pydef."""
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
	# posetiv value is truthines, negative falsiness and 0 is unknown.
	D('fAND', 'x x>x', 'a=st.pop()\nst[-1]=min(st[-1],a)')
	D('fOR',  'x x>x', 'a=st.pop()\nst[-1]=max(st[-1],a)')
	D('fXOR', 'x x>x', 'a=st.pop()\nst[-1]=min(max(st[-1],a),-min(st[-1],a))')
	D('fNAND','x x>x', 'a=st.pop()\nst[-1]=-min(st[-1],a)')
	D('fNOR', 'x x>x', 'a=st.pop()\nst[-1]=-max(st[-1],a)')
	D('fSAME','x x>x', 'a=st.pop()\nst[-1]=-min(max(st[-1],a),-min(st[-1],a))')
	D('fNOT', 'x > x', 'st[-1]=-st[-1]')

	# Treshold words to get truth flag out of a fuzzy logic value.
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
	# Word Zero... Raise exeption, it should not be called
	D('WordZero', '>', w_zero)
	D('test', '>', _test)

	# number to from str id. not actually used for something but usefull.
	D('nstr', 'i > s', 'st[-1] = nstr(st[-1])')
	D('strn', 's > i', 'st[-1] = strn(st[-1])')

def whrp_stack_extra(env):
	"""Add stack and memory ops to WHORTH env. Names will change!"""
	D = env.pydef
	D('mnpush', '* i >',
		'n=st.pop()\nenv.w_mem.extend(st[min(-1,-n):]);st[min(-1,-n):]=()')
	D('mnpop',  'i > *',
		'n=st.pop()\nst.extend(env.w_mem[min(-1,-n):]);env.w_mem[min(-1,-n):]=()')
	D('mnpushcp', '* i >',
		'n=st.pop()\nenv.w_mem.extend(st[min(-1,-n):])')
	D('mnpopcp',  'i > *',
		'n=st.pop()\nst.extend(env.w_mem[min(-1,-n):])')
	D('npush', '* i >',
		'n=st.pop()\nenv.sst.extend(st[min(-1,-n):]);st[min(-1,-n):]=()')
	D('npop',  'i > *',
		'n=st.pop()\nst.extend(env.sst[min(-1,-n):]);env.sst[min(-1,-n):]=()')
	D('npushcp', '* i >',
		'n=st.pop()\nenv.sst.extend(st[min(-1,-n):])')
	D('npopcp',  'i > *',
		'n=st.pop()\nst.extend(env.sst[min(-1,-n):])')


#def _literal(env, st):
#	env.x_p = env.x_p + 1
#	st.append(env.w_mem[env.x_p])
def _compw(env, w):
	env.w_mem.append(env.dicl[env.dic[w]])

def _wstartcomp(env, flag=0):
	txt = env.ip - len(env.w)
	#print('_wstartcomp1', env.w)
	#print(env.mk_err())
	env.word()
	#print('_wstartcomp2', env.w)
	#print(env.mk_err())
	env.c_w = env.w
	env.c_flag = flag # | __IW
	env.c_txt = env.ib[txt:env.ip]
	env.scip()
	#print('scip\n{}'.format(env.mk_err()))
	if env.ib[env.ip] != '(':
		
		env.c_sig = ''
	else:
		env.word()
		sp = env.ip
		#env.pst[-1].ctx = 'SIG'
		while (env.w != ')'): #or (env.pst[-1].ctx != 'SIG'):
			env.word()
		#print('while word\n{}'.format(env.mk_err()))
		env.c_sig = env.ib[sp:env.ip-1].strip()
		#print('__wstartcomp', txt, len(env.w), env.w)
		#print('ib:', env.ib[txt:env.ip])
		#sp = env.ip
		#env.ip = env.find(')')
		#if env.ip > sp:
		#	env.c_sig = env.ib[sp:env.ip-1].strip()
		#	#print('__wcompile sig:', c_sig)
		#	env.ip = env.ip + 1
		#else:
		#	env.err.append("compile error - no end to signature.\n" +
		#		env.mk_err())
		#	raise SyntaxError("compile error - no end to signature.")
	env.c_txt = env.ib[txt:env.ip]
	#print('_wstartcomp3', env.c_txt)
	#if env.w:
	#	env.c_w = env.w
	#	env.c_flag = flag # | __IW
	#	env.c_txt = env.ib[txt:env.ip]
	#	#print('c_txt:', env.c_txt)
	#	env.w = ''
	#	return
	#else:
	#	env.err.append("compile error - no word name given.\n" +
	#		env.mk_err())
	#	raise SyntaxError("compile error - no word name given.")

def _pycolon(env, st):
	"""Compile a python word. It ends with ';'"""
	env.po = 'py: '
	_wstartcomp(env)
	env.scip()
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
			#env.c_txt = ''
			#env.c_flag = 0
			return
		etxt = len(env.c_txt)
		env.word()
	env.po = ''
	env.err.append("Input end pycompiling word {}\n{}".format(c_w,
		env.mk_err()))
	raise SyntaxError("Input end pycompiling word {}".format(c_w))

def _scolon(env, st):
	"""End compiling word."""
	c_w, env.po, env.c_w = env.c_w, '', ''
	if c_w:
		env.w_mem.append(None);
		env._addic(c_w, env.c_mp)
		env.dicd[-1] = whr.Wmeta(
			c_w, env.c_mp, flag=env.c_flag, sig=env.c_sig, wsrc=env.c_txt)
		#print(';', repr(c_w), repr(env.c_w))

def _colon(env, st):
	"""Compile a Whorth word. Ends with ';'"""
	#env.po = ': ';
	_wstartcomp(env);
	env.c_mp = len(env.w_mem);
	#compl(env, st);
	#env.po = ''
	#if env.c_w:
	#	env.err.append("Input end compiling word {}\n{}".format(c_w,
	#		env.mk_err()))
	#	raise SyntaxError("Input end compiling word {}".format(c_w)) ;

def _py_gt(env, st):
	"""Tie a word to a python function."""
	env.po = 'py> ';
	_wstartcomp(env);
	env.word()
	#print('py>', env.c_w, env.w)
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
	#env.c_txt = ''; env.c_flag = 0; env.po = ''""") 'finalize'

def _py_doc(env, st):
	"""Copy doc from a pyton func __doc__ to latest word."""
	env.po = 'pyd> ';
	#_wstartcomp(env);
	env.word()
	try:
		env.addoc(eval(env.w))
	except:
		env.err.append("Error addoc {}\n{}".format(env.w,
			env.mk_err()))
		raise
	#env.dicd[-1].doc = eval('getattr({}, "__doc__", "")'.format(env.w), env.glob)
	env.po = ''

def _wo_doc(env, st):
	"""Copy doc from a whorth comment to latest word."""
	env.word()
	#print("_wo_doc", env.w)
	if env.w:
		d = env.dicd[-1].doc
		env.dicd[-1].doc = d + '\n\n' + env.w if d else env.w
			

def _com(env, st=None, doc=None):
	"""Scan a whorth comment. Ends with ';'"""
	po = env.po
	env.po = '#: '
	c_w = env.c_w; env.c_w = 'str'
	if not c_w:
		env.c_txt = ''
	sp = len(env.c_txt) + 1
	env.word()
	while env.w or (env.pst and (env.pst[-1].es[0] in '\'"')):
		if env.w == ';':
			if doc:
				env.addoc(env.c_txt[sp:-2], doc)
			env.c_w = c_w
			env.po = po
			return
		env.word()
	#print(env.pst)
	env.err.append("Input end while scanning comment\n{}".format(env.mk_err()))
	env.c_w = c_w
	env.po = po
	#print(env.ip, len(env.ib), '\n', env.ib)
	raise SyntaxError("Input end while scanning comment")

def _str(env, st, end='""', comp=True):
	"""A whorth string of sort."""
	po = env.po
	env.po = end + ': '
	c_w = env.c_w; env.c_w = 'str'
	if not c_w:
		env.c_txt = ''
	sp = len(env.c_txt) + 1
	env.word()
	while env.w:
		if env.w == end:
			st.append(env.c_txt[sp:-len(end)-1])
			if c_w and comp:
				env.w_mem.append(env.dicl[env.dic['lit>']])
				env.w_mem.append(st.pop())
			env.c_w = c_w
			env.po = po
			return
		env.word()
	env.err.append("Input end while scanning string\n{}".format(env.mk_err()))
	env.c_w = c_w
	env.po = po
	raise SyntaxError("Input end while scanning string")

def _kill_compl(env, st):
	"""Compiling inner interpreter."""
	mem = env.w_mem
	dic = env.dic
	dl = env.dicl
	dd = env.dicd
	lit = dl[dic['lit>']]
	#call = dic['_call_x_p']
	env.word()
	#print(env.w)
	while env.w: # or (env.pst and (env.pst[-1].es[0] == '"')):
		n = dic.get(env.w, None)
		if n is not None:
			#print('compile word:', env.w, n, dw)
			func = dl[n]
			if dd[n].flag & whr.Wmeta.IM:
				env.call_fnc(func)
				if not env.c_w:
					break
			else:
				mem.append(func)
		elif env.literal(env.w):
			mem.append(lit)
			mem.append(st.pop())
		else:
			env.e("compl: Not word or literal: {}".format(repr(env.w)))
			env.e(env.mk_err())
			raise SyntaxError('compl: Not word or literal')
		env.word()

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


