# WHORTH - the ugly duckling of forth
# Open sorceish - licence still to be chosen
# Terapy project so far only usefull for play - but feel free to play!
# (c) Lars Hallberg, WIP-FL lah2180@gmail.com
# WIP-FL: Whorth institutionalized patient for life

"""WHORTH: The ugly duckling of FORTH.

This is first incomplete draft of WHORTH implementing some FORTH like
stuff on top of Python. Uses Pythons Duck typing and plan to keep the
neatness of that in part despise loos plans of a WASM implementation.
Thus dose not even attempt to be standard FORTH - WHORTH will remain
an ugly duckling!

This is mainly a therapy project for cognitive training recovering from
illness. No guaranties of continuous work can be given at the moment (I'm
also in desperate need to take any opportunity to make some money whenever something I can do turn up). Focusing is part of the problem and not even
reply to queries can be promised. WHORTH is chosen tho for a slim
potential to actually be useful eventually - At least as a friendly
playground. Have fun!
"""

__whorth_name__ = 'WHORTH'

#
#  Constant state of mess!
#
#  Be warned.
#

import readline
import atexit
import inspect
import weakref
from traceback import format_exc
import types
import time
import math as mt
import whorthlib as whrd_lib
import slots_base as slt

# Wpst flags
STR=1; PAR=2; C_W=4; IGCOM=8;

class Wpst(slt.SlotsBase):
	"""Whorth parse stack (pst) item."""
	__slots__ = {
		'tok':   'Token - What kind of struct (comment, if, str, loop etc)',
		'sb':    'Start buffer',
		'sp':    'Start pointer',
		'eb':    'End buffer',
		'ep':    'End pointer',
		'es':    'End string',
		'efnc':  'End function',
		'flag':  'Flags',
		'lst':   'Whorth extension data list for compiling words',
		# flag 'c_w':   'Owns compiling word',
		'c_sp':  'Compile text start pointer (c_txt)',
		'ptxt':  'Post text',
		# flag 'igcom':  'Ignore comment',
		}


	def __init__(self, tok=None, es='', efnc=None, lst=None, flag=IGCOM):
		self.tok = tok;         self.es = es
		self.efnc = efnc;       self.flag = flag
		self.sb = None;         self.sp = None
		self.eb = None;         self.ep = None
		self.lst = lst;        self.c_sp = None
		self.ptxt = None

	def starts(self, s, tbl={ord('>'): '<', ord('<'): '>'}):
		self.es = ''.join(c for c in reversed(s.translate(tbl)))

	def cf(self, ch):
		return self.es and (ch == self.es[0])

	def cin(self, ch):
		return self.es and (ch in self.es)

class Wmeta(slt.SlotsBase):
	"""Metadata for a whorth word."""
	__slots__ = {
		'w':       'whorth word name.',
		'idx':     'Dictionary index number.',
		'sig':     'stack signature.',
		'flag':    'Word flags. Can be used with & | &= |= from wm.',
		'doc':     'Documentation string.',
		'wsrc':    'Whorth source code.',
		'pyfn':    'Python function.',
		'pybdy':   'Python body source code.',
		}

	# Flags
	IM = 1
	"""Immediate word - runs during compile."""

	def __init__(self, w, idx,
			sig='', flag=0, doc=None, wsrc=None, pyfn=None, pybdy=None):
		"""w: whorth word name and idx: index# of word must be given."""
		self.w = w;         self.idx = idx;       self.sig = sig
		self.flag = flag;   self.doc = doc;       self.wsrc = wsrc
		self.pyfn = pyfn;   self.pybdy = pybdy

	def shstr(self):
		"""Return short string presentation."""
		return '{' + self.w + '}'

	#def __repr__(self):
	#	"""Same as __str__, use asdict for serialization."""
	#	return self.__str__() # repr(self.asdict())

# some utility functions.

def to_pyid(name, pyid = '_',
		t={':': 'Kn', '.': 'Dt', ',': 'Km', ';': 'Sk', '+': 'Ad', '~': 'Td',
		'-': 'Mn', '*': 'As', '/': 'Sl', '\\': 'Bs', '%': 'Ps', '&': 'Ap',
		'=': 'Eq', '<': 'Lt', '>': 'Gt', '!': 'Ex', '"': 'Dt', "'": 'Et',
		'@': 'Ca', '#': 'Hc', '$': 'Dl', '?': 'Qe', '|': 'Pi', '^': 'Rf',
		'(': 'Pl', ')': 'Pr', '[': 'Bl', ']': 'Br', '{': 'Cl', '}': 'Cr'}):
	for c in name:
		nid = pyid + c
		if nid.isidentifier():
			pyid = nid
		else:
			pyid = pyid + t.get(c, '_')
	return pyid

def whrss(val):
	"""Convert Whorth special values to str else pass thru."""
	if val is None:
		return 'Nil'
	if isinstance(val, bool):
		return 'FT'[val]
	s = str(val)
	if s.startswith('<func'):
		return '(' + s.split()[1] + ')'
	return val

def whr_s(n):
	"""Value as whorth string."""
	return str(whrss(n))

def whr_rs(n):
	"""Value as whorth representation."""
	v = whrss(n)
	return repr(v) if v is n else v

def splitsig(sig):
	"""Split stack signatures returning tuple of tupels for in and out."""
	l = sig.split('>')
	if len(l) == 2:
		return tuple(s.strip().split() for s in l)
	if (len(l) == 1) and not l[0].strip():
		return ((), ())
	print('Error sig:', repr(sig))
	return None

def nsig(spl_sig):
	"""Stack sig len as tuple in & out from split signature."""
	if spl_sig is None:
		return (0, 0)
	else:
		return (len(spl_sig[0]), len(spl_sig[1]))

def shstr(s, mx=9):
	"""Short string, truncate str to max mx chars."""
	mx = max(5, mx)
	if len(s) > mx:
		return s[:mx-3] + '…' + s[-2:] # …
	else:
		return s

def shfloat(n, ml=7):
	"""Short float, convert float to string max ml chars unless n large."""
	try:
		dec = max(ml, 5) - max(0, int(mt.log10(abs(n)))) - 2 - (n < 0)
		if dec > 0:
			return '{:3.{}f}'.format(n, max(dec, 0))
		return '{:3.0f}.'.format(n)
	except ValueError:
		return '0.0'

def shdots(n, ml=7):
	"""Shortdots, convert value to shortish string (aiming for ml chars)."""
	if isinstance(n, float):
		return shfloat(n, ml)
	if isinstance(n, str):
		return shstr(n, ml)
	if hasattr(type(n), 'shstr'):
		return n.shstr()
	return whr_s(n)

def shdotr(n, ml=8):
	"""Shortdotr, convert value to shortish repr (aiming for ml chars)."""
	if isinstance(n, float):
		return shfloat(n, ml)
	if isinstance(n, str):
		return repr(shstr(n, ml))
	if hasattr(type(n), 'shstr'):
		return n.shstr()
	return whr_rs(n)

class Whorth(slt.SlotsBase):
	"""Whorth environment that can run worth code."""
	__slots__ = {
		'glob': """Globals.""",
		'ib': """Input buffer.""",
		'ip': """Input pointer - index into ib.""",
		#'x_p': """Execution pointer (index in to program memory).""",
		'w_mem': """Program memory as a list.""",
		'st': """Main value stack as a list.""",
		'r': """Return stack as list.""",
		'sst': """Side value stack as a list.""",
		'pst': """Parse stack""",
		'cst': """Compile stack""",
		'w': """Word buffer - latest word read from ib.""",
		'w_in': """'pointer' to word to refill input buffer.""",
		'po': """Prompt override.""",
		'pn': """Prompt depth - how much of stack to show in prompt.""",
		'pnn': """Prompt width - width of each stack element in prompt.""",
		'dic': """Word lookup dictionary.""",
		'err': """List of error messages.""",
		'dicl': """List of word implementations.""",
		'dicd': """List of word metadata and stuff.""",
		'c_wm': """Wmeta for word currently being compiled.
			Not None indicate compilation.""",
		'c_sig': """Signature of word being defined.""",
		'c_flag': """Flags of word being compiled.""",
		'c_w': """Word being defined or '' indicating interpreter mode.""",
		'c_mp': """Start memory position of word being defined.""",
		#c_p = 0 compilation pointer - implicit as end of program memmory.
		'c_txt': """Full text of word being defined.""",
		#'exdic': """Exported word lookup dictionary.""",
		'xto': """Exports to this whorth environment.""",
		'impd': """Dictionary with packages imported""",
		'i_mty': """Call on empty input line if w_in also true""",
		'err_pyst': """Latest python stack trace."""
		}

	_cach = weakref.WeakValueDictionary()
	"""Cach of compiled python functions."""
	gxto = { }
	"""Global exports to Whorth."""

	class PyEx(slt.SlotsBase):
		"""Function (word) exported to python."""
		__slots__ = {
			'env':     'Whorth environment to run in.',
			'idx':     'Index of word to run',
			'i':       'Number of in parameters.',
			'o':       'Number of out parameters',
			#'__doc__': 'Docstring' # WTF ... Later problem
			}

		def __init__(self, env, idx, i, o): #, doc):
			self.env=env; self.idx=idx; self.i=i; self.o=o #; self.doc=doc

		def __call__(self, *args):
			if len(args) != self.i:
				raise TypeError('Need {} arguments, {} given'.format(
					self.i, len(args)))
			env = self.env
			env.push(args)
			env.call_idx(self.idx)
			return env.pop(self.o)

		def doc(self):
			return self.env.doc(self.idx)

	def __init__(self, glob=None, pack=True):
		if glob is None:
			cf = inspect.currentframe()
			try:
				if cf is not None:
					cf = cf.f_back
					if cf is not None:
						glob = cf.f_globals
			finally:
				del cf
			if glob is None:
				raise ValueError("No globals given and can't be calculated.")
				#glob = globals()
		self.glob = glob;     self.ib = '';           self.ip = 0
		self.r = [ ];         self.w_mem = [None]*3;  self.st = [ ]
		self.sst = [ ];       self.w = '';            self.w_in = 0
		self.po = '';         self.pn = 4;            self.pnn = 8
		self.dic = { };       self.err = [ ];         self.dicl = [ ]
		self.dicd = [ ];      self.c_wm = None;       self.c_sig = ''
		self.c_flag = '';     self.c_w = '';          self.c_mp = 0
		self.c_txt = '';      self.xto = { };         self.impd = { }
		self.pst = [ ];       self.cst = [ ];           self.i_mty = None
		self.err_pyst = ''

		# Esential whorth stuff
		D = self.pydef
		d = self.addoc
		D('lit>', '> x', 'env.r[-1]+=1; st.append(env.w_mem[env.r[-1]])')
		d("Push val at exec pointer (xp) to the stack and advance xp.")
		D('.',  'x >',   'print(whr_s(st.pop()))')
		d('Print (and consume) top value on stack.')
		D('s.', '>', Whorth.sdot); d(Whorth.sdot)
		D('lsw', '>',     'env.lsw()')
		d('List all loaded words')
		D("lsi'", '>', 'env.po=None;env.word();print(env.lsis(env.w));env.po=""')
		d("""List import directory according to following word (the path).

The path is a . delimited list of modules. Just a dot list the root. Modules
are listed in {}, imports bare or surrounded by * if already imported. If the
path go all the way to an import the source of it is displayed.""")
		D("imp'", '>', 'env.po=None;env.word();env.imp(env.w);env.po=""')
		d("import the folowing word (the path). Se 'help> lsi>' for more.")
		D('inp',  '>', Whorth.inp)

		self.pyexport(glob=True, whorth=globals(), lib=whrd_lib)
		if pack:
			self.imp('lib.pycomp')
			self.imp('lib.basic')
			self.imp('lib.help')

	def setglob(self, glob):
		"""Set globals and return old value.

			If given None as argument do nothing and return None."""
		if glob is None:
			return None
		og = self.glob
		self.glob = glob
		return og

	def e(self, err):
		"""Add an error to the err list."""
		self.err.append(err)

	def push(self, *p):
		"""Push values on to stack."""
		if (len(p) == 1) and isinstance(p[0], (list, tuple)):
			self.st.extend(p[0])
		else:
			self.st.extend(p)

	def pop(self, n=1):
		"""Pop n values of stack, if n>1 return them in a tuple."""
		if n == 1:
			return self.st.pop()
		if n < 1:
			return None
		st = self.st
		r = tuple(st[-n:])
		st[-n:] = ()
		return r

	def setinbuf(self, s):
		"""Set input buffer to s and reset input pointer to 0 and w to ''."""
		self.ib, self.ip, self.w = s, 0, ''

	def pyimport(self, word=None):
		"""import a whorth word to a python callable that is returned.

			argument and return value is taken from words signature.
			word default to name of latest word defined."""
		if word is None:
			mt = self.dicd[-1]
			word = mt.w
			assert mt.idx == (len(self.dicd) - 1)
			n = mt.idx
		else:
			n = self.dic[word]
			mt = self.dicd[n]
		i, o = nsig(splitsig(mt.sig))
		return self.PyEx(self, n, i, o) #, doc)

	def pyexport(self, *dic, glob=False, **exp):
		"""Add exports that can be imported from Whorth."""
		# Attempt to mimic the situation inside of a WASM sandbox here.
		ex = Whorth.gxto if glob else self.xto
		dic = list(*dic) + [exp]
		for x in dic:
			for k, v in x.items():
				if isinstance(v, (dict, types.ModuleType)):
					if not k.startswith('whrd_'):
						k = 'whrd_' + k
				elif not k.startswith('whrp_'):
					continue
				ex[k] = v

	def ipt_nxt(self, key, fdic=True, dic=None):
		"""Resolve path"""
		if dic is None:
			dic = {}
			dic.update(Whorth.gxto)
			dic.update(self.xto)
			dic.update(self.glob)
		if isinstance(dic, types.ModuleType):
			dic = dic.__dict__
		if not isinstance(dic, dict):
			return None
		if key == '.':
			return dic
		if fdic:
			ret = None
		else:
			ret = dic.get('whrp_' + key)
		if ret is None:
			ret = dic.get('whrd_' + key)
		if ret is None:
			return None
		if isinstance(ret, types.ModuleType):
			ret = ret.__dict__
		if isinstance(ret, dict):
			return ret
		else:
			return (dic, ret, key)

	def ipt(self, path='whr'):
		"""Resolve path"""
		if path == '.':
			return self.ipt_nxt('.')
		ww = path.split('.')
		ret = None
		while ww:
			w = ww.pop(0)
			if w:
				ret = self.ipt_nxt(w, ww, ret)
				if ret is None:
					break
		return ret

	def imp(self, path):
		"""import path to this whorth env."""
		p = self.ipt(path)
		if p is None:
			print('ERROR: ignore import:', path)
			return
		if isinstance(p, dict):
			print('ERROR: cant import a dict:', path)
			return
		dic, pack, key = p
		glob = self.setglob(dic)
		try:
			g = self.glob
			wid = g.get('__whorthid__')
			if wid is None:
				wid = g.get('__name__', '') + str(time.monotonic_ns())
				g['__whorthid__'] = wid
			pwid = wid + ':' + key
			if pwid in self.impd:
				return
			stat = (len(self.w_mem), len(self.dic))
			if isinstance(pack, str):
				self.run(pack)
			else:
				pack(self)
			self.impd[pwid] = stat
		finally:
			self.setglob(glob)

	def lsis(self, path='.'):
		"""List imports available to whorth to a string."""
		p = self.ipt(path)
		if p is None:
			print('ERROR: bad path:', path)
			return
		if isinstance(p, dict):
			l = []
			for k, v in p.items():
				if isinstance(v, (dict, types.ModuleType)):
					if k.startswith('whrd_'):
						l.append('{' + k[5:] + '}')
				elif k.startswith('whrp_'):
					key = k[5:]
					wid = p.get('__whorthid__', '')
					if wid:
						if wid + ':' + key in self.impd:
							key = '*' + key + '*'
					l.append(key)
			wdoc = p.get('__doc__', 'no docstring.')
			wdoc = '\n\n'.join(wdoc.split('\n\n')[:2])
			wname = p.get('__name__','')
			if wname == '__main__': wname = 'WHORTH'
			whrname = p.get('__whorth_name__','')
			return 'Directory {}\n\n{}\n\n{}\n'.format(wname,
				wdoc, ' '.join(l))
		dic, pack, key = p
		wname = dic.get('__name__','')
		if wname == '__main__': wname = 'WHORTH'
		if isinstance(pack, str):
			return "\nWhorth paragraph:   {}   module: {}\n\n{}".format(key,
				wname, pack)
		return "\nPyfunc whorth paragraph:   {}   module: {}\n\n{}".format(key,
			wname, getattr(pack, '__doc__', 'no docstring.'))

	def call_fnc(self, fnc):
		"""Call a whorth word by fnc 'pointer'."""
		try:
			if callable(fnc):
				fnc(self, self.st)
			elif fnc is not None:
				self.binterp(fnc)
		except Exception as er:
			ec = str(er.__class__).split("'")[1]
			self.e("call_fnc({}): {}".format(str(fnc), ec))
			self.e(self.mk_err())
			raise er

	def call_idx(self, n):
		"""Call a whorth word by index."""
		try:
			fnc = self.dicl[n]
		except Exception as er:
			ec = str(er.__class__).split("'")[1]
			self.e("call_idx({}): {}".format(idx, ec))
			self.e(self.mk_err())
			raise er
		self.call_fnc(fnc) # self.dicl[n])

	def call(self, key):
		"""call a whorth word named by key."""
		try:
			idx = self.dic[key]
		except Exception as er:
			ec = str(er.__class__).split("'")[1]
			self.e("call({}): {}".format(repr(key), ec))
			self.e(self.mk_err())
			raise er
		self.call_idx(idx) # self.dicl[self.dic[key]])

	def _addic(self, key, func):
		self.dic[key] = len(self.dicl)
		self.dicl.append(func)
		self.dicd.append(None)

	def _pycomp(self, key, func): # ids, func):
		glob = self.glob
		cach = glob.get('__whorth_cach__')
		if cach is None:
			cach = weakref.WeakValueDictionary()
			glob['__whorth_cach__'] = cach
		else:
			fn = cach.get(func)
			if fn is not None:
				return fn
		fname = to_pyid(key, 'W')
		cs = 'def {}(env, st):\n'.format(fname)
		cs += '\n'.join('\t{}'.format(s) for s in (func.strip().split('\n')))
		ccs = compile(cs, key, 'exec')
		tdic = { }
		exec(ccs, glob, tdic)
		fn = tdic[fname]
		cach[func] = fn
		return fn

	def pydef(self, key, sig, func, flag=0):
		"""Define a word in python.

			key is word name, func is ether python body as string or a
			callable object."""
		idx = len(self.dicl)
		if callable(func):
			self._addic(key, func)
			self.dicd[-1] = Wmeta(key, idx, sig=sig, pyfn=func, flag=flag)
			return
		py_func = self._pycomp(key, func)
		self._addic(key, py_func)
		self.dicd[-1] = Wmeta(key, idx,
			sig=sig, flag=flag, pyfn=py_func, pybdy=func)

	def setIM(self, st=None):
		""" Mark latest defined word as immediate."""
		self.dicd[-1].flag |= Wmeta.IM

	def doc(env, key = -1):
		"""Return doc for word. key is ether word name, idx or Wmeta."""
		if isinstance(key, Wmeta):
			doc = key
		elif isinstance(key, int):
			doc = env.dicd[key]
		elif isinstance(key, str):
			doc = env.dicd[env.dic[key]]
		else:
			self.err.append('key not int, str or Wmeta: {} {}\n{}'.format(
				str(type(key)), str(key), self.mk_err()))
			raise(TypeError,
			  'key not int, str or Wmeta: {} {}'.format(str(type(key)), str(key)))
		return '{} ({})\n\n{}'.format(doc.w, doc.sig, doc.flag, doc.doc)

	def addoc(env, txt, doc=True):
		"""Add txt to doc for word.

			doc is word name, idx, Wmeta or True (default) for latest word.
			txt is a string or a python callable with a __doc__ str."""
		if callable(txt):
			txt = getattr(txt, '__doc__', '')
		if isinstance(doc, bool):
			doc = env.dicd[-1]
		if isinstance(doc, int):
			doc = env.dicd[doc]
		elif isinstance(doc, str):
			doc = env.dicd[env.dic[doc]]
		elif not isinstance(doc, Wmeta):
			doc = env.dicd[-1]
		if doc.doc is None:
			doc.doc = txt
		else:
			doc.doc += '\n\n' + txt

	def pststr(self):
		l = ['PST top first ip: {}  dept: {}'.format(self.ip, len(self.pst))]
		for p in reversed(self.pst):
			s = "{}:{} sp:{} es:{} {}".format(p.tok, p.flag, p.sp, p.es,
				'sb==ib' if self.ib==p.sb else '')
			if p.eb:
				s = "{} ENDED ep:{} {}".format(s, p.ep,
					'eb==ib' if self.ib==p.eb else '')
			l.append(s)
		return '\n'.join(l)

	def clrpstC_W(self):
		if self.pst:
			cur = self.pst[-1]
			if cur.flag & C_W:
				self.c_w = ''
				self.c_txt = ''
				cur.flag &= ~C_W

	def checkpst(self, ch, ip, l): #, skip=0): # , skip)
		# ret bitfield. 1: keep scan   2: keep skip .ctx .c_w
		ib = self.ib
		if self.pst: # Drop Zombie pst element.
			cur = self.pst[-1]
			while cur.eb and ((cur.eb != ib) or ((ip-l) > cur.ep)):
				if cur.flag & C_W:
					self.e('Reclaim of str holding C_W')
					self.e(self.mk_err(ip=ip))
					self.e(self.pststr())
					raise(SyntaxError('Reclaim of str holding C_W'))
				self.pst.pop()
				if self.pst:
					cur = self.pst[-1]
				else:
					break
		if self.pst:
			cur = self.pst[-1]
			if not cur.es:
				self.e('no es: {}\n{}\n{}'.format(ch,
					self.mk_err(ib=cur.sb, ip=cur.sp), self.mk_err(ip=ip)))
				self.e(self.pststr())
				raise(SyntaxError('{} no es: {}'.format(cur.tok, ch)))
			if cur.eb == None:
				if (cur.sb == ib) and (cur.sp >= ip):
					return 1 if ch else 2
				if cur.es[0] == '"': # '\'"'
					if cur.sb == ib:
						if (cur.sp+1) == ip:
							if cur.c_sp is None:
								if self.c_w:
									cur.c_sp = len(self.c_txt) + l
								else:
									self.c_w = 'str'
									cur.flag |= C_W
									self.c_txt = ''
									cur.c_sp = l
								if l > len(cur.es):
									return 0
					if cur.es == ib[ip:ip+len(cur.es)]:
						cur.eb = ib
						cur.ep = ip
					return 1 if ch else 2
				if (cur.es[0] in '}])') and (cur.sb == ib):
					if ((cur.sp+1) == ip) and l:
						return 0
				if ch and (ch in '}])'):
					if not (cur.es[0] == ch):
						self.e('PST Mismatch ch: {} vs {}\n{}\n{}'.format(
							ch, cur.es, self.mk_err(ib=cur.sb, ip=cur.sp),
							self.mk_err(ip=ip)))
						self.e(self.pststr())
						raise(SyntaxError('PST Mismatch ch: {} vs {}'.format(ch,
							cur.es)))
					cur.eb = ib
					cur.ep = ip
					if l:
						return 0
					return 1 if ch else 2
			else:
				if cur.es[0] == '"':
					if ip == cur.ep:
						return 1 if ch else 2
					if ch in " \n\t}])":
						if cur.ptxt is None:
							cur.ptxt = ib[cur.ep+len(cur.es):ip]
						return 0
					if ch == '"':
						self.e('Trying to train str: {}\n{}\n{}'.format(ch,
							self.mk_err(ib=cur.sb, ip=cur.sp), self.mk_err(ip=ip)))
						self.e(self.pststr())
						raise(SyntaxError('Trying to train str: {}'.format(ch)))
					return 1 if ch else 2
				if cur.es[0] in '}])':
					if ip <= cur.ep:
						return 1 if ch else 2
					if ch in " \n\t}])":
						return 0
					if ch in "({[": # Training
						cur.eb = None
						cur.es = '}])'['{[('.index(ch)]
						cur.sb = ib
						cur.sp = ip
						return 1 if ch else 2
		if not ch:
			return 2
		if ch in '}])':
			self.e('end fall thru: {}\n{}'.format(ch, self.mk_err(ip=ip)))
			self.e(self.pststr())
			raise(SyntaxError('end fall thru: {}'.format(ch)))
		if ch in '{[(':
			cur = Wpst()
			cur.es = '}])'['{[('.index(ch)]
			cur.tok = 'TRN'
			cur.sb = ib
			cur.sp = ip
			self.pst.append(cur)
			return 1
		if ch == '"':
			cur = Wpst()
			bp = ip
			#print('checkpst newstr', type(bp), ip)
			#print(self.mk_err())
			c = ib[bp-1:bp]
			while c and (c in "<>#|*"): # <|"hej"|>
				bp -= 1
				c = ib[bp-1:bp]
			cur.starts(ib[bp:ip+1])
			cur.tok = 'COM' if '#' in cur.es else 'STR'
			cur.sb = ib
			cur.sp = ip
			#cur.ctx = StrCtx()
			self.pst.append(cur)
			return 1
		return 2 if ch in ' \t\n' else 1

	def skip(self, chs=' \t\n'):
		"""Skip over set of chars in input buffer (default whitespace)."""
		ib = self.ib
		if ib == 'q':
			return
		ip = self.ip
		txt = ip
		c = ib[ip:ip+1]
		while self.checkpst(c, ip, ip-txt) & 2:
			if not c:
				if (self.po is not None) and self.w_in:
					if self.c_w:
						self.c_txt += ib[txt:ip] + '\n'
					self.call_fnc(self.w_in)
					if self.ib.strip() == '':
						self.ib = ''
						if self.i_mty:
							self.call_fnc(self.i_mty)
					ib = self.ib
					ip = self.ip
					txt = ip
					if ib == 'q':
						return
					c = ib[ip:ip+1]
					continue
				else:
					break
			ip += 1
			c = ib[ip:ip+1]
		if self.c_w:
			self.c_txt += ib[txt:ip]
		self.ip = ip
	

	def scan(self, chs=' \t\n'):
		"""Scan to a set of chars in input buffer (default whitespace)."""
		ib = self.ib
		ip = self.ip
		txt = ip
		c = ib[ip:ip+1]
		while self.checkpst(c, ip, ip-txt) & 1:
			ip += 1
			c = ib[ip:ip+1]
		if self.c_w:
			self.c_txt += ib[txt:ip]
		self.ip = ip

	def find(self, ses):
		"""find ses in input buffer starting at ip returning index in
			input buffer or ip if ses not found.""" 
		try:
			return self.ib.index(ses, self.ip)
		except:
			return self.ip

	def word(self):
		""" read one word from the input buffer to the word buffer """
		while True:
			self.skip()
			if self.ib == 'q':
				self.w = ''
				return
			sp = self.ip
			self.scan()
			if self.pst:
				cur = self.pst[-1]
				if (cur.es[0] == '"'):
					if cur.eb:
						if (cur.flag & IGCOM) and ('#' in cur.es):
							self.clrpstC_W()
							continue
						self.w = self.c_txt[cur.c_sp:len(self.c_txt)
							+ cur.ep - self.ip]
						self.clrpstC_W()
						return
					elif cur.sb == self.ib:
						ends = "#'" if '#' in cur.es else "'"
						cur.flag &= ~IGCOM
						self.w = self.ib[sp:self.ip-len(cur.es)] + ends
						return
			break
		self.w = self.ib[sp:self.ip]

	def sdots(self, _=None):
		"""nondestructive get stack as string"""
		return '{}: {}'.format(len(self.st),
			' '.join(whr_rs(n) for n in self.st))

	def sdot(self, _=None):
		"""non destructive stack print"""
		print(self.sdots())

	def lsw(self):
		"""ls words, print list of words."""
		print(' '.join(shstr(n, 15) for n in self.dic.keys()))

	def input(self, st):
		st.append(input(str(st.pop())))

	def inp(self, st=None):
		"""Print prompt and read input to input buffer."""
		pn = self.pn
		pnn = self.pnn
		if st is None: st = self.st

		if self.c_w:
			prompt = ': '
		elif self.po:
			prompt = self.po
		else:
			if len(st) > pn:
				prompt = str(len(st)) + ':<'
			else:
				prompt = '<'
			if st and pn:
				prompt = prompt + ' '.join(shdotr(n, pnn) for n in st[-pn:]) + '> '
			else:
				prompt = prompt + 'w> '
		self.setinbuf(input(prompt))
		if self.ib.strip() == 'q':
			self.ib = 'q'

	def binterp(self, x_p):
		"""bytecode interpreter aka bin/inner interpreter."""
		r = self.r
		mem = self.w_mem
		st = self.st
		r.append(x_p)
		bot = len(r)
		nxt = mem[r[-1]]
		while True:
			while callable(nxt):
				nxt(self, st)
				r[-1] += 1
				nxt = mem[r[-1]]
			if nxt is None:
				r.pop()
				if len(r) < bot:
					break
				r[-1] += 1
			else:
				r.append(nxt)
			nxt = mem[r[-1]]

	def literal(self, st):
		"""Convert literal in w and put on stack or raise error."""
		w = self.w
		if self.pst:
			cur = self.pst[-1]
			if cur.eb and cur.es[0] == '"':
				self.st.append(w)
				return
		if w == 'Nil':
			self.st.append(None)
			return
		try:
			i = int(w)
		except:
			pass
		else:
			self.st.append(i)
			return
		try:
			f = float(w);
		except:
			pass
		else:
			self.st.append(f)
			return
		tag = '(comp)' if self.compiling() else '(str)' if self.c_w else ''
		self.e("interp/literal{} Unknown: {}".format(
			tag, repr(self.w)))
		self.e(self.mk_err())
		raise SyntaxError('interp/literal: UNKNOWN')

	def compiling(self):
		"""return true if compiling"""
		return self.c_w and not (self.pst and (self.pst[-1].flag & C_W))

	def interp(env, st=None):
		"""Text interpreter - aka outer interpreter (also do compile)."""
		if st is None:
			st = env.st
		mem = env.w_mem
		dic = env.dic
		dl = env.dicl
		dd = env.dicd
		lit = dl[dic['lit>']]
		env.word()
		while env.w: # or (env.pst and (env.pst[-1].es[0] == '"'))):
			n = dic.get(env.w, None)
			if n is None:
				env.literal(st)
				if env.compiling():
					mem.append(lit)
					mem.append(st.pop())
			else:
				if env.compiling() and (not (dd[n].flag & Wmeta.IM)):
					mem.append(dl[n])
				else:
					env.call_fnc(dl[n])
			env.word()

	def clear_err(self):
		self.c_w = '';     self.c_flag = 0;     self.w = ''
		self.ib = '';      self.r.clear();        self.po = ''
		self.err = [];

	def mk_err(self, st=None, ip=None, ib=None):
		if ip is None:
			ip = max(0,self.ip-1)
		if ib is None:
			ib = self.ib
		try:
			ss = ib.rindex('\n', None, ip) + 1
			sss = ' ' * max(0, ip - ss) + '^'
		except:
			ss = None
			sss = ' ' * max(0, ip) + '^'
		try:
			sse = ib.index('\n', ss)
		except:
			sse = None
		return ib[ss:sse] + '\n' + sss

	def print_err(self, e=None):
		if self.err:
			if e is not None:
				print(e)
			if self.compiling():
				print('Compiling', self.c_w, self.c_flag, 'aborted')
				print('-->', self.c_txt)
			print('word: {}   ip: {}\nr: <{}> {}'.format(
				self.w, self.ip, len(self.r), self.r[-20:]))
			if(self.st):
				print('st: <{}> {}'.format(len(self.st), self.st[-9:]))
			if(self.sst):
				print('sst: <{}> {}'.format(len(self.sst), self.sst[-9:]))
		if self.err:
			print('\n'.join(self.err))
			print()
			self.clear_err()

	def run(self, cmd, *s, glob=None):
		"""Run whorth code in str cmd after adding s to stack.
			Return the stack."""
		self.st.extend(s)
		ib = self.ib
		ip = self.ip
		w = self.w
		w_in = self.w_in 
		self.w_in = 0
		self.setinbuf(cmd)
		glob = self.setglob(glob)
		try:
			self.interp()
		except:
			self.print_err('ERROR in run()')
			raise
		finally:
			self.setglob(glob)
			self.ib = ib
			self.ip = ip
			self.w = w
			self.w_in = w_in
		return self.st

	def sh(self, st=None):
		"""Run an interactive whorth shell."""
		print("\nWHORTH - the ugly duckling of Forth.\n")
		print(
		  ' Entering "help" give help. If not "imp\' lib.help" should load it.')
		print(' "q" on empty line to quit. "lsw" to see loaded words.\n')
		if st is None:
			st = self.st
		self.clear_err()
		self.w_in = self.dicl[self.dic['inp']]
		while self.ib != 'q':
			try:
				self.interp()
			except EOFError as e:
				if self.c_w:
					print("\nEOF while compiling")
					self.print_err(e)
					print(format_exc())
				self.ib = ''
				self.w_in = 0
				print('  EOF bye')
				return
			except Exception as e:
				if self.ib == 'q':
					print("\nException caught while quitting sh")
					self.print_err(e)
					print(format_exc())
					break
				else:
					print("\nException caught in whorth sh interp()")
					self.print_err(e)
					self.err_pyst = format_exc()
		self.ib = ''
		self.w_in = 0
		print('   bye')

if __name__ == '__main__':
	env = Whorth() # pack=False)
	env.sh()

