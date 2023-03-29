"""LEB128 encoding and decoding and some integer bound checking.

Integer type hints used in function names
	int   Python varlen big int.
	U     Python varlen big int as unsigned.
	S     Python varlen big int as signed.
	i     32bit integer.
	si    signed 32bit integer.
	ui    unsigned 32bit integer.
	l     64bit integer.
	sl    signed 64bit integer.
	ul    unsigned 64bit integer.

Canonical import:  import leb128 as leb
"""


def decU(buf):
	"""Decode LEB128 varint unsigned numbers from buf.

		Return tuple with result and number of chars read. buf
		can be any indexable sequece holding values 0..255."""
	i = 0
	s = 0
	n = 0
	while buf[i] & 128:
		n += (buf[i] & 127) << s
		i += 1
		s += 7
	n += buf[i] << s
	return (n, i+1)

def decS(buf):
	"""Decode LEB128 varint signed numbers from buf.

		Return tuple with result and number of chars read. buf
		can be any indexable sequece holding values 0..255."""
	n, i = decU(buf)
	if n & (1 << (i * 7 - 1)):
		return (-1 & ~((1 << i * 7) - 1) | n, i)
	return (n, i)

def encU(n, buf=None):
	"""Encode unsigned to LEB128 varint numbers to buf.

		If a buffer is passed extend that else create a new. buf
		can be bytearray, list & probably any mutable secquence."""
	if n < 0:
		raise ValueError('negative value to unsigned func leb128ue')
	if buf is None:
		buf = bytearray()
	while True:
		b = n & 127
		n = n >> 7
		if n > 0:
			buf.append(b | 128)
		else:
			buf.append(b)
			break
	return buf

def encS(n, buf=None):
	"""Encode signed to LEB128 varint numbers to buf.

		If a buffer is passed extend that else create a new. buf
		can be bytearray, list & probably any mutable secquence."""
	if buf is None:
		buf = bytearray()
	if n < 0:
		while True:
			b = n & 127
			n = n >> 7
			if n < -1:
				buf.append(b | 128)
			else:
				if not (b & 64):
					# must add extra 127 else mistaken for positive number
					buf.append(b | 128)
					buf.append(127)
				else:
					buf.append(b)
				return buf
	while True:
		b = n & 127
		n = n >> 7
		if n > 0:
			buf.append(b | 128)
		else:
			if b & 64:
				# must add extra 0 else mistaken for negative number
				buf.append(b | 128)
				buf.append(0)
			else:
				buf.append(b)
			return buf

ui_LIM = 2 ** 32
def uic(n):
	"""Check bounds of u32 and return True if OK."""
	return 0 <= n < ui_LIM

def uie(n):
	"""Check bounds of u32 and pass it thru or raise exception."""
	if uic(n):
		return n
	raise ValueError(
		'{} not an u32 (not in 0 <= n < 2**32)'.format(n))

si_LIM = 1 << 31
def sic(n):
	"""Check bounds of i32 and return True if OK."""
	return -si_LIM <= n < si_LIM

def sie(n):
	"""Check bounds of i32 and pass it thru or raise exception."""
	if sic(n):
		return n
	raise ValueError(
		'{} not an i32 (not in -2**31 <= n < 2**31)'.format(n))

ul_LIM = 1 << 64
def ulc(n):
	"""Check bounds of u64 and return True if OK."""
	return 0 <= n < ul_LIM

def ule(n):
	"""Check bounds of u64 and pass it thru or raise exception."""
	if ulc(n):
		return n
	raise ValueError(
		'{} not an u64 (not in 0 <= n < 2**64)'.format(n))

sl_LIM = 1 << 63
def slc(n):
	"""Check bounds of i64 and return True if OK."""
	return -sl_LIM <= n < sl_LIM

def sle(n):
	"""Check bounds of u64 and pass it thru or raise exception."""
	if slc(n):
		return n
	else:
		raise ValueError(
			'{} not an i64 (not in -2**63 <= n < 2**63)'.format(n))

if __name__ == '__main__':
	# some testing.'
	import time
	import math
	fail=[]
	ec = len(fail)

	def test_c(func_c, frm, lim, tag):
		if not func_c(lim - 1):
			fail.append('FAIL A:{}({}) should be True!'.format(
				lim - 1, tag))
		if func_c(lim):
			fail.append('FAIL B:{}({}) should be False!'.format(
				lim, tag))
		if not func_c(frm):
			fail.append('FAIL C:{}({}) should be True!'.format(
				frm, tag))
		if func_c(frm - 1):
			fail.append('FAIL D:{}({}) should be False!'.format(
				frm - 1, tag))

	def test_e(func_e, frm, lim, tag):
		try:
			x = func_e(lim - 1)
		except ValueError:
			fail.append('FAIL A{}({}) should not raise!'.format(
				lim, tag))
		else:
			if x != (lim - 1):
				fail.append('FAIL B{}({}) not passed thru!'.format(
					lim, tag))
		try:
			x = func_e(lim)
		except ValueError:
			pass
		else:
			fail.append('FAIL C{}({}) should raise!'.format(
				lim, tag))
		try:
			x = func_e(frm)
		except ValueError:
			fail.append('FAIL D{}({}) should not raise!'.format(
				frm, tag))
		else:
			if x != frm:
				fail.append('FAIL E{}({}) not passed thru!'.format(
					frm, tag))
		try:
			x = func_e(frm - 1)
		except ValueError:
			pass
		else:
			fail.append('FAIL F{}({}) should raise!'.format(
				frm - 1, tag))

	def test_lebU(seed, lim, tag):
		i = 0
		if seed < 1:
			seed = 1
		i = 0
		n = seed
		while n < lim:
			#print('test_lebU', n, lim)
			enc = encU(n)
			r, l = decU(enc)
			if r != n:
				fail.append('FAIL A{} i:{} r:{} != n:{} l:{} enc:{}'.format(
					tag, i, r, n, l, str(enc)))
			if len(enc) != l:
				fail.append('FAIL B{} i:{} l:{} != len({})'.format(
					tag, i, l, str(enc)))
			n = n << 1
			i += 1

	def test_lebS(seed, lim, tag):
		i = 0
		if seed < 1:
			seed = 1
		i = 0
		n = seed
		while n < lim:
			#print('test_lebS', n, lim)
			enc = encS(n-1)
			r, l = decS(enc)
			if r != (n-1):
				fail.append('FAIL A{} i:{} r:{} != n:{} l:{} enc:{}'.format(
					tag, i, r, n-1, l, str(enc)))
			if len(enc) != l:
				fail.append('FAIL B{} i:{} l:{} != len({})'.format(
					tag, i, l, str(enc)))
			enc = encS(-n)
			r, l = decS(enc)
			if r != -n:
				fail.append('FAIL C{} i:{} r:{} != n:{} l:{} enc:{}'.format(
					tag, i, r, n, l, str(enc)))
			if len(enc) != l:
				fail.append('FAIL D{} i:{} l:{} != len({})'.format(
					tag, i, l, str(enc)))
			n = n << 1
			i += 1

	#Performance tests
	def perf_lebU(bits, rep=1000):
		bits = max(bits, 3)
		lim = 2 ** bits
		irep = max(1, 13777 // ((bits//6+1)*max(bits,7)))
		blen = bits // 7 + 3
		tote = 0.0
		mine = 1.0
		maxe = 0.0
		totd = 0.0
		mind = 1.0
		maxd = 0.0
		lentot = 0
		for i in range(rep):
			#if (i & 15) == 0:
			print('  perf{:5}/{}'.format(i, rep), end='\r')
			buf = bytearray()
			t = time.thread_time()
			for j in range(irep):
				n = 2
				while n < lim:
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					buf = encU(n-1, buf)
					n = n << 1
			t = time.thread_time() - t
			tote += t
			mine = min(mine, t)
			maxe = max(maxe, t)
			lentot += len(buf)
			#print('tote:', tote, 'lentot:', lentot)
			pos = 0
			t = time.thread_time()
			while pos < len(buf):
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
				n, l = decU(buf[pos:pos+blen])
				pos += l
			t = time.thread_time() - t
			totd += t
			mind = min(mind, t)
			maxd = max(maxd, t)
		kb = lentot / 1024
		print("perf leb128 encU:{:6} KB/s    decU:{:6} KB/s{:12.2f} KB".format(
			int(kb / tote), int(kb / totd), kb))
		maxe *= rep
		maxd *= rep
		print("bits  worst encU:{:6} KB/s    decU:{:6} KB/s".format(
			int(kb / maxe), int(kb / maxd)))
		mine *= rep
		mind *= rep
		print("{:4}   best encU:{:6} KB/s    decU:{:6} KB/s".format(
			bits, int(kb / mine), int(kb / mind)))
		print()
		return (kb / tote, kb / totd, kb / mine, kb / mind)

	def perf_lebS(bits, rep=1000):
		bits = max(bits, 3)
		lim = 2 ** (bits-1)
		irep = max(1, 13777 // ((bits//6+1)*max(bits,7)))
		blen = bits // 7 + 3
		tote = 0.0
		mine = 1.0
		maxe = 0.0
		totd = 0.0
		mind = 1.0
		maxd = 0.0
		lentot = 0
		for i in range(rep):
			#if (i & 15) == 0:
			print('  perf{:5}/{}'.format(i, rep), end='\r')
			#print('loop:', i, blen)
			buf = bytearray()
			t = time.thread_time()
			for j in range(irep):
				n = 1
				while n < lim:
					buf = encS(n-1, buf)
					buf = encS(-n, buf)
					buf = encS(n-1, buf)
					buf = encS(-n, buf)
					buf = encS(n-1, buf)
					buf = encS(-n, buf)
					buf = encS(n-1, buf)
					buf = encS(-n, buf)
					buf = encS(n-1, buf)
					buf = encS(-n, buf)
					n = n << 1
			t = time.thread_time() - t
			tote += t
			mine = min(mine, t)
			maxe = max(maxe, t)
			lentot += len(buf)
			#print('tote:', tote, 'lentot:', lentot)
			pos = 0
			t = time.thread_time()
			while pos < len(buf):
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
				n, l = decS(buf[pos:pos+blen])
				pos += l
			t = time.thread_time() - t
			totd += t
			mind = min(mind, t)
			maxd = max(maxd, t)
		kb = lentot / 1024
		print("perf leb128 encS:{:6} KB/s    decS:{:6} KB/s{:12.2f} KB".format(
			int(kb / tote), int(kb / totd), kb))
		maxe *= rep
		maxd *= rep
		print("bits  worst encS:{:6} KB/s    decS:{:6} KB/s".format(
			int(kb / maxe), int(kb / maxd)))
		mine *= rep
		mind *= rep
		print("{:4}   best encS:{:6} KB/s    decS:{:6} KB/s".format(
			bits, int(kb / mine), int(kb / mind)))
		print()
		return (kb / tote, kb / totd, kb / mine, kb / mind)

	# Long integer test (i64)
	test_c(slc, -(2 ** 63), 2 ** 63, '1 slc')
	test_e(sle, -(2 ** 63), 2 ** 63, '1 sle')

	# Long unsigned integer test (i64)
	test_c(ulc, 0, 2 ** 64, '2 ulc')
	test_e(ule, 0, 2 ** 64, '2 ule')

	# Integer test (i32)
	test_c(sic, -(2 ** 31), 2 ** 31, '1 sic')
	test_e(sie, -(2 ** 31), 2 ** 31, '1 sie')

	# Unsigned integer test (i32)
	test_c(uic, 0, 2 ** 32, '2 uic')
	test_e(uie, 0, 2 ** 32, '2 uie')

	# leb128 tests
	test_lebU(5, 1 << 68, 'LEBu')
	test_lebU(55, 1 << 68, 'LEBu')
	test_lebU(777, 1 << 68, 'LEBu')
	test_lebS(5, 1 << 68, 'LEBi')
	test_lebS(55, 1 << 68, 'LEBi')
	test_lebS(777, 1 << 68, 'LEBi')
	
	# So far only tested inner consistensy. Following far to few
	# datapoints from the wild.

	# From: https://en.wikipedia.org/wiki/LEB128
	# unsigned example: 624485 = 0xE5 0x8E 0x26
	buf = bytes((int('E5', 16), int('8E', 16), int('26', 16)))
	r, l = decU(buf)
	if 624485 != r:
		fail.append('FAIL Wikipedia unsigned dec r:{}'.format(r))
	if l != 3:
		fail.append('FAIL Wikipedia unsigned dec l:{}'.format(l))
	r = bytes(encU(624485))
	if r != buf:
		fail.append('FAIL Wikipedia unsigned enc r:{}'.format(r.hex()))
	# signed example: -123456 = 0xC0 0xBB 0x78
	buf = bytes((int('C0', 16), int('BB', 16), int('78', 16)))
	r, l = decS(buf)
	if -123456 != r:
		fail.append('FAIL Wikipedia signed dec r:{}'.format(r))
	if l != 3:
		fail.append('FAIL Wikipedia signed dec l:{}'.format(l))
	r = bytes(encS(-123456))
	if r != buf:
		fail.append('FAIL Wikipedia signed enc r:{}'.format(r.hex()))

	# Performance 'test'... it's *slow*
	perf_lebU(64, 40)
	perf_lebU(32, 40)
	perf_lebU(16, 40)
	perf_lebU(8, 40)
	perf_lebU(4, 40)
	perf_lebS(64, 40)
	perf_lebS(32, 40)
	perf_lebS(16, 40)
	perf_lebS(8, 40)
	perf_lebS(4, 40)

	if fail:
		print('\n'.join(fail))
		print()
		print("In total {} failed tests.".format(len(fail)))
		exit(-len(fail))
	else:
		print('All tests pass.')




