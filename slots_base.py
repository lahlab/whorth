# SlotsBase class for classes with slots. Add som nifty functions
# Open sorceish - licence still to be chosen
# Terapy project so far only usefull for play - but feel free to play!
# (c) Lars Hallberg lah2180@gmail.com

class SlotsBase():

	def asdict(self):
		"""Return set values as a dict."""
		d = { }
		for k in self.__slots__.keys():
			v = getattr(self, k)
			if v is not None:
				d[k] = v
		return d

	def fromdict(self, dic):
		"""Set values from dict..."""
		for k in self.__slots__.keys():
			if k in dic:
				setattr(self, k, dic[k])

	def __getitem__(self, key):
		"""Make values readable by []."""
		#if key not in self.__slots__:
		#	raise KeyError
		return getattr(self, key)

	def __iter__(self):
		"""Make it possible to iterate over keys of set values."""
		return self.asdict().__iter__()

	def __str__(self):
		"""Returns string presentation."""
		return '{' + ' '.join('{}:{}'.format(str(k), repr(v))
			for k, v in self.asdict().items()) + '}'

	def __repr__(self):
		"""Same as asdict."""
		return str(self.asdict())

