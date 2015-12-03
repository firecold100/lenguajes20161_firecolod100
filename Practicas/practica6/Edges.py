from Vertex import *

class Edges(object):

	def __init__(self, origen, destino, peso):		
		self.origen = origen
		self.destino = destino
		self.peso = peso
	
	def svertex():
		print('Origen')

	def tvertex():
		print('destino')

	def wight():
		print('peso')

	def __str__(self):
		return "origen: (%s), destino: (%s), peso: %s" % (str(self.origen), str(self.destino), str(self.peso))