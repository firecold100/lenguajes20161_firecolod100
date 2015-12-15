from Vertex import *

class Edges(object):
	def __init__(self, origen, destino, peso):		
		self.origen = origen
		self.destino = destino
		self.peso = peso
	
	def svertex(self):
		return self.origen

	def tvertex(self):
		return self.destino

	def weight(self):
		return self.peso

	def __repr__(self):
		return "(%s,%s,%s)" % (self.origen , self.destino, self.peso)
