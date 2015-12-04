from Vertex import *
from Edges import *

class Graph:
	"""docstring for Graph"""
	def __init__(self):
		self.dirigida = False
		self.listvertices = []
		self.aristas = []

	def set_dirigida(self, dirigida):
		self.dirigida = dirigida

	def add_vertice(self, vertice):
		self.listvertices.append(vertice)

	def add_arista(self, arista):
		self.aristas.append(arista)

#Metodo solicitado
	def directed(self):
		return self.dirigida

#Metodo solicitado
	def vertices(self):
		return self.listvertices

#Metodo solicitado
	def edges(self):
		return self.aristas

	def get_etiquetas(self):
		etiquetas = []
		for vertice in self.listvertices:
			etiquetas.append(vertice.etiqueta)
		return etiquetas

	def incr_grado(self, etiqueta):
		for vertice in self.listvertices:
			if vertice.etiqueta == etiqueta:
				vertice.incr_grado()