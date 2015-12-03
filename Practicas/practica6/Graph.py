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

	def set_vertices(self, vertices):
		self.vertices = listvertices

	def set_aristas(self, aristas):
		self.aristas = aristas

	def add_vertice(self, vertice):
		self.listvertices.append(vertice)

	def add_arista(self, arista):
		self.aristas.append(arista)

	def directed(self):
		return self.dirigida

	def vertices(self):
		return self.listvertices

	def edges(self):
		return self.aristas

	def get_etiquetas(self):
		etiquetas = []
		for vertice in self.listvertices:
			etiquetas.append(vertice.etiqueta)
		return etiquetas