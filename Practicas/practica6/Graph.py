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

	def dfs_visit(self, G, u, found_cycle, pred_node, marked):
		if found_cycle[0]:
			return
		marked[u] = True
		for v in G:
			if marked[v.etiqueta] and v.etiqueta != pred_node:
				found_cycle[0] = True
				return
			if not marked[v.etiqueta]:
				self.dfs_visit(G, v, found_cycle, u, marked)

	def cycle_exists(self):
		marked = { v.etiqueta : False for v in self.listvertices }
		found_cycle = [False]

		for v in self.listvertices:
			if not marked[v.etiqueta]:
				self.dfs_visit(self.listvertices, v.etiqueta, found_cycle, v.etiqueta, marked)
			if found_cycle[0]:
				break
		return found_cycle[0]

	def dfs_visit2(self, G, u, found_cycle, pred_node, marked):
		if found_cycle[0]:
			return
		marked[u] = True
		for v in G:
			if marked[v] and v != pred_node:
				found_cycle[0] = True
				return
			if not marked[v]:
				self.dfs_visit2(self.get_out(v), v, found_cycle, u, marked)

	def cycle_exists2(self):
		marked = { v.etiqueta : False for v in self.listvertices }
		found_cycle = [False]

		for v in self.listvertices:
			if not marked[v.etiqueta]:
				self.dfs_visit2(self.get_out(v.etiqueta), v.etiqueta, found_cycle, v.etiqueta, marked)
			if found_cycle[0]:
				break
		return found_cycle[0]

	def get_out(self, vertice):
		outs = []
		for a in self.aristas:
			if a.origen == vertice:
				outs.append(a.destino)
		return outs