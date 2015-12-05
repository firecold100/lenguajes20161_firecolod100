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

#Metodo solicitado 
	def has_cycles(self):		
		if self.directed() == True:
			return self.cycle_exists2()	
		else: 
			return self.cycle_exists()

	def get_etiquetas(self):
		etiquetas = []
		for vertice in self.listvertices:
			etiquetas.append(vertice.etiqueta)
		return etiquetas	

	def dfs_visit(self, G, u, found_cycle, pred_node, marked):
		if found_cycle[0]:
			return
		marked[u] = True
		for v in G:
			if marked[v] and v != pred_node:
				found_cycle[0] = True
				return
			if not marked[v]:
				for vertice in self.listvertices:
					if vertice.etiqueta == v:
						self.dfs_visit(vertice.neighbours(), v, found_cycle, u, marked)

	def cycle_exists(self):
		marked = { v.etiqueta : False for v in self.listvertices }
		found_cycle = [False]

		for v in self.listvertices:
			if not marked[v.etiqueta]:
				self.dfs_visit(v.neighbours(), v.etiqueta, found_cycle, v.etiqueta, marked)
			if found_cycle[0]:
				break
		return found_cycle[0]

	#Este metodo visita lo vecinos en una grafica es dirigida
	def dfs_visit2(self, G, u, found_cycle, pred_node, marked):
		#si ya encontro un ciclo
		if found_cycle[0]:
			return
		marked[u] = True #marca al vecino
		for v in G: #para cada vecino
			#si ya esta marcado y viene de otro que no estaba marcado
			if marked[v] and v != pred_node:
				found_cycle[0] = True
				return
			if not marked[v]: #si no estaba marcado lo marca y vuelve a marcar a sus vecinos
				self.dfs_visit2(self.get_out(v), v, found_cycle, u, marked)

	def cycle_exists2(self): #Este metodo se debe regrsar si la grafica es dirigida
		#para facilitar el acceso a los nodos (hash) y saber si ya fue visitado o no
		marked = { v.etiqueta : False for v in self.listvertices }
		#para ver si ya encontro un ciclo, como es la inicializacion es false
		found_cycle = [False]

		for v in self.listvertices: #a cada vertice
			if not marked[v.etiqueta]: #si no esta marcado
				#aqui llamamos al metodo que visita a los vecinos
				self.dfs_visit2(self.get_out(v.etiqueta), v.etiqueta, found_cycle, v.etiqueta, marked)
			if found_cycle[0]: #si ya encontro un ciclo rompe
				break
		return found_cycle[0]

	def get_out(self, vertice): #Metodo que regresa los vecinos de salida del vertice parametro
		outs = []
		for a in self.aristas:
			if a.origen == vertice:
				outs.append(a.destino)
		return outs