class Vertex(object):
	def __init__(self, etiqueta, grado, vecinos):
		self.grado = grado
		self.etiqueta = etiqueta
		self.vecinos = vecinos
#Metodo solicitado
	def degree(self):
		return self.grado
#Metodo solicitado
	def neighbours(self):
		return self.vecinos

	def add_vecino(self, vertice):
		self.vecinos.append(vertice)
		self.grado += 1

	def __repr__(self):
		return "%s"	% (self.etiqueta)