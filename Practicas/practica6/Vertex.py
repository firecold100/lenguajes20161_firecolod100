class Vertex(object):
	def __init__(self, etiqueta, grado, vecinos):
		self.grado = grado
		self.etiqueta = etiqueta
		self.vecinos = vecinos

	def incrementar_grado(self):
		self.grado += 1

	def add_vecino(self, vertice):
		self.vecinos.append(vertice)
		self.grado += 1

	def __str__(self):
		objeto = "id: "+ str(self.etiqueta) + ", grado: "+ str(self.grado) + ", vecinos: ["
		mvecinos = ','.join([("("+str(vecino)+")") for vecino in self.vecinos])
		objeto += mvecinos + "]"
		return objeto