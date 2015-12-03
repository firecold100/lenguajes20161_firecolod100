class Vertex(object):
	def __init__(self, etiqueta, grado, vecinos):
		self.grado = grado
		self.etiqueta = etiqueta
		self.vecinos = vecinos

	def get_grado(self):
		return self.grado

	def setVecino(self, vertice):
		self.vecinos.append(vertice)

	def __str__(self):
		objeto = "id: "+ str(self.etiqueta) + ", grado: "+ str(self.grado) + ", vecinos: ["
		mvecinos = ','.join([("("+str(vecino.etiqueta)+")") for vecino in self.vecinos])
		objeto += mvecinos + "]"
		return objeto