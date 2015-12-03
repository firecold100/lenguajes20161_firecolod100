
import csv, json
from Graph import *
from Vertex import *
from Edges import *
from xml.dom import minidom


def readCSV():
	graph = Graph()
	reader = csv.reader(open('graph.csv', 'r'))
	tipo= next(reader, None)	
	if tipo ==['direct=1']:
		print('Dirigida')
		graph.set_dirigida(True)
	else:
		print('No dirigida')
		for row in enumerate(reader):
			if row[1][0] not in graph.get_etiquetas():
				graph.add_vertice(Vertex(row[1][0], 1, [row[1][1][2::3]]))
			else:
				for vertice in graph.vertices():
					if vertice.etiqueta == row[1][0]:
						vertice.add_vecino(row[1][1][2::3])
			if row[1][1][2::3] not in graph.get_etiquetas():
				graph.add_vertice(Vertex(row[1][1][2::3], 1, [row[1][0]]))
			else:
				for vertice in graph.vertices():
					if vertice.etiqueta == row[1][1][2::3]:
						vertice.add_vecino(row[1][0])
		for vertice in graph.vertices():
		 	print(vertice)
	pesos = []
	origen = []
	destino = []
	for row in enumerate(reader):
		print('Orgien: ' +row[1][0]+', Destino: '+row[1][1][2::3]+', Peso: '+row[1][2][1::])
		origen.append(row[1][0]) 		
		destino.append(row[1][1][2::3])
		pesos.append(row[1][2][1::])
	print(origen)
	print(destino)
	print(pesos)

def readJSON():
	with open('graph.json',encoding='utf-8') as data_file:
		data =  json.loads(data_file.read())
	print(data)

def readXML():
	doc = minidom.parse("graph.xml")
	vertexs = doc.getElementsByTagName("vertex")
	lis = []
	for vertex in vertexs:
		lis.append(vertex.getAttribute("label"))	
		