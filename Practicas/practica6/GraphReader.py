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
		graph.set_dirigida(True)
		for row in enumerate(reader):
			if row[1][0] not in graph.get_etiquetas():
				graph.add_vertice(Vertex(row[1][0], 0, [row[1][1][2::3]]))
			else:
				for vertice in graph.vertices():
					if vertice.etiqueta == row[1][0]:
						vertice.add_vecino(row[1][1][2::3])
			graph.add_arista(Edges(row[1][0],row[1][1][2::3],row[1][2][1::]))
	else:		
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
			graph.add_arista(Edges(row[1][0],row[1][1][2::3],row[1][2][1::]))
	return graph

def readJSON():
	graph = Graph()	
	with open('graph.json',encoding='utf-8') as data_file:
		data =  json.loads(data_file.read())
	if data['direct']==1:
		graph.set_dirigida(True)
		for vertice in data['vertices']:
			graph.add_vertice(Vertex(vertice,0,[]))
		for arista in data['edges']:			
			graph.add_arista(Edges(arista[0],arista[1],arista[2]))
			for vertice in graph.vertices():
				if vertice.etiqueta == arista[0]:
					vertice.add_vecino(arista[1])
	for v in graph.vertices():
		print(v)
		print(v.vecinos)
	return graph

def readXML():
	doc = minidom.parse("graph.xml")
	vertexs = doc.getElementsByTagName("vertex")
	lis = []
	for vertex in vertexs:
		lis.append(vertex.getAttribute("label"))	
