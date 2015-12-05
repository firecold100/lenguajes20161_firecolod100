import csv, json, os
from Graph import *
from Vertex import *
from Edges import *
from xml.dom import minidom


def readCSV(archivo):
	graph = Graph()
	reader = csv.reader(open(os.getcwd()+"/ejemplos/"+archivo, 'r'))
	tipo= next(reader, None)	
	if tipo ==['direct=1']:		
		graph.set_dirigida(True)		
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

def readJSON(archivo):
	graph = Graph()	
	with open(os.getcwd()+"/ejemplos/"+archivo,encoding='utf-8') as data_file:
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
				for vertex in graph.vertices():
					if vertex.etiqueta == arista[1]:
						vertex.add_vecino(arista[0])	
	return graph

def readXML(archivo):
	graph=Graph()
	doc = minidom.parse(os.getcwd()+"/ejemplos/"+archivo)	
	vertexs = doc.getElementsByTagName("vertex")
	edges = doc.getElementsByTagName("edge")
	grafo = doc.getElementsByTagName("graph")		
	lis=[]
	for g in grafo:
		lis.append(g.getAttribute("direct"))		
		if lis[0] == '1':
			graph.set_dirigida(True)
	for vertex in vertexs:
		graph.add_vertice(Vertex(vertex.getAttribute("label"),0,[]))		
	for edge in edges:
		graph.add_arista(Edges(edge.getAttribute("source"),edge.getAttribute("target"),edge.getAttribute("weight")))
		for vertice in graph.vertices():
			if vertice.etiqueta == edge.getAttribute("source"):
				vertice.add_vecino(edge.getAttribute("target"))
				for vertex in graph.vertices():
					if vertex.etiqueta == edge.getAttribute("target"):
						vertex.add_vecino(edge.getAttribute("source"))
	return graph

def main():
	lfiles = os.listdir(os.getcwd()+"/ejemplos")
	graphs = []
	for element in lfiles:
		if element.find(".csv") != -1:
			graphs.append((element, readCSV(element)))
		elif element.find(".json") != -1:
			graphs.append((element, readJSON(element)))
		elif element.find(".xml") != -1:
			graphs.append((element, readXML(element)))
	for graph in graphs:
		print("Archivo: "+ graph[0])
		print("Dirigida: "+ str(graph[1].directed()))
		print("Vertices: "+ str(graph[1].vertices()))
		print("Aristas: "+ str(graph[1].edges()))
		print("Tiene Ciclos: "+ str(graph[1].has_cycles()))
		print("\n ")

if __name__ == '__main__':
	main()