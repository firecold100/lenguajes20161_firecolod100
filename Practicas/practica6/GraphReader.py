
import csv
import json   
from xml.dom import minidom


def readCSV():
	reader = csv.reader(open('graph.csv', 'r'))
	tipo= next(reader, None)	
	if tipo ==['direct=1']:
		print('Dirigida')
	else:
		print('No dirigida')
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
		