#!/usr/bin/env python

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
		print('Orgien: ' +row[1][0]+', Destino: '+row[1][1]+', Peso: '+row[1][2])
		origen.append(row[1][0]) 		
		destino.append(row[1][1])
		pesos.append(row[1][2])
	print(origen[0])
	print(destino[0])
	print(pesos[0])

def readJSON():
	with open('graph.json',encoding='utf-8') as data_file:
		data =  json.loads(data_file.read())
	print(data)

def readXML():
	doc = minidom.parse("graph.xml")
	vertexs = doc.getElementsByTagName("vertex")
	for vertex in vertexs:
		sid = vertex.getAttribute("label")
		print("label:%s " %(sid))