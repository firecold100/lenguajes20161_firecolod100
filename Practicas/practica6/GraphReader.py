#!/usr/bin/env python

import csv
import json   

def readCVS():
	reader = csv.reader(open('graph.csv', 'rb'))
	next(reader, None)
	for row in enumerate(reader):
		print 'Orgien: ' + row[1][0] + ', Destino: ' + row[1][1] + ', Peso: '+ row[1][2]

def readJSON():
	prueba =  json.loads(open('graph.json').read())
	print(prueba)