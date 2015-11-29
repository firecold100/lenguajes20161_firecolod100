#!/usr/bin/env python
import csv
  
reader = csv.reader(open('graph.csv', 'rb'))
tipo= next(reader)
next(reader, None)
print(tipo)
for row in enumerate(reader):
    print '------------'
    print 'Orgien: ' + row[0] + ', Destino: ' + row[1] + ', Peso: ' + row[2]    