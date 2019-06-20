#!/usr/bin/env python

import string, Numeric, math
chan0 = []
gain1 = []
gain2 = []
off3 = []
off4 = []
datafile = file('LABOCA-2009-05.rcp','r')
datalist = datafile.readlines()
for i in range(len(datalist)):
  if not (datalist[i].startswith('!') or datalist[i].isspace()):
    chan0.append(string.atof(datalist[i].split()[0]))
    gain1.append(string.atof(datalist[i].split()[1]))
    gain2.append(string.atof(datalist[i].split()[2]))
    off3.append(string.atof(datalist[i].split()[3]))
    off4.append(string.atof(datalist[i].split()[4]))
datafile.close()

output=file('LABOCA-for-APECS.rcp','w')
output.write("# Feed  Gain    X [\"]    Y [\"]\n")
for i in range(len(datalist)):
    output.write('%5i  %6.3f %8.2f %8.2f\n' %(chan0[i],gain1[i],off3[i],off4[i]))
output.close()
