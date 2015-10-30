# -*- coding: utf-8 -*-
"""
Created on Wed Apr  1 14:23:47 2015

@author: bolaka
"""

import os
os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
import datetime
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/COOP')
    
idCol = 'WELL ID'
testingAllfilename = 'coop-mudwts.csv'
zonesfilename = 'zones.csv'

testingAll = pd.read_csv(testingAllfilename,  index_col=idCol) 
zones = pd.read_csv(zonesfilename) #,index_col=idCol

print('testingAll set has', rows(testingAll))

# normalize 
#testingAll = normalizeColumns(testingAll, ['DAYSFROMSPUDCALC'])

depthCol = 'DEPTH'
mudwtCol = 'MUD WEIGHT'
depthTopCol = 'DEPTHTOP'
depthBtmCol = 'DEPTHBTM'
threshold = 200

# remove the irrelevant values from data
testingAll[depthCol] = [float(x) for x in testingAll[depthCol]]
testingAll = testingAll.loc[ testingAll[depthCol] > 0 ]
testingAll = testingAll.loc[ np.isnan(testingAll[mudwtCol]) == False ] 
print('testingAll set has', rows(testingAll))
print('zones set has', rows(zones))

# remove the irrelevant values from zones
zones = zones.loc[ np.isnan(zones[depthTopCol]) == False ]
zones = zones.loc[ np.isnan(zones[depthBtmCol]) == False ] 
print('zones set has', rows(zones))

formations = addFormations(testingAll, zones, depthCol, depthTopCol, depthBtmCol, 'IDWELL', threshold, 'ZONENAME')
formations.to_csv('coop-formations.csv', sep=',', encoding='utf-8')