# -*- coding: utf-8 -*-
"""
Created on Tue Apr  7 13:35:38 2015

@author: bolaka
"""

import os

os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV')
    
idCol = 'IDWELL'
testingAllfilename = 'lostMudAllWellsClassified-csg.csv'# 'sayantani-classified.csv' lostMudAllWellsClassified-new
zonesfilename = 'zones.csv'
savedfilename = 'nojv-formations.csv'

testingAll = pd.read_csv(testingAllfilename,  index_col=idCol) 
zones = pd.read_csv(zonesfilename) #,index_col=idCol

print('testingAll set has', rows(testingAll))

depthCol = 'depth'
mudwtCol = 'mud-wt'
visCol = 'viscosity'
phCol = 'pH'
ropCol = 'ROP'
depthTopCol = 'DEPTHTOP'
depthBtmCol = 'DEPTHBTM'
threshold = 200
metric1 = 'Circulation'
metric2 = 'Total Loss'
metric3 = 'Mud Loss'

#testingAll[metric3] = 0
#testingAll.loc[testingAll[metric2] == 0, metric3] = 1
#testingAll.loc[testingAll[metric1] == 0, metric2] = 0
#testingAll.loc[testingAll[metric1] == 0, metric3] = 0

featuresToSave = ['SUMMARYOPS','DTTMSTART', 'LATITUDE', 'LONGITUDE', 'WELLIDA', metric2, metric1, metric3] #
testingAll = extractMudWtFeatures(testingAll, 'mudwt-features.csv', featuresToSave, 'NOJV')

bagOfWords = [ 'LCM', 'RETURNS' ]
extractFeatures(testingAll, 'testing-fuzzy-features.csv',bagOfWords)

testingAll[depthCol] = [float(x) for x in testingAll[depthCol]]
testingAll[mudwtCol] = [float(x) for x in testingAll[mudwtCol]]
testingAll[visCol] = [float(x) for x in testingAll[visCol]]
testingAll[phCol] = [float(x) for x in testingAll[phCol]]
#testingAll[ropCol] = [float(x) for x in testingAll[ropCol]]

## filter only rows that have tvds
#testingAll = testingAll.loc[ testingAll[depthCol] > 0 ]
#
## filter only rows that have lost events
##testingAll = testingAll.loc[ testingAll[metric1] > 0 ]
#
## filter only rows that Total Loss classified
##testingAll = testingAll.loc[ testingAll[metric2] >= 0 ]
#print('testingAll set has', rows(testingAll))
#print('zones set has', rows(zones))
#
## remove the irrelevant values from zones
#zones = zones.loc[ np.isnan(zones[depthTopCol]) == False ]
#zones = zones.loc[ np.isnan(zones[depthBtmCol]) == False ] 
#print('zones set has', rows(zones))
#
## add the formations zones
#formations = addFormations(testingAll, zones, depthCol, depthTopCol, depthBtmCol, 'IDWELL', threshold,
#                           'ZONECODE', 'ZONENAME')
#formations.to_csv(savedfilename, sep=',', encoding='utf-8')

testingAll.to_csv(savedfilename, sep=',', encoding='utf-8')