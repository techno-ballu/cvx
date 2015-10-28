# -*- coding: utf-8 -*-
"""
Created on Tue Mar 31 15:06:23 2015

@author: bolaka
"""

import os
os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
import datetime
#import mpld3
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV')
    
idCol = 'IDWELL'
#testingAllfilename = 'joe-classified.csv'
testingAllfilename = 'sayantani-classified.csv'# 
zonesfilename = 'zones.csv'
savedfilename = 'nojv-formations-all.csv'

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
metric1 = 'Lost Circulation'
metric2 = 'SEVERITY'

featuresToSave = ['Date', 'SUMMARYOPS', 'Rig', metric1 ] #
testingAll = extractMudWtFeatures(testingAll, 'mudwt-features.csv', featuresToSave, 'NOJV')

testingAll[depthCol] = [float(x) for x in testingAll[depthCol]]
testingAll[mudwtCol] = [float(x) for x in testingAll[mudwtCol]]
testingAll[visCol] = [float(x) for x in testingAll[visCol]]
testingAll[phCol] = [float(x) for x in testingAll[phCol]]
#testingAll[ropCol] = [float(x) for x in testingAll[ropCol]]

# filter only rows that have tvds
testingAll = testingAll.loc[ testingAll[depthCol] > 0 ]

print('testingAll set has', rows(testingAll))
#print('zones set has', rows(zones))

# remove the irrelevant values from zones
zones = zones.loc[ np.isnan(zones[depthTopCol]) == False ]
zones = zones.loc[ np.isnan(zones[depthBtmCol]) == False ] 
#print('zones set has', rows(zones))

formations = addFormations(testingAll, zones, depthCol, depthTopCol, depthBtmCol, 'IDWELL', threshold,
                           'ZONECODE')
        
formations.to_csv(savedfilename, sep=',', encoding='utf-8')

##bagOfWords = [ 'LCM', 'PILLS' ]
##extractFeatures(testingAll, 'testing-fuzzy-features.csv',bagOfWords)
##testingAll.to_csv('nojv-formations.csv', sep=',', encoding='utf-8')
#testingAll[depthCol] = [float(x) for x in testingAll[depthCol]]
#testingAll = testingAll.loc[ testingAll[depthCol] > 0 ]
##testingAll = testingAll.loc[ testingAll[mudwtCol] != 'NA' ] 
#print('testingAll set has', rows(testingAll))
#print('zones set has', rows(zones))
#
## remove the irrelevant values from zones
#zones = zones.loc[ np.isnan(zones[depthTopCol]) == False ]
#zones = zones.loc[ np.isnan(zones[depthBtmCol]) == False ] 
#print('zones set has', rows(zones))
#
#formations = addFormations(testingAll, zones, depthCol, depthTopCol, depthBtmCol, 'IDWELL', threshold,
#                           'ZONECODE')
#severe = formations.loc[formations[metric2] == 1]['formation']
#print(severe.value_counts())
#
#verysevere = formations.loc[formations[metric2] == 2]['formation']
#print(verysevere.value_counts())
#        
##formations[mudwtCol] = [float(x) for x in formations[mudwtCol]]
##formations[depthCol] = [-1 * float(x) for x in formations[depthCol]]
##a = formations[mudwtCol]
##bins = np.percentile(a, [0, 33.3, 66.7])
##formations[mudwtCol] = np.digitize(a, bins)
##print(bins)
##print(formations[mudwtCol].value_counts())
##formations['formation_code'] = [-1 * float(x) for x in formations['formation_code']]
#formations.to_csv('nojv-formations.csv', sep=',', encoding='utf-8')
#
## plot a multi-series scatter plot
#x0 = formations.loc[formations[metric1] == 0]['depth'].values
#y0 = formations.loc[formations[metric1] == 0]['mud-wt'].values
#x1 = formations.loc[formations[metric1] == 1]['depth'].values
#y1 = formations.loc[formations[metric1] == 1]['mud-wt'].values
#
#fig = plt.figure()
#ax1 = fig.add_subplot(111)
#
#scatter0 = ax1.scatter(x0, y0, s=10, c='b', marker=".", label='No lost Mud') #
#scatter1 = ax1.scatter(x1, y1, c='r', marker="o", label='Lost Mud') #s=10, 
#plt.legend(loc='upper left');
#plt.show()
##N = 100
##labels = ['point {0}'.format(i + 1) for i in range(N)]
##tooltip = mpld3.plugins.PointLabelTooltip(scatter0, labels=labels)
##mpld3.plugins.connect(fig, tooltip)
##
##mpld3.display()