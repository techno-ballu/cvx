# -*- coding: utf-8 -*-
"""
Created on Fri Apr 10 12:38:38 2015

@author: bolaka
"""

import os

os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV')
    
idCol = 'IDWELL'
savedfilename = 'nojv-formations.csv'
#zonesfilename = 'zones.csv'

formations = pd.read_csv(savedfilename,  index_col=idCol) 
#zones = pd.read_csv(zonesfilename) #,index_col=idCol

viscositySTDCutoff = 2 # debatable!
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

#print('zones set has', rows(zones))
#
## remove the irrelevant values from zones
#zones = zones.loc[ np.isnan(zones[depthTopCol]) == False ]
#zones = zones.loc[ np.isnan(zones[depthBtmCol]) == False ] 
#print('zones set has', rows(zones))
#
#formationdepths = orderFormations(zones, 'ZONECODE', 'ZONENAME', depthTopCol, depthBtmCol)
#print(formationdepths)

## separate high viscosity cases
#formations['high STD in viscosity'] = None
#
#allD = formations.groupby(formations.index)
#for index, group in allD:
##    print(index, group['viscosity'].std())
#    if group['viscosity'].std() > viscositySTDCutoff:
#        formations.loc[index, 'high STD in viscosity'] = 1
#    elif group['viscosity'].std() > 0:
#        formations.loc[index, 'high STD in viscosity'] = 0
#
#formations.to_csv(savedfilename, sep=',', encoding='utf-8')
#
#highSTD = formations.loc[ formations['high STD in viscosity'] == 1 ]
#lowSTD = formations.loc[ formations['high STD in viscosity'] == 0 ]

#stringfile = 'strings-classified.csv'
#stringinfo = pd.read_csv(stringfile)
#twos = stringinfo.loc[stringinfo.string_pred == 2]
#threes = stringinfo.loc[stringinfo.string_pred == 3]

# add the strings
stringfile = 'nojv-string-classified-672.csv'
stringinfo = pd.read_csv(stringfile)
twos = stringinfo.loc[stringinfo.string == 2]
threes = stringinfo.loc[stringinfo.string == 3]

formations['string'] = float('nan')
twoGrps = twos.groupby([idCol]) 
threeGrps = threes.groupby([idCol]) 
wells = formations.groupby(formations.index)

for index, group in wells:
    if index in twoGrps.groups:
        formations.loc[index, 'string'] = 2
    elif index in threeGrps.groups:
        formations.loc[index, 'string'] = 3

formations.to_csv('nojv_features_string.csv')

# separate 2-string & 3-string wells
twos = formations.loc[ formations['string'] == 2 ] # ~ high
threes = formations.loc[ formations['string'] == 3 ] # ~ low


data = twos
#filtered = prepareSeries1(data, 'depth', 'mud-wt', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'mud-wt', 0, 'all-lost-mud-2')

#filtered = prepareSeries1(data, 'depth', 'viscosity', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'viscosity', 0, 'all-lost-viscosity')

#filtered = prepareSeries1(data, 'depth', 'WOB', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'WOB', 5, 'all-lost-wob')

#filtered = prepareSeries1(data, 'depth', 'pump psi', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'pump psi', 200, 'all-lost-psi')

filtered = prepareSeries1(data, 'depth', 'diameter', metric1, True, 5)
res = plotDensityMatrix1(filtered, 300, 'diameter', 0, 'all-lost-dia-2')

#filtered = prepareSeriesGeos(data, 'depth', 'mud-wt', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'mud-wt', 1, formationdepths, 'all-lost-mud-high-geo')
#
#filtered = prepareSeriesGeos(data, 'depth', 'viscosity', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'viscosity', 5, formationdepths, 'all-lost-mud-high-geo')
#
#filtered = prepareSeriesGeos(data, 'depth', 'WOB', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'WOB', 10, formationdepths, 'all-lost-mud-high-geo')
#
#d = threes
#before = d.loc[d.lostmudtimeline == 0]
#after = d.loc[d.lostmudtimeline > 0]
#data = before

#filtered = prepareSeriesGeos(data, 'depth', 'mud-wt', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'mud-wt', 1, formationdepths, 'all-lost-mud-low-geo')
#
#filtered = prepareSeriesGeos(data, 'depth', 'viscosity', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'viscosity', 2, formationdepths, 'all-lost-mud-low-geo')
#
#filtered = prepareSeriesGeos(data, 'depth', 'WOB', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'WOB', 10, formationdepths, 'all-lost-mud-low-geo')

#filtered = prepareSeries1(data, 'depth', 'mud-wt', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'mud-wt', 0, 'all-lost-mud-3')

#filtered = prepareSeries1(data, 'depth', 'viscosity', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'viscosity', 2, 'all-lost-viscosity-3')
#
#filtered = prepareSeries1(data, 'depth', 'WOB', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'WOB', 10, 'all-lost-mud-3')

#filtered = prepareSeries1(data, 'depth', 'pump psi', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'pump psi', 200, 'all-lost-psi-3')

data = threes
#filtered = prepareSeries1(data, 'depth', 'WOB', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'WOB', 10, 'all-lost-mud-3')

#filtered = prepareSeries1(data, 'depth', 'mud-wt', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'mud-wt', 0, 'all-lost-mud-3')

#filtered = prepareSeries1(data, 'depth', 'viscosity', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'viscosity', 2, 'all-lost-viscosity-3')

#filtered = prepareSeries1(data, 'depth', 'pump psi', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'pump psi', 200, 'all-lost-psi-3')

filtered = prepareSeries1(data, 'depth', 'diameter', metric1, True, 5)
res = plotDensityMatrix1(filtered, 300, 'diameter', 0, 'all-lost-dia-3')

#filtered = prepareSeries1(data, 'depth', 'CASING', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 600, 'CASING', 0, 'all-lost-csg-low')

#filtered = prepareSeries1(data, 'depth', 'diameter', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'diameter', 0, 'all-lost-mud')

#filtered = prepareSeries1(data, 'depth', 'pump psi', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'pump psi', 200, 'all-lost-mud')



#filtered = prepareSeries1(data, 'depth', 'mud-wt', metric3, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'mud-wt', 0, 'lost-mud')
#
#filtered = prepareSeries1(data, 'depth', 'viscosity', metric3, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'viscosity', 0, 'lost-mud')
#
#filtered = prepareSeries1(data, 'depth', 'diameter', metric3, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'diameter', 0, 'lost-mud')
#
#filtered = prepareSeries1(data, 'depth', 'WOB', metric3, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'WOB', 5, 'lost-mud')
#
#filtered = prepareSeries1(data, 'depth', 'pump psi', metric3, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'pump psi', 200, 'lost-mud')
#
#
#
#filtered = prepareSeries1(data, 'depth', 'mud-wt', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'mud-wt', 0, 'lost-circ')
#
#filtered = prepareSeries1(data, 'depth', 'viscosity', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'viscosity', 0, 'lost-circ')
#
#filtered = prepareSeries1(data, 'depth', 'diameter', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'diameter', 0, 'lost-circ')
#
#filtered = prepareSeries1(data, 'depth', 'WOB', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'WOB', 5, 'lost-circ')
#
#filtered = prepareSeries1(data, 'depth', 'pump psi', metric2, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'pump psi', 200, 'lost-circ')

## depth vs mud weight
#x0, y0 = prepareSeries(formations, 'depth', 'mud-wt', formations[metric1] == 0, False, 5)
#x1, y1 = prepareSeries(formations, 'depth', 'mud-wt', formations[metric1] == 1, False, 5)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-mudweight', [7.5,11], [0, 12000])
#
#
## depth vs viscosity
#x0, y0 = prepareSeries(formations, 'depth', 'viscosity', formations[metric1] == 0, False, 5)
#x1, y1 = prepareSeries(formations, 'depth', 'viscosity', formations[metric1] == 1, False, 5)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-viscosity', [25,60], [0, 12000])



# depth vs pH
#x0, y0 = prepareSeries(formations, 'depth', 'pH', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'pH', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-pH')

#filtered = prepareSeries1(formations, 'depth', 'pH', metric1, True, 12)
#res = plotDensityMatrix1(filtered, 300, 'pH', 1)

## depth vs ROP
#x0, y0 = prepareSeries(formations, 'depth', 'ROP', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'ROP', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-ROP')
#
#filtered = prepareSeries1(formations, 'depth', 'ROP', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'ROP', 10)
#
## depth vs diameter
#x0, y0 = prepareSeries(formations, 'depth', 'diameter', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'diameter', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-diameter')
#


#
## depth vs WOB
#x0, y0 = prepareSeries(formations, 'depth', 'WOB', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'WOB', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-WOB')
#

#
## depth vs pump psi
#x0, y0 = prepareSeries(formations, 'depth', 'pump psi', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'pump psi', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-pump psi')
#

#
## depth vs jet size 1
#x0, y0 = prepareSeries(formations, 'depth', 'jet size 1', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'jet size 1', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-jet size 1')
#
#filtered = prepareSeries1(formations, 'depth', 'jet size 1', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'jet size 1', 0)
#
## depth vs jet size 2
#x0, y0 = prepareSeries(formations, 'depth', 'jet size 2', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'jet size 2', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-jet size 2')
#
#filtered = prepareSeries1(formations, 'depth', 'jet size 2', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'jet size 2', 1)
