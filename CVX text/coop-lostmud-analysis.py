# -*- coding: utf-8 -*-
"""
Created on Fri Apr 24 15:47:02 2015

@author: bolaka
"""

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

setPath('/home/bolaka/CVX text/COOP')
    
idCol = 'IDWELL'
savedfilename = 'WITHID.csv'
stringfilename = 'coop-string1.csv'

strings = pd.read_csv(stringfilename,  index_col=idCol) 
formations = pd.read_csv(savedfilename,  index_col=idCol) 

depthCol = 'depth'
threshold = 200
metric1 = 'Circulation'

# separate by strings of wells
formations['string'] = float('nan')
wells = formations.groupby(formations.index)
strGrps = strings.groupby(strings.index)
for index, group in wells:
    if index in strGrps.groups:
        formations.loc[index, 'string'] = strings.loc[index, 'string']

formations.to_csv('coop_features_string.csv')
threes = formations.loc[ formations['string'] == 3.0 ]
twos = formations.loc[ formations['string'] == 2.0 ]

#data = twos
#filtered = prepareSeries1(data, 'depth', 'mud-wt', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'mud-wt', 0, 'all-lost-mud-2')

#filtered = prepareSeriesGeos(data, 'depth', 'mud-wt', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'mud-wt', 1, formationdepths, 'all-lost-mud-high-geo')

#
#data = lowSTD
#filtered = prepareSeriesGeos(data, 'depth', 'mud-wt', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'mud-wt', 1, formationdepths, 'all-lost-mud-low-geo')
#
#filtered = prepareSeriesGeos(data, 'depth', 'viscosity', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'viscosity', 2, formationdepths, 'all-lost-mud-low-geo')
#
#filtered = prepareSeriesGeos(data, 'depth', 'WOB', 'formation', metric1, True, 5)
#res = plotDensityMatrixGeos(filtered, 300, 'WOB', 10, formationdepths, 'all-lost-mud-low-geo')

#filtered = prepareSeries1(data, 'DEPTH', 'csg', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 500, 'csg', 0, 'all-lost-csg-high')

#filtered = prepareSeries1(data, 'depth', 'diameter', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'diameter', 0, 'all-lost-mud')

#filtered = prepareSeries1(data, 'depth', 'pump psi', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'pump psi', 200, 'all-lost-mud')

data = threes
filtered = prepareSeries1(data, 'depth', 'mud-wt', metric1, True, 5)
res = plotDensityMatrix1(filtered, 300, 'mud-wt', 0, 'all-lost-mud-3')

filtered = prepareSeries1(data, 'depth', 'viscosity', metric1, True, 3)
res = plotDensityMatrix1(filtered, 300, 'viscosity', 0, 'all-lost-mud-3')

filtered = prepareSeries1(data, 'depth', 'pH', metric1, True, 5)
res = plotDensityMatrix1(filtered, 300, 'pH', 0, 'all-lost-mud-3')

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

#data = formations
#filtered = prepareSeries1(data, 'depth', 'mud-wt', metric1, True, 5)
#res = plotDensityMatrix1(filtered, 300, 'mud-wt', 0, 'all-lost-mud')

## depth vs mud weight
#x0, y0 = prepareSeries(formations, 'depth', 'mud-wt', formations[metric1] == 0, True, 5)
#x1, y1 = prepareSeries(formations, 'depth', 'mud-wt', formations[metric1] == 1, True, 5)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-mudweight', [7.5,11], [0, 12000])
#

#
## depth vs viscosity
#x0, y0 = prepareSeries(formations, 'depth', 'viscosity', formations[metric1] == 0, False)
#x1, y1 = prepareSeries(formations, 'depth', 'viscosity', formations[metric1] == 1, False)
#multiseriesScatter(x0,y0,x1,y1,'No lost Mud','Lost Mud', 'depth-viscosity')
#


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
