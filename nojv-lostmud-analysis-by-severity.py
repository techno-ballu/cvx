# -*- coding: utf-8 -*-
"""
Created on Tue Apr 14 18:23:56 2015

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

formations = pd.read_csv(savedfilename,  index_col=idCol) 

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
metric3 = 'bbls lost'

filtered = prepareSeries1(formations, 'depth', 'mud-wt', metric3, True, 5)
res = plotDensityMatrix2(filtered, 300, 'mud-wt', 0)

filtered = prepareSeries1(formations, 'depth', 'viscosity', metric3, True, 5)
res = plotDensityMatrix2(filtered, 300, 'viscosity', 0)

#filtered = prepareSeries1(formations, 'depth', 'pH', metric3, True, 12)
#res = plotDensityMatrix2(filtered, 300, 'pH', 1)

#filtered = prepareSeries1(formations, 'depth', 'ROP', metric3, True, 5)
#res = plotDensityMatrix2(filtered, 500, 'ROP', 10)

filtered = prepareSeries1(formations, 'depth', 'diameter', metric3, True, 5)
res = plotDensityMatrix2(filtered, 300, 'diameter', 0)

filtered = prepareSeries1(formations, 'depth', 'WOB', metric3, True, 5)
res = plotDensityMatrix2(filtered, 300, 'WOB', 5)

filtered = prepareSeries1(formations, 'depth', 'pump psi', metric3, True, 5)
res = plotDensityMatrix2(filtered, 300, 'pump psi', 200)

#filtered = prepareSeries1(formations, 'depth', 'jet size 1', metric3, True, 5)
#res = plotDensityMatrix2(filtered, 300, 'jet size 1', 0)
#
#filtered = prepareSeries1(formations, 'depth', 'jet size 2', metric3, True, 5)
#res = plotDensityMatrix2(filtered, 300, 'jet size 2', 1)
