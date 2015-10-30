# -*- coding: utf-8 -*-
"""
Created on Wed Apr 29 16:06:07 2015

@author: bolaka
"""

import os
os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
import datetime
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV')
    
idCol = 'IDWELL'
file2 = 'nojv-string-merged-363.csv'
file1 = 'nojv-string-merged-420.csv'

data1 = pd.read_csv(file1,  index_col=idCol) 
data2 = pd.read_csv(file2,  index_col=idCol) 

IDs = set(data1.index.values)
ids = set(data2.index.values)
tossedIDs = IDs.difference(ids)

tossed = data1.loc[data1.index.isin(tossed)]
tossed.to_csv('tossed_wells.csv')