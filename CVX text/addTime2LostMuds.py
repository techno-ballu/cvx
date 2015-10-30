# -*- coding: utf-8 -*-
"""
Created on Thu Apr 30 15:51:17 2015

@author: bolaka
"""

import os

os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV')
    
idCol = 'IDWELL'
filename = 'nojv-formations.csv'# 'sayantani-classified.csv' lostMudAllWellsClassified-new
data = pd.read_csv(filename) #, index_col=idCol

#data['lostmudlag'] = data['Circulation'].shift(-1)
#data['temp'] = 0
data['lostmudtimeline'] = 0

#wells = data.groupby([idCol]) #, 
#
#for index, group in wells:
#    ssum = 0
#
#    for i, row in group.iterrows():
#        ssum += row['lostmudlag'] # datanew
#        data.loc[i, 'temp'] = ssum

def shiftUp(grp):
    grp['lostmudtimeline'] = grp['Circulation'].cumsum()
#    grp['lostmudtimeline'] = grp['temp'].shift(1)
#    grp['lostmudtimeline'].iloc[0] = 0
#    print(grp['temp'].values, grp['lostmudtimeline'].values)
    return grp
   
datanew = data.groupby(idCol).apply(shiftUp)        
#datanew = datanew[[idCol, 'SUMMARYOPS', 'Circulation', 'lostmudtimeline']]
datanew.to_csv(filename)