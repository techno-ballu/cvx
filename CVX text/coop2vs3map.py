# -*- coding: utf-8 -*-
"""
Created on Thu Apr 23 14:35:06 2015

@author: bolaka
"""

import os
os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/COOP')

def remove_duplicates(li):
    my_set = set()
    res = []
    for e in li:
        if e not in my_set:
            res.append(e)
            my_set.add(e)
    #
    return res

idCol = 'IDWELL'

stringfilename = 'coop-string1.csv'
latlogfilename = 'coop-latitudes.csv'

savedfile = 'coopheaders.csv'
strings = pd.read_csv(stringfilename,  index_col=idCol) 
latlongs = pd.read_csv(latlogfilename,  index_col=idCol) 

latlongs['string'] = float('nan')

wellshavingstring = strings.groupby(strings.index)
wellshavinglatlong = latlongs.groupby(latlongs.index)

for index, group in wellshavingstring:
    if index in wellshavinglatlong.groups:
        detail = wellshavinglatlong.get_group(index)
        print(index, group.string.values, detail.LATITUDE.values, detail.LONGITUDE.values)
        latlongs.loc[ index, 'string'] = group.string.values[0]

latlongs.to_csv(savedfile, sep=',', encoding='utf-8')

#d = latlongs
##d = d.loc[ (d.LONGITUDE < -100) & (d.LONGITUDE > -105) ]
#ax = d.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='string', s=d['WELL_CLASSIFICATION_LOST_MUD']*70 + 10, cmap='Blues',figsize=(12, 8)) #
#ax.set_xlim(-102.4, -102.2)
#ax.set_ylim(31.55, 31.8)

def plotWellsByGrp(groups):
    # Plot
    plt.rcParams.update(pd.tools.plotting.mpl_stylesheet)
    colors = pd.tools.plotting._get_standard_colors(len(groups), color_type='random')
    
    fig, ax = plt.subplots()
    fig.set_size_inches(11,8)
    ax.set_color_cycle(colors)
    ax.margins(0.05)
    for name, group in groups:
        ax.plot(group.LONGITUDE, group.LATITUDE, marker='o', linestyle='', ms=3, label=name)
    ax.legend(numpoints=1, loc='upper right')
    ax.set_xlim(-102.4, -101.8)
    ax.set_ylim(31.2, 31.8)
    plt.show()

groups = latlongs.groupby('Total Loss')
plotWellsByGrp(groups)