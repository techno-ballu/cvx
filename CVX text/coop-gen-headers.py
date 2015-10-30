# -*- coding: utf-8 -*-
"""
Created on Wed May  6 16:17:15 2015

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
depthTopCol = 'DEPTHTOP'
depthBtmCol = 'DEPTHBTM'

savedfilename = 'coop_features.csv'
headersfilename = 'well-headers.csv'
stringfile = 'coop-string1.csv'

nojvheadersfile = 'coop-headers.csv'
data = pd.read_csv(savedfilename,  index_col=idCol) 
headers = pd.read_csv(headersfilename,  index_col=idCol) 
stringinfo = pd.read_csv(stringfile, index_col=idCol)
twos = stringinfo.loc[stringinfo.string == 2]
threes = stringinfo.loc[stringinfo.string == 3]

## remove outliers
#data = data.loc[ data['LONGITUDE'] < 0 ]

#headers['TVD'] = float('nan')
headers['string'] = 0
headers['Lost Mud'] = 0
#headers['Total Loss'] = 0
#headers['viscosity STD'] = float('nan')
#headers['viscosity AVG'] = float('nan')
headers['mud weight STD'] = float('nan')
headers['mud weight AVG'] = float('nan')
#headers['pump psi STD'] = float('nan')
#headers['pump psi AVG'] = float('nan')
#headers['wob STD'] = float('nan')
#headers['wob AVG'] = float('nan')
#headers['ph STD'] = float('nan')
#headers['diameter STD'] = float('nan')
#headers['formation'] = None
#headers['total days'] = 0

#formationdepths = orderFormations(zones, 'ZONECODE', 'ZONENAME', depthTopCol, depthBtmCol)
#for zonename in formationdepths['zone'].values:
#    headers[zonename+'_top'] = None
#    headers[zonename+'_deep'] = None
#
#depths = jobs.groupby(jobs.index)
twoGrps = twos.groupby(twos.index) #'WellID'
threeGrps = threes.groupby(threes.index) #'WellID'
allD = data.groupby(data.index)
for index, group in allD:
    
    # add string classification
    string = None
    if index in twoGrps.groups:
        headers.loc[index, 'string'] = 2
    elif index in threeGrps.groups:
        headers.loc[index, 'string'] = 3
    
    if np.isnan(group['mud-wt'].std()) == False:
        headers.loc[index, 'mud weight STD'] = group['mud-wt'].std()
    if np.isnan(group['mud-wt'].mean()) == False:
        headers.loc[index, 'mud weight AVG'] = group['mud-wt'].mean()    
    
    if group['Circulation'].sum() > 0:
        headers.loc[index, 'Lost Mud'] = 1

headers['OPERATOR'] = 'Chevron'

## drop columns with many NaNs
#headers.dropna(axis=1, how='any', thresh=0.1 * len(headers), inplace=True)
#
#IDs = set(stringinfo.index.values)
#ids = set(data.index.values)
#tossed = IDs.difference(ids)
#headers.loc[headers.index.isin(tossed), 'string'] = 1
#
#
#f = lambda x: x.isnull().sum() / len(x)
#headers['missingness'] = headers.apply(f,1)

#headers['string'] = [int(x) for x in headers['string']]

twos = headers.loc[ headers['string'] == 2]
threes = headers.loc[ headers['string'] == 3]
#twos.to_csv('strings-training2.csv', sep=',', encoding='utf-8')
#threes.to_csv('strings-training3.csv', sep=',', encoding='utf-8')

headers.to_csv(nojvheadersfile, sep=',', encoding='utf-8')
print(headers['string'].value_counts())

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

groups = threes.groupby('Lost Mud')
plotWellsByGrp(groups)


#d = headers
#d = d.loc[ d['string'] > 1 ]
#
#ax = d.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='Total Loss', s=40, cmap='Blues',figsize=(12, 8)) #d['viscosity STD']*70 + 10
#ax.set_xlim(-102.4, -101.8)
#ax.set_ylim(31.2, 31.8)

#by = 'OPERATOR' # 'Lost Mud', 'AREA'
#plt.scatter(d.LONGITUDE, d.LATITUDE, c=d['string'], s=40)
##plt.xlim(-102.4, -101.8) 
##plt.ylim(31.2, 31.8)
#
##ax = d.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c=by + '_code', s=d['Lost Mud'] * 50 + 20, cmap='Blues',figsize=(15, 12))
##ax = d.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='Lost Mud', s=50, cmap='Blues',figsize=(15, 12))
##ax = d.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c= by + '_code', s=d['viscosity STD'] * 50 + 10, cmap='Blues',figsize=(15, 12))
#
#
#ax = d.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='string', s=d['Lost Mud'] * 70 + 10, cmap='Blues',figsize=(12, 8)) #
#ax.set_xlim(-102.4, -101.8)
#ax.set_ylim(31.2, 31.8)
#print('viscosity vs mud wt = ', headers['viscosity STD'].corr(headers['mud weight STD']))
#print('viscosity vs pump psi = ', headers['viscosity STD'].corr(headers['pump psi STD']))
#print('viscosity vs wob = ', headers['viscosity STD'].corr(headers['wob STD']))
#print('mud wt vs pump psi = ', headers['mud weight STD'].corr(headers['pump psi STD']))
#print('mud wt vs wob = ', headers['mud weight STD'].corr(headers['wob STD']))
#print('pump psi vs wob = ', headers['pump psi STD'].corr(headers['wob STD']))
#
#print('string vs viscosity = ', headers['string'].corr(headers['viscosity STD'])) # voila!
#print('string vs mud wt = ', headers['string'].corr(headers['mud weight STD']))
#print('string vs pump psi = ', headers['string'].corr(headers['pump psi STD']))
#print('string vs wob = ', headers['string'].corr(headers['wob STD']))