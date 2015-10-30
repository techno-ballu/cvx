# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 15:05:17 2015

@author: bolaka
"""

import os
os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from cvxtextproject import *
from mlclassificationlibs import *
from datetime import datetime

setPath('/home/bolaka/CVX text/NOJV')

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

viscositySTDCutoff = 2 # debatable!
daysfilename = 'nojv.csv'
savedfilename = 'nojv-formations.csv'
#zonesfilename = 'zones.csv'
headersfilename = 'wellheaders.csv'
twostringfile = 'nojv2timelinesnew.csv'
threestringfile = 'nojv3timelinesnew.csv'
stringfile = 'nojv-string-merged-420.csv'

nojvheadersfile = 'nojvheaders.csv'
daysD = pd.read_csv(daysfilename, index_col=idCol)
data = pd.read_csv(savedfilename,  index_col=idCol) 
headers = pd.read_csv(headersfilename,  index_col=idCol) 
#zones = pd.read_csv(zonesfilename) #,index_col=idCol
stringinfo = pd.read_csv(stringfile, index_col=idCol)
twos = stringinfo.loc[stringinfo.string == 2]
threes = stringinfo.loc[stringinfo.string == 3]

# extract the TVDs for each well from wvjob
jobfilename = 'wvjob.csv'
jobs = pd.read_csv(jobfilename,  index_col=idCol) 
jobs = jobs[jobs['JOBTYP'].isin(['Drilling', 'Drill and Complete'])]

jobs = jobs[['TOTALDEPTHCALC']]

#print('zones set has', rows(zones))
#
## remove the irrelevant values from zones
#zones = zones.loc[ np.isnan(zones[depthTopCol]) == False ]
#zones = zones.loc[ np.isnan(zones[depthBtmCol]) == False ] 
#print('zones set has', rows(zones))

# remove outliers
#data = data.loc[ data['LONGITUDE'] < 0 ]

headers['DTTMEND'] = float('nan')
headers['TVD'] = float('nan')
headers['string'] = 0
headers['Lost Mud'] = 0
headers['Total Loss'] = 0
headers['viscosity STD'] = float('nan')
headers['viscosity AVG'] = float('nan')
headers['mud weight STD'] = float('nan')
headers['mud weight AVG'] = float('nan')
headers['pump psi STD'] = float('nan')
headers['pump psi AVG'] = float('nan')
headers['wob STD'] = float('nan')
headers['wob AVG'] = float('nan')
headers['ph STD'] = float('nan')
headers['diameter STD'] = float('nan')
#headers['formation'] = None
headers['total days'] = 0

#formationdepths = orderFormations(zones, 'ZONECODE', 'ZONENAME', depthTopCol, depthBtmCol)
#for zonename in formationdepths['zone'].values:
#    headers[zonename+'_top'] = None
#    headers[zonename+'_deep'] = None

depths = jobs.groupby(jobs.index)
twoGrps = twos.groupby(twos.index) #'WellID'
threeGrps = threes.groupby(threes.index) #'WellID'
#zoneGrps = zones.groupby([idCol])
allD = data.groupby(data.index)
daysGrps = daysD.groupby(daysD.index)
for index, group in allD:
    
    # add total days from SPUD
    if index in daysGrps.groups:
        well = daysGrps.get_group(index)
        days = well['RIGDAYSCALC'].max()
        endDatetimeinmillis = well.tail(1)['DTTMEND'].values[0]
        endDate = datetime.fromtimestamp( int(endDatetimeinmillis)/1000 ).strftime('%Y-%m-%d')
#        print(index, endDatetimeinmillis, endDate)
        headers.loc[index, 'DTTMEND'] = endDate
        headers.loc[index, 'total days'] = days    
    
    # add TVD
    if index in depths.groups:
        depth1 = depths.get_group(index)['TOTALDEPTHCALC'].values[0]
    depth2 = group['depth'].max()
    depth = max(depth1, depth2)
    
    headers.loc[index, 'TVD'] = depth
        
#    # add formations comma separated
#    forms_uni = remove_duplicates(group['formation'].values)
#    headers.loc[index, 'formation'] = ','.join(forms_uni).replace('before formations start,','').replace(',beyond 200 ft of formations','')
    
    # add string classification
    string = None
    if index in twoGrps.groups:
        headers.loc[index, 'string'] = 2
    elif index in threeGrps.groups:
        headers.loc[index, 'string'] = 3
    
    if np.isnan(group['viscosity'].std()) == False:
        headers.loc[index, 'viscosity STD'] = group['viscosity'].std()
    if np.isnan(group['mud-wt'].std()) == False:
        headers.loc[index, 'mud weight STD'] = group['mud-wt'].std()
    if np.isnan(group['pump psi'].std()) == False:
        headers.loc[index, 'pump psi STD'] = group['pump psi'].std()
    if np.isnan(group['WOB'].std()) == False:
        headers.loc[index, 'wob STD'] = group['WOB'].std()
    if np.isnan(group['pH'].std()) == False:
        headers.loc[index, 'ph STD'] = group['pH'].std()
    if np.isnan(group['diameter'].std()) == False:
        headers.loc[index, 'diameter STD'] = group['diameter'].std()
    
    if np.isnan(group['viscosity'].mean()) == False:
        headers.loc[index, 'viscosity AVG'] = group['viscosity'].mean()
    if np.isnan(group['mud-wt'].mean()) == False:
        headers.loc[index, 'mud weight AVG'] = group['mud-wt'].mean()    
    if np.isnan(group['pump psi'].mean()) == False:
        headers.loc[index, 'pump psi AVG'] = group['pump psi'].mean()
    if np.isnan(group['WOB'].mean()) == False:
        headers.loc[index, 'wob AVG'] = group['WOB'].mean()  
    
    if group['Circulation'].sum() > 0:
        headers.loc[index, 'Lost Mud'] = 1
    if group['Total Loss'].sum() > 0:
        headers.loc[index, 'Total Loss'] = 1
    
#    if index in zoneGrps.groups:
#        zoneinfo = zoneGrps.get_group(index)
#            
#        # group by depthtop & depthbtm to remove duplicates
#        layers = zoneinfo.groupby([depthTopCol, depthBtmCol], sort=False)        
#        
#        for (top, btm), layer in layers:
#            name = set(layer['ZONECODE'].values).pop()
#            headers.loc[index, name+'_top'] = layer['DEPTHTOP'].mean()
#            headers.loc[index, name+'_deep'] = layer['DEPTHTOPTOBTMCALC'].mean()

headers['COUNTY_code'] = pd.Categorical.from_array(headers['COUNTY']).labels
headers['DISTRICT_code'] = pd.Categorical.from_array(headers['DISTRICT']).labels
headers['FIELDNAME_code'] = pd.Categorical.from_array(headers['FIELDNAME']).labels
headers['LEASE_code'] = pd.Categorical.from_array(headers['LEASE']).labels
headers['OPERATOR_code'] = pd.Categorical.from_array(headers['OPERATOR']).labels

# drop columns with many NaNs
headers.dropna(axis=1, how='any', thresh=0.1 * len(headers), inplace=True)

IDs = set(stringinfo.index.values)
ids = set(data.index.values)
tossed = IDs.difference(ids)
headers.loc[headers.index.isin(tossed), 'string'] = 1


f = lambda x: x.isnull().sum() / len(x)
headers['missingness'] = headers.apply(f,1)


#headers['string'] = [int(x) for x in headers['string']]
headers.to_csv(nojvheadersfile, sep=',', encoding='utf-8')
print(headers['string'].value_counts())


d = headers
d = d.loc[ d['string'] > 1 ]

ax = d.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='Total Loss', s=40, cmap='Blues',figsize=(12, 8)) #d['viscosity STD']*70 + 10
ax.set_xlim(-102.4, -101.8)
ax.set_ylim(31.2, 31.8)
#plot = d.boxplot(column='TVD',by='string',figsize=(12, 8), sym='r+') #color=color,kind='box',

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