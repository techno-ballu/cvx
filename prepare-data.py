# -*- coding: utf-8 -*-
"""
Created on Wed Mar 18 14:10:57 2015

@author: bolaka
"""

# imports
import os
import pandas as pd
import numpy as np

# extract the names of the pandas data frame
def names(pandas_dataframe):
    return (pandas_dataframe.columns.values.tolist())

results = '/ML/16March'

directory = '/home/bolaka/CVX text/' + results

if not os.path.exists(directory):
    os.makedirs(directory)

# setting work directory
os.chdir(directory)
idCol = 'IDWELL'

nojvfilename = '../nojv.csv'
totrainfilename = '../trainv9.csv'
trainingfilename = '../trainv7.csv'
train = pd.read_csv(trainingfilename,  index_col=idCol) 

nojv = pd.read_csv(nojvfilename, index_col=idCol) #
selectCols = ['DAYSFROMSPUDCALC','DTTMEND','DTTMSTART','RIGSCALC','RIGDAYSCALC','SUMMARYOPS']
nojv = nojv[selectCols]
# drop duplicate wells
nojv[idCol] = nojv.index
nojv = nojv.sort(['IDWELL', 'RIGDAYSCALC'], ascending=[True, True])
del nojv[idCol]

# remove the completions rows and save copy in file
nojv = nojv[np.isfinite(nojv['RIGDAYSCALC'])]
#nojv.to_csv('nojv-only-drilling.csv', sep=',', encoding='utf-8')
#
## wells info
wellIDs = set(nojv.index.values)
numberOfWells = len(wellIDs)
#
## extract the TVDs for each well from wvjob
jobfilename = '../wvjob.csv'
jobs = pd.read_csv(jobfilename,  index_col=idCol) 
jobs = jobs[jobs['JOBTYP'].isin(['Drilling', 'Drill and Complete'])]
#
## 
jobs = jobs.loc[wellIDs,:]
jobsIDs = set(jobs.index.values)
jobsCount = len(jobsIDs)

# check wells count
jobsCount == numberOfWells

jobs = jobs.loc[jobsIDs,:]
jobs = jobs[['TOTALDEPTHCALC','DURATIONSPUDTOTDCALC']]
#jobs.to_csv('nojv-tvd.csv', sep=',', encoding='utf-8')

tvds = []
#duration = []
for index, row in nojv.iterrows():
    tvds.append(jobs.loc[index]['TOTALDEPTHCALC'])
#    duration.append(jobs.loc[index]['DURATIONSPUDTOTDCALC'])
    
nojv['JOBTVD'] = tvds
#nojv['DURATIONSPUDTOTDCALC'] = duration
nojv.to_csv('nojv.csv', sep=',', encoding='utf-8')
toTrain = pd.read_excel('../2tra.xlsx', sheetname=0, index_col=idCol)
# rename the columns and save the data
mappings = {
    'StringFromCode' : 'Code 1',
    'PhaseFromCode' : 'Code 2'
};
toTrain = toTrain.rename(columns=mappings)
colorder = ['DAYSFROMSPUDCALC',
 'DTTMEND',
 'DTTMSTART',
 'RIGSCALC',
 'RIGDAYSCALC',
 'SUMMARYOPS',
 'Code 1',
 'Code 2',
 'TVD']
toTrain = toTrain[colorder]
toTrain.to_csv('train-2-string.csv', sep=',', encoding='utf-8')

#toTrain = pd.read_csv(totrainfilename, index_col=idCol)
IDs = set(toTrain.index.values)
ids = set(train.index.values)
common = IDs & ids
uniqueInToTrain = IDs.difference(ids)
#for ID in uniqueInToTrain:
#    print(toTrain.loc[ID]) # nojv.loc[ID][['RIGDAYSCALC','DTTMEND']]
    
toTrain = toTrain[toTrain.index.isin(uniqueInToTrain)]
toTrain.to_csv('trainv9.csv', sep=',', encoding='utf-8')


filename = '../updatedtraining.csv'
data = pd.read_csv(filename, index_col='ID') #
print('count of wells', len(set(data.index)))

data['strings'] = data['StringFromCode'] + ' ' + data['PhaseFromCode']
trainWells = data.groupby(data.index)['strings'].apply(lambda x: len(x.unique()))
trainWells.value_counts()

IDs = set(nojv.index.values)
ids = set(data.index.values)
common = IDs & ids
uniqueInToTrain = IDs.difference(ids)

nojvSansTVDs = nojv[nojv.index.isin(uniqueInToTrain)]
nojvSansTVDs.to_csv('nojv-sans-tvds.csv', sep=',', encoding='utf-8')