# -*- coding: utf-8 -*-
"""
Created on Sat Mar 21 17:36:01 2015

@author: bolaka
"""

# imports
import os
import pandas as pd
from cvxtextproject import *
from mlclassificationlibs import *

results = '/training sets/sets'
directory = '/home/bolaka/CVX text/' + results

if not os.path.exists(directory):
    os.makedirs(directory)

# setting work directory
os.chdir(directory)

idCol1 = 'WellID'
idCol2 = 'IDWELL'

nojv2filename = '../nojv2timelinesnew.csv'
coop2filename = '../coop2timelinesnew.csv'
nojv3filename = '../nojv3timelinesnew.csv'
coop3filename = '../coop3timelinesnew.csv'

rawNOJVfilename = '../nojv.csv'

trainedNOJV2 = pd.read_csv(nojv2filename,  index_col=idCol1) 
trainedNOJV3 = pd.read_csv(nojv3filename,  index_col=idCol1) 
trainedCOOP2 = pd.read_csv(coop2filename,  index_col=idCol1) 
trainedCOOP3 = pd.read_csv(coop3filename,  index_col=idCol1) 

# drop rows with missing
trainedNOJV2 = trainedNOJV2.dropna()
trainedNOJV3 = trainedNOJV3.dropna()
trainedCOOP2 = trainedCOOP2.dropna()
trainedCOOP3 = trainedCOOP3.dropna()

rawNOJV = pd.read_csv(rawNOJVfilename,  index_col=idCol2) 
groupedRawNOJV = rawNOJV.groupby(rawNOJV.index)

#groupedTrained = trainedNOJV2.groupby(trainedNOJV2.index)

def prepareTraining(raw, manual2, manual3, groups):
    
    twostring = ['SURFACE DRILLING','SURFACE CASING','PRODUCTION DRILLING','PRODUCTION CASING']
    threestring = ['SURFACE DRILLING','SURFACE CASING','INTERMEDIATE DRILLING','INTERMEDIATE CASING',
                 'PRODUCTION DRILLING','PRODUCTION CASING']

    columns = list(names(raw))
    columns.append('Code 1')
    columns.append('Code 2')
    trained = pd.DataFrame(data=np.zeros((0,len(columns))), columns=columns)
    
    # 2-string
    print('2-string')
    for index, row in manual2.iterrows(): #
        days = row[['Surf D days', 'Surf C days', 'Prod D days', 'Prod C days']].values
        drillTimeTotal = sum(days)
        
        if index in groups.groups:
            group = groups.get_group(index)
            print(index, 'rows=', group['DAYSFROMSPUDCALC'].count(), 'manual=', drillTimeTotal)
            if group['DAYSFROMSPUDCALC'].count() == int(drillTimeTotal):
                
                strings1 = []    
                strings2 = []  
                for i, d in enumerate(days):
                    split = twostring[i].split(' ')
                    count = int(d)
                    while count > 0:
                        strings1.append(split[0])
                        strings2.append(split[1])
                        count -= 1
                group['Code 1'] = strings1
                group['Code 2'] = strings2
                trained = trained.append(group)
            
    
    # 3-string
    print('3-string')
    for index, row in manual3.iterrows(): #
        days = row[['Surf D days', 'Surf C days','Inter D days','Inter C days', 
                    'Prod D days', 'Prod C days']].values
        drillTimeTotal = sum(days)
        
        if index in groups.groups:
            group = groups.get_group(index)
            print(index, 'rows=', group['DAYSFROMSPUDCALC'].count(), 'manual=', drillTimeTotal)
            if group['DAYSFROMSPUDCALC'].count() == int(drillTimeTotal):
                
                strings1 = []    
                strings2 = []      
                for i, d in enumerate(days):
                    split = threestring[i].split(' ')
                    count = int(d)
                    while count > 0:
                        strings1.append(split[0])
                        strings2.append(split[1])
                        count -= 1
                group['Code 1'] = strings1
                group['Code 2'] = strings2
                trained = trained.append(group)
    
    return (trained)

#trainedNOJV = prepareTraining(rawNOJV,trainedNOJV2, trainedNOJV3,groupedRawNOJV)
#
#trainingfilename = '../trainv7.csv'
#train = pd.read_csv(trainingfilename,  index_col=idCol) 
#IDs = set(train.index.values)
#ids = set(trainedNOJV.index.values)
#common = IDs & ids
#uniqueInToTrain = IDs.difference(ids)
#train = train[train.index.isin(uniqueInToTrain)]
#
#trainedNOJV = trainedNOJV.append(train)
#trainedNOJV.to_csv('trained-nojv.csv', sep=',', encoding='utf-8')

rawCOOPfilename = '../coop.csv'
rawCOOP = pd.read_csv(rawCOOPfilename,  index_col=idCol2) 
groupedRawCOOP = rawCOOP.groupby(rawCOOP.index)

## process the raw data to remove the completions
#rawWithoutCompletionsCOOP = pd.DataFrame(data=np.zeros((0,len(columns))), columns=names(columns))
#groupedRawCOOP['RIGSCALC'].apply(lambda x: x.unique()[:2])
#
#for key, group in groupedRawCOOP:
#    print(key, group['RIGSCALC'].unique()[:2])

trainedCOOP = prepareTraining(rawCOOP,trainedCOOP2, trainedCOOP3,groupedRawCOOP)

trainingfilename = '../coop-training-final.csv'
train = pd.read_csv(trainingfilename,  index_col=idCol) 
IDs = set(train.index.values)
ids = set(trainedCOOP.index.values)
common = IDs & ids
uniqueInToTrain = IDs.difference(ids)
train = train[train.index.isin(uniqueInToTrain)]

trainedCOOP = trainedCOOP.append(train)
trainedCOOP.to_csv('trained-coop.csv', sep=',', encoding='utf-8')

rows(trainedNOJV)
rows(trainedCOOP)