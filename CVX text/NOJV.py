# -*- coding: utf-8 -*-
"""
Created on Mon Mar 23 12:06:23 2015

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
trainingfilename = 'trained-nojv.csv'
testingfilename = 'nojv-verified.csv'
testingAllfilename = 'nojv.csv'

training = pd.read_csv(trainingfilename,  index_col=idCol) 
testing = pd.read_csv(testingfilename,  index_col=idCol) 
testingAll = pd.read_csv(testingAllfilename,  index_col=idCol) 
nojv2Manual = pd.read_csv('nojv2timelinesnew.csv',  index_col='WellID') 
nojv3Manual = pd.read_csv('nojv3timelinesnew.csv',  index_col='WellID') 

print('training set has', rows(training))
print('testing set has', rows(testing))
print('testingAll set has', rows(testingAll))
print('nojv2Manual set has', rows(nojv2Manual))
print('nojv3Manual set has', rows(nojv3Manual))

# testing word cloud generator
#wordsFreq = drawWordCloud(testingAll['SUMMARYOPS']) # SUMMARYOPS
#wordsFreq.to_csv('words-frequency.csv', sep=',', encoding='utf-8')

IDs = set(testingAll.index.values)

# remove verified wells
ids = set(testing.index.values)
uniqueInToTrain = IDs.difference(ids)
print('Removing verified wells from all will leave ', len(uniqueInToTrain), 'wells')
testingAll = testingAll[testingAll.index.isin(uniqueInToTrain)]
IDs = set(testingAll.index.values)

# remove 2-string manual wells
ids = set(nojv2Manual.index.values)
uniqueInToTrain = IDs.difference(ids)
print('Removing 2-string manual wells from all will leave ', len(uniqueInToTrain), 'wells')
testingAll = testingAll[testingAll.index.isin(uniqueInToTrain)]
IDs = set(testingAll.index.values)

# remove 3-string manual wells
ids = set(nojv3Manual.index.values)
uniqueInToTrain = IDs.difference(ids)
print('Removing 3-string manual wells from all will leave ', len(uniqueInToTrain), 'wells')
testingAll = testingAll[testingAll.index.isin(uniqueInToTrain)] # 1099
IDs = set(testingAll.index.values)

# normalize 
training = normalizeColumns(training, ['DAYSFROMSPUDCALC'])
testing = normalizeColumns(testing, ['DAYSFROMSPUDCALC'])
testingAll = normalizeColumns(testingAll, ['DAYSFROMSPUDCALC'])

#print('testingAll set has', rows(testingAll))
#extractRegexFeatures(training, 'training-re-features.csv')

# convert the dttmend & dttmstart columns
#testingAll['DTTMEND'] = [datetime.datetime.fromtimestamp( int(x)/1000 ).strftime('%Y-%m-%d') for x in testingAll['DTTMEND']]
#testingAll['DTTMSTART'] = [datetime.datetime.fromtimestamp( int(x)/1000 ).strftime('%Y-%m-%d') for x in testingAll['DTTMSTART']]

#extractRegexFeatures(testingAll, 'testing-re-features.csv')

# add text extracted features
#bagOfWords = [ 'SPUD','MIRU','RU', 'R/U','RIG UP', 'TOH', 'TOOH', 'TRIP OUT', 'DRILLING', 
#              'DRLG', 'WASHING TO BOTTOM', 'TIH', 'TRIP IN',
#              'RIG RELEASE','NIPPLE', 'NU', 'NUBOP', 'ND',
#              'RIG DOWN', 'RD', 'R/D', 'CASING', 'CSG', 'CREW', 'WOC', 'LAYING DOWN', 
#              'WAIT ON CEMENT', 'BOP', 'WELL HEAD', 
#              'FISHING', 'PRODUCTION','N/U','NIPPLE DOWN', 'N/D','NDBOP', 'PROD',
#              'SURFACE', 'SRF', 'INTERMEDIATE']  # 'CEMENT', 'CMT','LDDP', 'LD DP', '13-3/8 CSG', '8-5/8 CSG', '5-1/2 CSG', '9-5/8 CSG'

# add text extracted features
bagOfWords = [ 'SPUD', 'PRODUCTION', 'INTERMEDIATE','SURFACE','RIG RELEASE','RIG DOWN',
               'RUN CSG','RUN 13-3/8" CASING', 'LDDP', 'LAYING DOWN DRILL PIPE',
               'RUN 8-5/8" CASING','RUN 5-1/2" CASING','CEMENTING','WELL HEAD' ] # 'PROD','DRILLING', 'DRLG',
# 
extractFeatures(training, 'training-features.csv',bagOfWords)
extractFeatures(testing, 'testing-features.csv',bagOfWords)
extractFeatures(testingAll, 'testing-all-features.csv',bagOfWords)

training['RIGSCALC_code'] = pd.Categorical.from_array(training['RIGSCALC']).labels
testing['RIGSCALC_code'] = pd.Categorical.from_array(testing['RIGSCALC']).labels
testingAll['RIGSCALC_code'] = pd.Categorical.from_array(testingAll['RIGSCALC']).labels

featuresUnused = ['Code 1', 'Code 2','SUMMARYOPS','RIGSCALC','TVD','StringFromCode','PhaseFromCode'] #
code2 = analyzeMetric('Code 2',training, featuresUnused)
showFeatureImportance(training, code2['features'], 'Code 2')

tempTest = predict(code2['model'], testing[code2['features']], 'Code 2_ML')
testing['Code 2_ML'] = tempTest['Code 2_ML']
tempTest = predict(code2['model'], testingAll[code2['features']], 'Code 2_ML')
testingAll['Code 2_ML'] = tempTest['Code 2_ML']

featuresUnused = ['Code 1','Code 2_ML','SUMMARYOPS','RIGSCALC','TVD','StringFromCode','PhaseFromCode']
training['Code 2'] = pd.Categorical.from_array(training['Code 2']).labels
testing['Code 2'] = pd.Categorical.from_array(testing['Code 2_ML']).labels
testingAll['Code 2'] = pd.Categorical.from_array(testingAll['Code 2_ML']).labels
code1 = analyzeMetric('Code 1',training,featuresUnused)
showFeatureImportance(training, code1['features'], 'Code 1')

tempTest = predict(code1['model'], testing[code1['features']], 'Code 1_ML')
testing['Code 1_ML'] = tempTest['Code 1_ML']
tempTest = predict(code1['model'], testingAll[code1['features']], 'Code 1_ML')
testingAll['Code 1_ML'] = tempTest['Code 1_ML']

testing.to_csv('classified_nojv.csv', sep=',', encoding='utf-8')
testingAll.to_csv('classified_nojv_all.csv', sep=',', encoding='utf-8')

#
print(classificationAccuracy(testing['StringFromCode'],testing['Code 1_ML']))
#91.3043478261

print(classificationAccuracy(testing['PhaseFromCode'],testing['Code 2_ML']))
#94.8849104859

##training['metric_ori'] = training['Code 1'] + ' ' + training['Code 2']
##numberOfOriStrings =  training.groupby(training.index)['metric_ori'].apply(lambda x: len(x.unique()))
##print(numberOfOriStrings.value_counts())
##rows(training)
#
#training['metric_pred'] = training['Code 1_ML'] + ' ' + training['Code 2_ML']
#numberOfPredStrings =  training.groupby(training.index)['metric_pred'].apply(lambda x: len(x.unique()))
#print(numberOfPredStrings.value_counts())


#
## calculate the timelines
##print('check training originals')
##training['strings'] = training['Code 1'] + ' ' + training['Code 2']
##trainWells = training.groupby(training.index)
##aggregateWells(trainWells, 'NOJV', 'trained original')
##
##print('check training predicteds')
##training['strings'] = training['Code 1_ML'] + ' ' + training['Code 2_ML']
##trainWells = training.groupby(training.index)
##aggregateWells(trainWells, 'NOJV', 'trained predicted')
##print('for count of wells = ', len(set(training.index)))

print('check test 200 for timelines:')
wellsML = aggregateWells('NOJV', 'test ML', 'strings', 'Code 1_ML','Code 2_ML',testing)
wellsCode = aggregateWells('NOJV', 'test Code', 'strings-code', 'StringFromCode','PhaseFromCode',testing)
print(classificationAccuracy(testing['strings-code'],testing['strings']))
#87.5959079284

# here we need to calculate the 1 string wells
wellsOthers = aggregateWells('NOJV', 'all wells', 'strings', 'Code 1_ML','Code 2_ML',testingAll)

#nojv2filenameCode = 'test Code-NOJV-2string.csv'
#nojv2filenameML = 'test ML-NOJV-2string.csv'
#nojv3filenameCode = 'test Code-NOJV-3string.csv'
#nojv3filenameML = 'test ML-NOJV-3string.csv'

# Code timelines
nojv2 = wellsCode['2-string-wells']
nojv3 = wellsCode['3-string-wells']

IDs = set(nojv2Manual.index.values)
ids = set(nojv2.index.values)
uniqueInToTrain = IDs.difference(ids)
nojv2Manual = nojv2Manual[nojv2Manual.index.isin(uniqueInToTrain)]

# combined = manual + code
nojv2string = nojv2.append(nojv2Manual)

IDs = set(nojv3Manual.index.values)
ids = set(nojv3.index.values)
uniqueInToTrain = IDs.difference(ids)
nojv3Manual = nojv3Manual[nojv3Manual.index.isin(uniqueInToTrain)]

# combined = manual + code
nojv3string = nojv3.append(nojv3Manual)

# ML timelines
nojv2ML = wellsML['2-string-wells']
nojv3ML = wellsML['3-string-wells']

#drillNOJV2Times = drawTimelines('NOJV',nojv2string, False)
#drillNOJV3Times = drawTimelines('NOJV',nojv3string, False)
