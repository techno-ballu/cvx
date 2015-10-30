# -*- coding: utf-8 -*-
"""
Created on Mon Mar 23 20:12:12 2015

@author: bolaka
"""

# imports
import time
import datetime
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/COOP')
    
idCol = 'IDWELL'
trainingfilename = 'trained-coop.csv'
testingfilename = 'coop-verified.csv'

training = pd.read_csv(trainingfilename,  index_col=idCol) 
testing = pd.read_csv(testingfilename,  index_col=idCol) 

# normalize 
training = normalizeColumns(training, ['DAYSFROMSPUDCALC', 'DEPTHENDDPCALC'])
testing = normalizeColumns(testing, ['DAYSFROMSPUDCALC', 'DEPTHENDDPCALC'])

print('training set has', rows(training))
print('testing set has', rows(testing))

# testing word cloud generator
#wordsFreq = drawWordCloud(training['SUMMARYOPS']) # SUMMARYOPS
#wordsFreq.to_csv('words-frequency.csv', sep=',', encoding='utf-8')

# add text extracted features
#bagOfWords = [ 'SPUD','MIRU','RU', 'R/U','RIG UP', 'TOH', 'TOOH', 'TRIP OUT', 'DRILLING', 
#              'DRLG', 'WASHING TO BOTTOM', 'TIH', 'TRIP IN',
#              'RIG RELEASE','NIPPLE', 'NU', 'NUBOP', 'ND',
#              'RIG DOWN', 'RD', 'R/D', 'CASING', 'CSG', 'CREW', 'WOC', 'LAYING DOWN', 
#              'WAIT ON CEMENT', 'BOP', 'WELL HEAD', 
#              'FISHING', 'PRODUCTION','N/U','NIPPLE DOWN', 'N/D','NDBOP', 'PROD',
#              'SURFACE', 'SRF', 'INTERMEDIATE']  # 'CEMENT', 'CMT','LDDP', 'LD DP', '13-3/8 CSG', '8-5/8 CSG', '5-1/2 CSG', '9-5/8 CSG'

# add text extracted features
#bagOfWords = [ 'MIRU', 'SPUD', 'PRODUCTION', 'INTERMEDIATE','SURFACE','RIG RELEASE','RIG DOWN',
#               'RUN CSG','RUN 13-3/8" CASING', 'LDDP', 'LAYING DOWN DRILL PIPE',
#               'RUN 8-5/8" CASING','RUN 5-1/2" CASING','CEMENTING','WELL HEAD' ] # 'PROD','DRILLING', 'DRLG',

bagOfWords = [ 'MIRU', 'SPUD', 'PRODUCTION', 'INTERMEDIATE','SURFACE','RIG RELEASE','RIG DOWN',
               'RUN CSG','RUN 13-3/8" CASING', 'LDDP', 'LAYING DOWN DRILL PIPE','RUN CASING',
               'RUN 8-5/8" CASING','RUN 5-1/2" CASING','CEMENTING','WELL HEAD' ]

extractFeatures(training, 'training-features.csv',bagOfWords)
extractFeatures(testing, 'testing-features.csv',bagOfWords)

# handle RIGDAYSCALC missing
#training['RIGDAYS_missing'] = training['RIGDAYSCALC'].isnull() * 1
#testing['RIGDAYS_missing'] = testing['RIGDAYSCALC'].isnull() * 1
training['RIGDAYSCALC'] = training['RIGDAYSCALC'].fillna(0)
testing['RIGDAYSCALC'] = testing['RIGDAYSCALC'].fillna(0)

training['DTTMEND'] = [time.mktime(datetime.datetime.strptime(x, "%m/%d/%Y").timetuple()) for x in training['DTTMEND']]
testing['DTTMEND'] = [time.mktime(datetime.datetime.strptime(x, "%m/%d/%Y").timetuple()) for x in testing['DTTMEND']]
training['DTTMSTART'] = [time.mktime(datetime.datetime.strptime(x, "%m/%d/%Y").timetuple()) for x in training['DTTMSTART']]
testing['DTTMSTART'] = [time.mktime(datetime.datetime.strptime(x, "%m/%d/%Y").timetuple()) for x in testing['DTTMSTART']]

training['RIGSCALC_code'] = pd.Categorical.from_array(training['RIGSCALC']).labels
testing['RIGSCALC_code'] = pd.Categorical.from_array(testing['RIGSCALC']).labels
featuresUnused = ['Code 1', 'Code 2','RIGSCALC','SUMMARYOPS','TVD','StringFromCode','PhaseFromCode',
                  'PLANNEXTRPTOPS','STATUSEND','DAYSFROMSPUDCALC','DEPTHENDDPCALC'  ] #'RIGDAYSCALC',
code2 = analyzeMetric('Code 2',training,featuresUnused)
showFeatureImportance(training, code2['features'], 'Code 2')

tempTest = predict(code2['model'], testing[code2['features']], 'Code 2_ML')
testing['Code 2_ML'] = tempTest['Code 2_ML']

featuresUnused = ['Code 1','Code 2_ML','SUMMARYOPS','RIGSCALC','TVD','StringFromCode','PhaseFromCode',
                  'PLANNEXTRPTOPS','STATUSEND','DAYSFROMSPUDCALC','DEPTHENDDPCALC' ] #'RIGDAYSCALC',
training['Code 2'] = pd.Categorical.from_array(training['Code 2']).labels
categories = pd.Categorical.from_array(testing['Code 2_ML'])
testing['Code 2'] = categories.labels
code1 = analyzeMetric('Code 1',training,featuresUnused)
showFeatureImportance(training, code1['features'], 'Code 1')

tempTest = predict(code1['model'], testing[code1['features']], 'Code 1_ML')
testing['Code 1_ML'] = tempTest['Code 1_ML']

testing.to_csv('classified_coop.csv', sep=',', encoding='utf-8')

print(classificationAccuracy(testing['StringFromCode'],testing['Code 1_ML']))
##Out[77]: 92.8773204197
#
print(classificationAccuracy(testing['PhaseFromCode'],testing['Code 2_ML']))
##Out[78]: 95.01614205
#
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

print('check test for timelines:')

testing.rename(columns={'DEPTHENDDPCALC':'TVD'}, inplace=True)
wellsML = aggregateWells('COOP', 'test ML', 'strings', 'Code 1_ML','Code 2_ML',testing)
wellsCode = aggregateWells('COOP', 'test Code', 'strings-code', 'StringFromCode','PhaseFromCode',testing)
print(classificationAccuracy(testing['strings-code'],testing['strings']))
#90.2340597256

# Code timelines
#coop2 = wellsCode['2-string-wells']
#coop3 = wellsCode['3-string-wells']

#coop2 = wellsML['2-string-wells']
#coop3 = wellsML['3-string-wells']

# ML timelines
#nojv2 = wellsML['2-string-wells']
#nojv3 = wellsML['3-string-wells']

#drillCOOP2Times = drawTimelines('COOP',coop2, False)
#drillCOOP3Times = drawTimelines('COOP',coop3, False)