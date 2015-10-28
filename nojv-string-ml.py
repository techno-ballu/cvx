# -*- coding: utf-8 -*-
"""
Created on Fri Apr 24 18:03:10 2015

@author: bolaka
"""

import os
os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV')
    
idCol = 'IDWELL'
datafilename = 'nojvheaders.csv'
#datafilename = 'outliers3.csv'
trainingfilename = 'trainingheaders.csv'
testingfilename = 'testingheaders.csv'
testingAllfilename = datafilename

data = pd.read_csv(datafilename,  index_col=idCol) 

# categorical to numerical
#training['RIGSCALC_code'] = pd.Categorical.from_array(training['RIGSCALC']).labels
#testing['RIGSCALC_code'] = pd.Categorical.from_array(testing['RIGSCALC']).labels
#testingAll['RIGSCALC_code'] = pd.Categorical.from_array(testingAll['RIGSCALC']).labels

training = data.loc[ data.string > 0 ]
testing = data # data.loc[ np.isnan(data.string) ]
testingAll = data

print('training set has', rows(training))
print('testing set has', rows(testing))
print('testingAll set has', rows(testingAll))

training.to_csv('strings-training.csv', sep=',', encoding='utf-8')

#twos = training.loc[ training['string'] == 2]
twos = testing.loc[ testing['string'] == 2]
print(twos['OPERATOR'].value_counts())
#threes = training.loc[ training['string'] == 3]
threes = testing.loc[ testing['string'] == 3]
print(threes['OPERATOR'].value_counts())
twos.to_csv('strings-training2.csv', sep=',', encoding='utf-8')
threes.to_csv('strings-training3.csv', sep=',', encoding='utf-8')

# handle missing values
training = training.fillna(0)
testing = testing.fillna(0)
#twos = twos.fillna(0)
#threes = threes.fillna(0)
#training = training.dropna()
#testing = testing.dropna()

featuresUnused = ['string','formation','AREA','BASIN','COUNTY','CURRENTWELLSTATUS1','CURRENTWELLSTATUS2', #'DISTRICT_code','LEASE_code',
                  'DISTRICT','DIVISION','FIELDNAME','LEASE','OPERATOR', 'Lost Mud', 'Total Loss', 'DTTMEND' ] #'COUNTY_code','FIELDNAME_code'
strings = analyzeMetric('string',training, featuresUnused)
showFeatureImportance(training, strings['features'], 'string')

tempTest = predict(strings['model'], testing[strings['features']], 'string_pred')
testing['string_pred'] = tempTest['string_pred']
testing['string_pred'] = [int(x) for x in testing['string_pred']]
#print(testing.info())
testing.to_csv('strings-classified.csv', sep=',', encoding='utf-8')

print(testing.string_pred.value_counts())
#testing = testing.loc[ testing['string_pred'] > 1 ]
#ax = testing.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='string_pred', s=40, cmap='Blues',figsize=(12, 8)) #d['viscosity STD']*70 + 10
#ax.set_xlim(-102.4, -101.8)
#ax.set_ylim(31.2, 31.8)

#twos.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='OPERATOR_code', s=40, cmap='Blues',figsize=(12, 8))
#threes.plot(kind='scatter', x='LONGITUDE', y='LATITUDE', c='OPERATOR_code', s=40, cmap='Blues',figsize=(12, 8))

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

#groups = testing.groupby('OPERATOR')
#plotWellsByGrp(groups)

#featuresUnused = ['string','string_ML','formation','AREA','BASIN','COUNTY','CURRENTWELLSTATUS1','CURRENTWELLSTATUS2', 'missingness', 'total days', 'DISTRICT_code', #'string','LEASE_code',
#                  'DISTRICT','DIVISION','FIELDNAME','LEASE','OPERATOR', 'Lost Mud', 'Total Loss','LEASE_code','diameter STD','ph STD' ] # 'COUNTY_code','FIELDNAME_code'
#totalloss = analyzeMetric('Lost Mud',threes, featuresUnused)
#showFeatureImportance(threes, totalloss['features'], 'Lost Mud')

#tempTest = predict(code2['model'], testingAll[code2['features']], 'Code 2_ML')
#testingAll['Code 2_ML'] = tempTest['Code 2_ML']
#
#featuresUnused = ['Code 1','Code 2_ML','SUMMARYOPS','RIGSCALC','TVD','StringFromCode','PhaseFromCode']
#training['Code 2'] = pd.Categorical.from_array(training['Code 2']).labels
#testing['Code 2'] = pd.Categorical.from_array(testing['Code 2_ML']).labels
#testingAll['Code 2'] = pd.Categorical.from_array(testingAll['Code 2_ML']).labels
#code1 = analyzeMetric('Code 1',training,featuresUnused)
#showFeatureImportance(training, code1['features'], 'Code 1')
#
#tempTest = predict(code1['model'], testing[code1['features']], 'Code 1_ML')
#testing['Code 1_ML'] = tempTest['Code 1_ML']
#tempTest = predict(code1['model'], testingAll[code1['features']], 'Code 1_ML')
#testingAll['Code 1_ML'] = tempTest['Code 1_ML']
#
#testing.to_csv('classified_nojv.csv', sep=',', encoding='utf-8')
#testingAll.to_csv('classified_nojv_all.csv', sep=',', encoding='utf-8')
#
##
#print(classificationAccuracy(testing['StringFromCode'],testing['Code 1_ML']))
##91.3043478261
#
#print(classificationAccuracy(testing['PhaseFromCode'],testing['Code 2_ML']))
##94.8849104859
#
###training['metric_ori'] = training['Code 1'] + ' ' + training['Code 2']
###numberOfOriStrings =  training.groupby(training.index)['metric_ori'].apply(lambda x: len(x.unique()))
###print(numberOfOriStrings.value_counts())
###rows(training)
##
##training['metric_pred'] = training['Code 1_ML'] + ' ' + training['Code 2_ML']
##numberOfPredStrings =  training.groupby(training.index)['metric_pred'].apply(lambda x: len(x.unique()))
##print(numberOfPredStrings.value_counts())
#
#
##
### calculate the timelines
###print('check training originals')
###training['strings'] = training['Code 1'] + ' ' + training['Code 2']
###trainWells = training.groupby(training.index)
###aggregateWells(trainWells, 'NOJV', 'trained original')
###
###print('check training predicteds')
###training['strings'] = training['Code 1_ML'] + ' ' + training['Code 2_ML']
###trainWells = training.groupby(training.index)
###aggregateWells(trainWells, 'NOJV', 'trained predicted')
###print('for count of wells = ', len(set(training.index)))
#
#print('check test 200 for timelines:')
#wellsML = aggregateWells('NOJV', 'test ML', 'strings', 'Code 1_ML','Code 2_ML',testing)
#wellsCode = aggregateWells('NOJV', 'test Code', 'strings-code', 'StringFromCode','PhaseFromCode',testing)
#print(classificationAccuracy(testing['strings-code'],testing['strings']))
##87.5959079284
#
## here we need to calculate the 1 string wells
#wellsOthers = aggregateWells('NOJV', 'all wells', 'strings', 'Code 1_ML','Code 2_ML',testingAll)
#
##nojv2filenameCode = 'test Code-NOJV-2string.csv'
##nojv2filenameML = 'test ML-NOJV-2string.csv'
##nojv3filenameCode = 'test Code-NOJV-3string.csv'
##nojv3filenameML = 'test ML-NOJV-3string.csv'
#
## Code timelines
#nojv2 = wellsCode['2-string-wells']
#nojv3 = wellsCode['3-string-wells']
#
#IDs = set(nojv2Manual.index.values)
#ids = set(nojv2.index.values)
#uniqueInToTrain = IDs.difference(ids)
#nojv2Manual = nojv2Manual[nojv2Manual.index.isin(uniqueInToTrain)]
#
## combined = manual + code
#nojv2string = nojv2.append(nojv2Manual)
#
#IDs = set(nojv3Manual.index.values)
#ids = set(nojv3.index.values)
#uniqueInToTrain = IDs.difference(ids)
#nojv3Manual = nojv3Manual[nojv3Manual.index.isin(uniqueInToTrain)]
#
## combined = manual + code
#nojv3string = nojv3.append(nojv3Manual)
#
## ML timelines
#nojv2ML = wellsML['2-string-wells']
#nojv3ML = wellsML['3-string-wells']
#
##drillNOJV2Times = drawTimelines('NOJV',nojv2string, False)
##drillNOJV3Times = drawTimelines('NOJV',nojv3string, False)
