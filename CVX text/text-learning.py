# -*- coding: utf-8 -*-
"""
Created on Mon Mar 16 12:11:14 2015

@author: bolaka
"""

# imports
import os
import re
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from numpy.random import seed
from sklearn.cross_validation import train_test_split
from sklearn.cross_validation import KFold
from sklearn.svm import SVC
from sklearn.ensemble import AdaBoostClassifier as ABC
from sklearn.ensemble import BaggingClassifier as BC
from sklearn.ensemble import ExtraTreesClassifier as ETC
from sklearn.ensemble import RandomForestClassifier as RF
from sklearn.ensemble import GradientBoostingClassifier as GBC
from sklearn.neighbors import KNeighborsClassifier as KNN
from sklearn.linear_model import LogisticRegression as LR
from sklearn.metrics import average_precision_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
from fuzzywuzzy import fuzz

# extract the names of the pandas data frame
def names(pandas_dataframe):
    return (pandas_dataframe.columns.values.tolist())
    
def rows(data):
    print('count of wells', len(set(data.index)))

results = '/ML/16March'

directory = '/home/bolaka/CVX text/' + results

if not os.path.exists(directory):
    os.makedirs(directory)

# setting work directory
os.chdir(directory)
    
idCol = 'IDWELL'
trainingfilename = '../trainv7.csv'
validationfilename = '../test2.csv'
testing200filename = '../test200.csv'
testing1260filename = '../test1260.csv'

#trainingfilename = 'training-features.csv'
#validationfilename = 'validation-features.csv'
#testing200filename = 'testing200-features.csv'
#testing1260filename = 'testing1260-features.csv'

training = pd.read_csv(trainingfilename,  index_col=idCol) 
validation = pd.read_csv(validationfilename,  index_col=idCol) 
testing200 = pd.read_csv(testing200filename,  index_col=idCol) 
testing1260 = pd.read_csv(testing1260filename,  index_col=idCol) 

## add text extracted features
#bagOfWords = [ 'SPUD','MIRU','RU', 'R/U','RIG UP', 'TOH', 'TOOH', 'TRIP OUT', 'DRILLING', 
#              'DRLG', 'WASHING TO BOTTOM', 'TIH', 'TRIP IN',
#              'RIG RELEASE','NIPPLE', 'NU', 'NUBOP', 'ND',
#              'RIG DOWN', 'RD', 'R/D', 'CASING', 'CSG', 'CREW', 'WOC', 'LAYING DOWN', 
#              'WAIT ON CEMENT', 'BOP', 'WELL HEAD', 
#              'FISHING', 'PRODUCTION','N/U','NIPPLE DOWN', 'N/D','NDBOP', 'PROD',
#              'SURFACE', 'SRF', 'INTERMEDIATE']  # 'CEMENT', 'CMT','LDDP', 'LD DP', '13-3/8 CSG', '8-5/8 CSG', '5-1/2 CSG', '9-5/8 CSG'

# add text extracted features
bagOfWords = [ 'SPUD', 'RUN CSG','RUN CASING', 'DRILL', 'DRLG', 'SURFACE CASING', 'LD DP', 
              'DMC', 'DAILY MUD COST', 'PRODUCTION CASING', 'INTERMEDIATE CASING' ]  

def extractFeatures(data,name):
    print('Extract features for',name)
    data['SUMMARYOPS'].fillna('NA', inplace=True)
    for word in bagOfWords:
        print(word)
        
        ratio = [fuzz.partial_token_set_ratio( re.sub(r'[\s"\\]', ' ', cmt).strip().upper(), word ) for cmt in data['SUMMARYOPS']]
        data['is' + word] = np.array([r > 70 for r in ratio]) * 1
    data.to_csv(name, sep=',', encoding='utf-8')

extractFeatures(training, 'training-features.csv')
extractFeatures(validation, 'validation-features.csv')
extractFeatures(testing200, 'testing200-features.csv')
extractFeatures(testing1260, 'testing1260-features.csv')

X = training[training.columns - ['DURATIONSPUDTOTDCALC','SUMMARYOPS','RIGSCALC','Code 1', 'Code 2','Code 3',
                         'JOBTVD','MANUALTVD','CODETVD','StringFromCode','PhaseFromCode', 'TVD']]
x_val = validation[validation.columns - ['DURATIONSPUDTOTDCALC','SUMMARYOPS','RIGSCALC','Code 1', 'Code 2','Code 3',
                         'JOBTVD','MANUALTVD','CODETVD','StringFromCode','PhaseFromCode', 'TVD']]
x_test200 = testing200[testing200.columns - ['DURATIONSPUDTOTDCALC','SUMMARYOPS','RIGSCALC','Code 1', 'Code 2','Code 3',
                         'JOBTVD','MANUALTVD','CODETVD','StringFromCode','PhaseFromCode', 'TVD']]
x_test1260 = testing1260[testing1260.columns - ['DURATIONSPUDTOTDCALC','SUMMARYOPS','RIGSCALC','Code 1', 'Code 2','Code 3',
                         'JOBTVD','MANUALTVD','CODETVD','StringFromCode','PhaseFromCode', 'TVD']]

print(X.info(),'\n')

# Cross Validation
def runCV(X,y,clf_class,**kwargs):
    # Construct a kfolds object
    kf = KFold(len(y),n_folds=5,shuffle=True)
    y_pred = y.copy()
    
    # Iterate through folds
    for train_index, test_index in kf:
#        print('Training Samples = ', len(train_index), '; Test Samples', len(test_index))
        X_train, X_test = X[train_index], X[test_index]
        y_train = y[train_index]
        # Initialize a classifier with key word arguments
        clf = clf_class(random_state=100)
        clf.fit(X_train,y_train)
        y_pred[test_index] = clf.predict(X_test)
    return y_pred

# accuracy
def classificationAccuracy(Y_ori,Y_pred):
    # NumPy interpretes True and False as 1. and 0.
#    return np.mean(Y_ori == Y_pred)*100
    return (accuracy_score(Y_ori, Y_pred,normalize=False)/len(Y_ori) * 100)

# Confusion Matrix    
def drawConfusionMatrix(confusion_matrix,class_names):
    class_names = class_names.tolist()
    for cm in confusion_matrix:
        classifier, cm, accu = cm[0], cm[1], cm[2]
        print(classifier)
        print(accu)
#        print(cm)
        print('\n') 
        
        norm_cm = []
        for i in cm:
                a = 0
                tmp_arr = []
                a = sum(i,0)
                for j in i:
                        tmp_arr.append(float(j)/float(a) * 100)
                norm_cm.append(tmp_arr)        
        
        fig = plt.figure()
        ax = fig.add_subplot(111)
#        cax = ax.matshow(cm)
        cax = ax.imshow(norm_cm, interpolation='nearest')
        for i, cas in enumerate(norm_cm):
            for j, c in enumerate(cas):
                if c>0:
                    plt.text(j-.2, i+.2, "%.1f" % c, fontsize=14)
        plt.title('Confusion matrix for %s' % classifier)
        fig.colorbar(cax)
        ax.set_xticklabels([''] + class_names)
        ax.set_yticklabels([''] + class_names)
        plt.xlabel('Predicted')
        plt.ylabel('Actual')
        plt.show()

def analyzeMetric(metric):
#    metric = 'Code 2'
    print('Analyzing', metric)
    Y = training[metric]
    y_val = validation[metric]
    
    features = X.columns
#    print("Column names in training:")
#    print(features,'\n')
#    
#    featuresVal = x_val.columns
#    print("Column names in validation:")
#    print(featuresVal,'\n')
#    
#    featuresTest = x_test.columns
#    print("Column names in testing:")
#    print(featuresTest,'\n')
    
    # To reproduce results, fix the random seed
    seed(1)

#     as an array
    x = np.asanyarray(X)
    y = np.asanyarray(Y)
    #est = GradientBoostingClassifier(n_estimators=200, max_depth=3)
    #est.fit(X,Y)
    #Y_HAT = est.predict(X)

    # classifier predictions
    linearP = runCV(x, y, LR)
    gbmP = runCV(x, y, GBC )
    svmP = runCV(x, y, SVC)
    forestP = runCV(x, y, RF)
    adaP = runCV(x, y, ABC )
    bagP = runCV(x, y, BC)
    extraP = runCV(x, y, ETC)
    #knnP = runCV(x, y, KNN)

    # accuracies
    linearAcc = classificationAccuracy(y, linearP) 
    gbmAcc = classificationAccuracy(y, gbmP) 
    svmAcc = classificationAccuracy(y, svmP) 
    forestAcc = classificationAccuracy(y, forestP) 
    adaAcc = classificationAccuracy(y, adaP) 
    bagAcc = classificationAccuracy(y, bagP) 
    extraAcc = classificationAccuracy(y, extraP) 
    #knnAcc = classificationAccuracy(y, knnP) 

    mappings = [
        { 'name' : "Logistic Regression", 
          'algo' : LR
        },
        { 'name' : "Gradient Boosting Classifier", 
          'algo' : GBC
        },
        { 'name' : "Support Vector Machines", 
          'algo' : SVC
        },
        { 'name' : "Random Forest", 
          'algo' : RF
        },
        { 'name' : "Ada Boost Classifier", 
          'algo' : ABC
        },
        { 'name' : "Bagging Classifier", 
          'algo' : BC
        },
        { 'name' : "Extra Trees Classifier", 
          'algo' : ETC
        }
    #    { 'name' : "K-Nearest-Neighbors", 
    #      'algo' : KNN
    #    }
    ]
    accuracies = [
        { 'name' : "Logistic Regression", 
          'accuracy' : linearAcc
        },
        { 'name' : "Gradient Boosting Classifier", 
          'accuracy' : gbmAcc
        },
        { 'name' : "Support Vector Machines", 
          'accuracy' : svmAcc
        },
        { 'name' : "Random Forest", 
          'accuracy' : forestAcc 
        },
        { 'name' : "Ada Boost Classifier", 
          'accuracy' : adaAcc 
        },
        { 'name' : "Bagging Classifier", 
          'accuracy' : bagAcc 
        },
        { 'name' : "Extra Trees Classifier", 
          'accuracy' : extraAcc 
        }
    #    ,{ 'name' : "K-Nearest-Neighbors", 
    #      'accuracy' : knnAcc
    #    }
    ]
    
    print(Y.value_counts(), '\n')

    class_names = np.unique(y)
    confusion_matrices = [
        ( "Support Vector Machines", confusion_matrix( y, svmP ), svmAcc  ),
        ( "Random Forest", confusion_matrix( y, forestP ), forestAcc ),
    #    ( "K-Nearest-Neighbors", confusion_matrix( y, knnP ), knnAcc ),
        ( "Gradient Boosting Classifier", confusion_matrix( y,gbmP ), gbmAcc ),
        ( "Logistic Regression", confusion_matrix( y,linearP ), linearAcc ),
        ( "Ada Boost Classifier", confusion_matrix( y, adaP ), adaAcc  ),
        ( "Bagging Classifier", confusion_matrix( y, bagP ), bagAcc ),
        ( "Extra Trees Classifier", confusion_matrix( y, extraP ), extraAcc )
    ]

    drawConfusionMatrix(confusion_matrices, class_names)
    #print(accuracies)
    theBest = max(accuracies, key=lambda x:x['accuracy'])['name']
    bestAlgo = next(d for (index, d) in enumerate(mappings) if d["name"] == theBest)['algo']
    bestAcc = max(accuracies, key=lambda x:x['accuracy'])['accuracy']
    print('Best performer:')
    print(theBest, bestAcc, bestAlgo)

    # get the best performing algorithm
    best = bestAlgo()
    whole_fit = best.fit(X, Y)
    
    y_hat = best.predict(X)    
    training[metric + '_ML'] = y_hat    
    
    # validation predictions    
    y_hat_val = best.predict(x_val)
    valAccuracy = classificationAccuracy(y_val,y_hat_val)
    print('Accuracy on validation set for',metric,'is',valAccuracy)
    validation[metric + '_ML'] = y_hat_val
    
    # 200 predictions    
    y_hat_test200 = best.predict(x_test200)
    testing200[metric + '_ML'] = y_hat_test200
    
    # 1260 predictions    
    y_hat_test1260 = best.predict(x_test1260)
    testing1260[metric + '_ML'] = y_hat_test1260
    
    # feature importance
    importances = whole_fit.feature_importances_
    indices = np.argsort(importances)[::-1]
    # Print the feature ranking
    print("Feature ranking:")
    for f in range(len(features)):
        print("%d. %s (%f)" % (f + 1, features[f], importances[indices[f]]))
    
#    importances = 100.0 * (importances / importances.max())
#    indices = np.argsort(importances)
#    pos = np.arange(indices.shape[0]) + 0.5
#    plt.figure(figsize=(12,8))
#    plt.subplot(1, 1, 1)
#    plt.barh(pos, importances[indices], align='center')
#    plt.yticks(pos, features[indices])
#    plt.xlabel('Relative Importance')
#    plt.title('Variable Importance')
#    plt.show()
    
analyzeMetric('Code 1')    
analyzeMetric('Code 2') 
#print(names(testing))
#testing.to_csv('testing.csv', sep=',', encoding='utf-8')

#training['metric_ori'] = training['Code 1'] + ' ' + training['Code 2']
#numberOfOriStrings =  training.groupby(training.index)['metric_ori'].apply(lambda x: len(x.unique()))
#print(numberOfOriStrings.value_counts())
#
#training['metric_pred'] = training['Code 1_ML'] + ' ' + training['Code 2_ML']
#numberOfPredStrings =  training.groupby(training.index)['metric_pred'].apply(lambda x: len(x.unique()))
#print(numberOfPredStrings.value_counts())
#
#training.to_csv('training.csv', sep=',', encoding='utf-8')

#groups1 = testing.groupby(testing.index)['strings'].value_counts()
#groups2 =  testing.groupby(testing.index)['strings'].apply(lambda x: len(x.unique()))
#print(groups2.value_counts())

def aggregateWells(groups, owner, prefix):
    ID = 'WELLID'
    two_string_wells = []
    three_string_wells = []
    tossed_out_wells = []

    for name, group in groups:
#        print(name)
        extractDepth = False
        if 'TVD' in group.columns:   
            extractDepth = True
        
        rigIden = set(group['RIGSCALC'])
        strings = group['strings'].value_counts()
        names = strings.index.values
        
        correct2 = ['SURFACE DRILLING','SURFACE CASING','PRODUCTION DRILLING','PRODUCTION CASING']
        match2 = set(names) & set(correct2)
        correct3 = ['SURFACE DRILLING','SURFACE CASING', 'INTERMEDIATE DRILLING', 'INTERMEDIATE CASING',
                    'PRODUCTION DRILLING', 'PRODUCTION CASING']
        match3 = set(names) & set(correct3)    
        previousRow = None             
        
        if len(names) == 4 and len(match2) == 4:
            depths = {'SURFACE' : 'NA', 'PRODUCTION' : 'NA'}       
            
            if extractDepth:
                # extract depths for 2-string
#                print('2-string')
                
                # extract depths for 3-string
                for index, row in group.iterrows():
                    if row['Code 2_ML'] == 'CASING':  
                        if len(depths.keys()) > 0 and row['Code 1_ML'] in depths == True:
                            continue
#                        print('checking', row['TVD'], row['RIGDAYSCALC'])
                        if pd.isnull(row['TVD'] and previousRow is not None):
#                            depth = {}
                            depths[row['Code 1_ML']] = previousRow['TVD']
#                            print('previous=', depth)
#                            depths.append(depth)
                        else:
#                            depth = {}
                            depths[row['Code 1_ML']] = row['TVD']
#                            print('current=', row['TVD'])
#                            depths.append(depth)

                    previousRow = row
                     
                print(depths)
                                
            dict1 = {}
            dict1['#'] = len(two_string_wells) + 1
            dict1[ID] = name
            dict1['TVD'] = depths['PRODUCTION']
            dict1['Surf D days'] = strings['SURFACE DRILLING']
            dict1['Surf C days'] = strings['SURFACE CASING']
            dict1['Prod D days'] = strings['PRODUCTION DRILLING']
            dict1['Prod C days'] = strings['PRODUCTION CASING']
            dict1['Surf string TVD'] = depths['SURFACE']
            dict1['Prod string TVD'] = depths['PRODUCTION']
            dict1['RIGIDENTIFIER'] = rigIden
            
            two_string_wells.append(dict1)
            
        elif len(names) == 6 and len(match3) == 6:
            depths = {'SURFACE' : 'NA', 'PRODUCTION' : 'NA', 'INTERMEDIATE' : 'NA'}
            
            if extractDepth:
#                print('3-string')
                
                # extract depths for 3-string
                for index, row in group.iterrows():
                    if row['Code 2_ML'] == 'CASING':
                        if len(depths.keys()) > 0 and row['Code 1_ML'] in depths == True:
                            continue
#                        print('checking', row['TVD'], row['RIGDAYSCALC'])
                        if pd.isnull(row['TVD'] and previousRow is not None):
    #                        previousDay = int(row['RIGDAYSCALC']) - 1
#                            previousRow = group[group['RIGDAYSCALC'] == previousDay]
#                            depth = {}
                            depths[row['Code 1_ML']] = previousRow['TVD']
#                            print('previous=', previousRow['TVD'])
#                            depths.append(depth)
                        else:
#                            depth = {}
                            depths[row['Code 1_ML']] = row['TVD']
#                            print('current=', row['TVD'])
#                            depths.append(depth)

#                        if len(depths) == 3:
#                            break
                    previousRow = row
                    
                print(depths)
                    
            dict1 = {}
            dict1['#'] = len(three_string_wells) + 1
            dict1[ID] = name
            dict1['TVD'] = depths['PRODUCTION']
            dict1['Surf D days'] = strings['SURFACE DRILLING']
            dict1['Surf C days'] = strings['SURFACE CASING']
            dict1['Inter D days'] = strings['INTERMEDIATE DRILLING']
            dict1['Inter C days'] = strings['INTERMEDIATE CASING']
            dict1['Prod D days'] = strings['PRODUCTION DRILLING']
            dict1['Prod C days'] = strings['PRODUCTION CASING']
            dict1['Surf string TVD'] = depths['SURFACE']
            dict1['Inter string TVD'] = depths['INTERMEDIATE']
            dict1['Prod string TVD'] = depths['PRODUCTION']
            dict1['RIGIDENTIFIER'] = rigIden
            
            three_string_wells.append(dict1)
        
        else:
            dict1 = {}
            dict1['#'] = len(tossed_out_wells) + 1
            dict1[ID] = name
            dict1['TVD'] = 'NA'
#            dict1['Surf D days'] = strings['SURFACE DRILLING']
#            dict1['Surf C days'] = strings['SURFACE CASING']
#            dict1['Inter D days'] = strings['SURFACE DRILLING']
#            dict1['Inter C days'] = strings['SURFACE CASING']
#            dict1['Prod D days'] = strings['PRODUCTION DRILLING']
#            dict1['Prod C days'] = strings['PRODUCTION CASING']
#            dict1['Surf string TVD'] = name
#            dict1['Inter string TVD'] = name
#            dict1['Prod string TVD'] = name
            dict1['RIGIDENTIFIER'] = rigIden
            
            tossed_out_wells.append(dict1)
        if len(three_string_wells) > 100:
            break
            
    if len(two_string_wells) > 0:
        twoStrings = pd.DataFrame(two_string_wells)   
        twoStrings.set_index([ID],inplace=True)
        count2 = len(set(twoStrings.index))
        print('\n', count2, '2-string',owner, prefix, 'wells extracted')
        twoStrings.to_csv(prefix + '-' + owner + '-2string.csv', sep=',', encoding='utf-8')

    if len(three_string_wells) > 0:
        threeStrings = pd.DataFrame(three_string_wells)   
        threeStrings.set_index([ID],inplace=True)
        count3 = len(set(threeStrings.index))
        print(count3, '3-string', owner, prefix, 'wells extracted')
        threeStrings.to_csv(prefix + '-' + owner + '-3string.csv', sep=',', encoding='utf-8')
    
    if len(tossed_out_wells) > 0:
        tossed = pd.DataFrame(tossed_out_wells)   
        tossed.set_index([ID],inplace=True)
        countTossed = len(set(tossed.index))
        print(countTossed, owner, prefix, 'wells tossed','\n\n')
        tossed.to_csv(prefix + '-' + owner + '-tossed.csv', sep=',', encoding='utf-8')


# calculate the timelines
#print('check training originals')
#training['strings'] = training['Code 1'] + ' ' + training['Code 2']
#trainWells = training.groupby(training.index)
#aggregateWells(trainWells, 'NOJV', 'trained original')
#
#print('check training predicteds')
#training['strings'] = training['Code 1_ML'] + ' ' + training['Code 2_ML']
#trainWells = training.groupby(training.index)
#aggregateWells(trainWells, 'NOJV', 'trained predicted')
#print('for count of wells = ', len(set(training.index)))

print('check test 200 for timelines')
testing200['strings'] = testing200['Code 1_ML'] + ' ' + testing200['Code 2_ML']
testWells200 = testing200.groupby(testing200.index)
aggregateWells(testWells200, 'NOJV', 'test 200')
testing200.to_csv('classified_200.csv', sep=',', encoding='utf-8')

print('check remainder test 1260')
testing1260['strings'] = testing1260['Code 1_ML'] + ' ' + testing1260['Code 2_ML']
testWells1260 = testing1260.groupby(testing1260.index)
aggregateWells(testWells1260, 'NOJV', 'test 1260')
testing1260.to_csv('classified_1260.csv', sep=',', encoding='utf-8')
