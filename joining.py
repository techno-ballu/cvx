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
tvdsfilename = '../ml.csv'

nojv = pd.read_csv(nojvfilename) 
tvds = pd.read_csv(tvdsfilename) 

indexed_nojv = nojv.set_index(['IDWELL', 'RIGDAYSCALC'])
indexed_tvds = tvds.set_index(['ID', 'RIGDAYSCALC'])

indexed_nojv.join(indexed_tvds, lsuffix='_l', rsuffix='_r')

#indexed_nojv.merge(indexed_tvds, indexed_tvds, on='business_id', how='outer')