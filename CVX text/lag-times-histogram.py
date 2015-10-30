# -*- coding: utf-8 -*-
"""
Created on Sat Mar 14 11:23:32 2015

@author: bolaka
"""

# imports
import os
import pandas as pd
import matplotlib.pyplot as plt


# extract the names of the pandas data frame
def names(pandas_dataframe):
    return (pandas_dataframe.columns.values.tolist())

results = '/deliverables/14March'

directory = '/home/bolaka/CVX text/' + results

color = '#9ECAF5'  # light blue
barWidth=2
idCol = 'WellID'

# setting work directory
os.chdir(directory)

def drawSingleHistogram(series1, title, name):
    plt.hist(series1, bins=20, color=color) #, width=barWidth
    plt.title(title)
    plt.xlabel("Time in days")
    plt.ylabel("Rates of times")
    plt.legend()
    F = plt.gcf() 
    DefaultSize = F.get_size_inches()
    F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
    F.savefig(name, dpi = (200)) 
    plt.show()

coop2filename = 'coop2lagtimes.csv'
coop3filename = 'coop3lagtimes.csv'
nojvfilename = 'nojv-tossed-drill-times.csv'

# header is conveniently inferred by default
coop2 = pd.read_csv(coop2filename,  index_col=idCol) 
coop3 = pd.read_csv(coop3filename,  index_col=idCol) 
#nojv = pd.read_csv(nojvfilename, index_col=idCol)

#drawSingleHistogram(nojv['Total Drilling Time'], 'Histogram showing distribution of drilling times of NOJV tossed out wells', 'nojv-drill-tossed.png')
drawSingleHistogram(sorted(coop2['LAG']), 'Histogram showing distribution of sorted lag times of COOP 2-string wells', 'coop2-lag.png')
drawSingleHistogram(sorted(coop3['LAG']), 'Histogram showing distribution of sorted lag times of COOP 3-string wells', 'coop3-lag.png')