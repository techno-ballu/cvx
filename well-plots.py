# -*- coding: utf-8 -*-
"""
Created on Fri Apr 17 12:50:03 2015

@author: bolaka
"""

import os

os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV')
    
idCol = 'IDWELL'
savedfilename = 'nojv-formations.csv'
deltaX = 1000

formations = pd.read_csv(savedfilename,  index_col=idCol) 

depths = formations['depth'].values
    
#uniqdepths = np.unique(depths)
#print('extracted ', len(uniqdepths), ' unique depths')
#minDepth = int(min(uniqdepths))
#maxDepth = int(max(uniqdepths))
#
#orderedX = list(range(minDepth, maxDepth, deltaX))
#orderedX = orderedX[:11]
##histX = np.histogram(depths, 50)[1]
##print('no of bins in x = ', binsX, ' at:\n', histX.tolist())
#formations['depth_code'] = np.digitize(depths, orderedX)
#print(formations['depth_code'].value_counts())
#
##print('can be grouped into = ',len(orderedX))
#labels = []
#for i, x in enumerate(orderedX):
#    labelX = str(x) + "' to " + str(x + deltaX) + "'"
#    labels.append({ 'index':(i+1), 'min':x, 'upper':(x + deltaX),'label':labelX })
##    print(labelX)
#
##print(labels)
#df = pd.DataFrame(labels)
#df = df.set_index('index')

plt.figure(figsize=(15, 8))

# Remove the plot frame lines. They are unnecessary chartjunk.
ax = plt.subplot(111)
ax.spines["top"].set_visible(False)
ax.spines["bottom"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.spines["left"].set_visible(False)

# Ensure that the axis ticks only show up on the bottom and left of the plot.
# Ticks on the right and top of the plot are generally unnecessary chartjunk.
ax.get_xaxis().tick_bottom()
ax.get_yaxis().tick_left()

wells = formations.groupby(formations.index)
#count = 0
for name, group in wells:
#    well = group.groupby('depth_code')
    lostcases = np.sum(group['Circulation'].values)
    haslost = False
    if lostcases > 0:
        haslost = True
    print(name, haslost)
    
#    if haslost == False:
#        plt.plot(group['depth'].values, group['viscosity'].values, lw=1.0, color='b')    
#    else:
    plt.plot(group['depth'].values, group['WOB'].values, lw=2.0, color='r')    
    ax.fill_between(group['depth'].values, group['WOB'], y2=0, where=group['Circulation'], facecolor='blue', alpha=0.5)
#    if count > 20:
#        break
    
#    count += 1    
    
#    mudwts = []
#    for i, r in df.iterrows():
#        weight = float('nan')
#        for code, rows in well:
#        
#            if i == code:
#                weight = rows['mud-wt'].mean() # diameter, viscosity
##                print(name, code, weight)
#                break
#        mudwts.append(weight)
#        
#    availables = np.sum(~np.isnan(mudwts))
#    if availables > 0:
#        df[name] = mudwts
#        # Plot each line separately with its own color, using the Tableau 20
#        # color set in order.
#        if haslost == False:
##            plt.plot(df.index.values, mudwts, lw=1.0, color='b')
#            print('plotting ', name)
#        else:
#            plt.plot(df.index.values, mudwts, lw=2.0, color='r')

#wells = formations.groupby(formations.index)
#for name, group in wells:
#    well = group.groupby('depth_code')
#    lostcases = np.sum(group['Circulation'].values)
#    haslost = False
#    if lostcases > 0:
#        haslost = True
#    print(name, haslost)
#    mudwts = []
#    for i, r in df.iterrows():
#        weight = float('nan')
#        for code, rows in well:
#        
#            if i == code:
#                weight = rows['mud-wt'].mean() # diameter, viscosity
##                print(name, code, weight)
#                break
#        mudwts.append(weight)
#        
#    availables = np.sum(~np.isnan(mudwts))
#    if availables > 0:
#        df[name] = mudwts
#        # Plot each line separately with its own color, using the Tableau 20
#        # color set in order.
#        if haslost == False:
##            plt.plot(df.index.values, mudwts, lw=1.0, color='b')
#            print('plotting ', name)
#        else:
#            plt.plot(df.index.values, mudwts, lw=2.0, color='r')

#df = df.set_index('label')
#temp = df
#temp.drop(temp.columns[[0, 1]], axis=1, inplace=True)
#
#f = plt.figure(figsize=(15, 8)) # Change the size as necessary
#ax = temp.plot(ax=f.gca(), alpha=0.5,color='k', legend=False) #, title='viscosity over depths'
#
#ax.spines["top"].set_visible(False)  
#ax.spines["bottom"].set_visible(False)  
#ax.spines["right"].set_visible(False)  
#ax.spines["left"].set_visible(False)
#ax.set_xlabel("depth bins")
##ax.set_ylabel("viscosity")