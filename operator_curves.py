# -*- coding: utf-8 -*-
"""
Created on Wed May 13 15:34:53 2015

@author: bolaka
"""

import os

os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports
from datetime import datetime
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/NOJV/operatorsWithTime')
    
idCol = 'IDWELL'
savedfilename = '../nojv_operators_time_analysis_detailed.csv'
aggrfilename = '../nojvheaders.csv'

aggregated = pd.read_csv(aggrfilename,  index_col=idCol) 
formations = pd.read_csv(savedfilename,  index_col=idCol) 

formations['timestamp'] = [datetime.strptime(x, "%m/%d/%Y") for x in formations.DTTMSTART]
formations['year'] = [x.year for x in formations['timestamp'] ]
formations['month'] = [x.month for x in formations['timestamp'] ]

## add the strings
#stringfile = '../nojv-string-classified-672.csv'
#stringinfo = pd.read_csv(stringfile)
#twos = stringinfo.loc[stringinfo.string == 2]
#threes = stringinfo.loc[stringinfo.string == 3]
#
#aggregated['string'] = float('nan')
#twoGrps = twos.groupby([idCol]) 
#threeGrps = threes.groupby([idCol]) 
#wells = aggregated.groupby(aggregated.index)
#
#for index, group in wells:
#    if index in twoGrps.groups:
#        aggregated.loc[index, 'string'] = 2
#    elif index in threeGrps.groups:
#        aggregated.loc[index, 'string'] = 3
#
aggregated = aggregated.loc[ (aggregated.DTTMEND != '0') ]

aggregated['timestamp'] = [datetime.strptime(x, "%m/%d/%Y") for x in aggregated.DTTMEND]
aggregated['year'] = [x.year for x in aggregated['timestamp'] ]
aggregated['month'] = [x.month for x in aggregated['timestamp'] ]

## extract OPERATOR
#wellDetails = formations.groupby(formations.index)
#wells = headers.groupby(headers.index)
#
#def setOperator(group):
#    idx = group.index.values[0]
#    if idx in wells.groups:
#        g = wells.get_group(idx)
#        group['OPERATOR'] = g['OPERATOR']
#    return group
#
#formations = wellDetails.apply(setOperator)
#formations.to_csv(savedfilename, sep=',', encoding='utf-8')

# remove outliers
toBeremoved = formations.loc[ (formations['year'] < 1990) ]
formations = formations.drop(toBeremoved.index)

#aggregated['year_month'] = (aggregated['year'] + aggregated['month']/12)
formations['year_month'] = (formations['year'] + formations['month']/12)
formations = formations.loc[ (formations['LONGITUDE'] < 0) ]
#depthCol = 'depth'
#mudwtCol = 'mud-wt'
#visCol = 'viscosity'
#phCol = 'pH'
#ropCol = 'ROP'
#depthTopCol = 'DEPTHTOP'
#depthBtmCol = 'DEPTHBTM'
#threshold = 200
#metric1 = 'Circulation'
#metric2 = 'Total Loss'
#metric3 = 'Mud Loss'

# separate 2-string & 3-string wells
#twos = formations.loc[ formations['string'] == 2 ]
#threes = formations.loc[ formations['string'] == 3 ]

#names = [ 'TVD', 'viscosity AVG', 'viscosity STD', 'mud weight STD', 'mud weight AVG', 'wob STD', 'wob AVG' ]
names = [ 'viscosity', 'mud-wt', 'pH', 'WOB', 'pump psi' ]

def plotWellsByGrp(groups, xName, yName, operator, xlim, ylim):
    # Plot
    plt.rcParams.update(pd.tools.plotting.mpl_stylesheet)
    colors = pd.tools.plotting._get_standard_colors(len(groups), color_type='random')
    
    fig, ax = plt.subplots()
    fig.set_size_inches(18,13)
    ax.set_color_cycle(colors)
    ax.margins(0.05)
    for name, group in groups:
        group = group.loc[ (group[yName] > 0) ]
        ax.plot(group[xName], group[yName], marker='o', linestyle='', ms=5, label=name)
    ax.legend(numpoints=1, loc='upper right')
    ax.set_xlabel(xName)
    ax.set_ylabel(yName)
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    plt.gca().get_xaxis().get_major_formatter().set_useOffset(False)
    plt.gca().get_yaxis().get_major_formatter().set_useOffset(False)
    plt.title(xName + ' vs ' + yName + ' for ' + operator + ' wells')
    plt.show()
    fig.savefig(xName + '_' + yName + '_' + operator + '.png')

def plotWellsforOperatorbyString(groups, xName, yName, operator):
    # Plot
    plt.rcParams.update(pd.tools.plotting.mpl_stylesheet)
    colors = pd.tools.plotting._get_standard_colors(len(groups), color_type='random')
    
    fig, ax = plt.subplots()
    fig.set_size_inches(18,13)
    ax.set_color_cycle(colors)
    ax.margins(0.05)
    for name, group in groups:
        group = group.loc[ (group[yName] > 0) ]        
        x = group[xName]
        y = group[yName]
        ax.plot(x, y, marker='o', linestyle='', ms=5, label=name)
        
    ax.set_color_cycle(colors)
    for name, group in groups:
        group = group.loc[ (group[yName] > 0) ]
        x = group[xName]
        y = group[yName]
        # calc the trendline (it is simply a linear fitting)
#        if (y.empty == False):
        z = np.polyfit(x, y, 1)
        p = np.poly1d(z)
        ax.plot(x, p(x), ':')
        
    ax.legend(numpoints=1, loc='upper left')
    ax.set_xlabel(xName)
    ax.set_ylabel(yName)
#    ax.set_xlim(2000, 2015)
#    ax.set_ylim(9500, 11800)
    plt.gca().get_xaxis().get_major_formatter().set_useOffset(False)
    plt.title(xName + ' vs ' + yName + ' for ' + operator + ' operated wells')
    plt.show()
    fig.savefig(xName + '_' + yName + '_' + operator + '.png')

def plotLostMudsbyYearnString(groups, xName, yName, operator):
    # Plot
    plt.rcParams.update(pd.tools.plotting.mpl_stylesheet)
    colors = pd.tools.plotting._get_standard_colors(len(groups), color_type='random')
    
    fig, ax = plt.subplots()
    fig.set_size_inches(18,13)
    ax.set_color_cycle(colors)
    ax.margins(0.05)
    for name, group in groups:
        byYearMonth = group.groupby('year_month').agg({ yName:'mean' })
        byYearMonth[yName] *= 100
        print(byYearMonth)
        
        x = byYearMonth.index
        y = byYearMonth[yName]
        ax.plot(x, y, marker='o', linestyle='', ms=5, label=name)
        
    ax.set_color_cycle(colors)
    for name, group in groups:
        byYearMonth = group.groupby('year_month').agg({ yName:'mean' })
        
        x = byYearMonth.index
        y = byYearMonth[yName] * 100
        # calc the trendline (it is simply a linear fitting)
        z = np.polyfit(x, y, 1)
        p = np.poly1d(z)
        ax.plot(x, p(x), '--')
        
    ax.legend(numpoints=1, loc='upper left')
    ax.set_xlabel(xName)
    ax.set_ylabel(yName)
#    ax.set_xlim(2000, 2015)
#    ax.set_ylim(9500, 11800)
    plt.gca().get_xaxis().get_major_formatter().set_useOffset(False)
    plt.title(xName + ' vs ' + yName + ' for ' + operator + ' operated wells')
    plt.show()
    fig.savefig(xName + '_' + yName + '_' + operator + '.png')

def plotLostMudsbyYearOverall(data, xName, yName, operator):
    # Plot
    plt.rcParams.update(pd.tools.plotting.mpl_stylesheet)
    colors = pd.tools.plotting._get_standard_colors(len(groups), color_type='random')
    
    fig, ax = plt.subplots()
    fig.set_size_inches(18,13)
    ax.set_color_cycle(colors)
    ax.margins(0.05)
#    for name, group in groups:
    byYearMonth = data.groupby('year_month').agg({ yName:'mean' })
    byYearMonth[yName] *= 100
#    print(byYearMonth)
    
    x = byYearMonth.index
    y = byYearMonth[yName]
    ax.plot(x, y, marker='o', linestyle='', ms=5) #, label=name
        
    ax.set_color_cycle(colors)
#    for name, group in groups:
#        byYearMonth = group.groupby('year_month').agg({ yName:'mean' })
        
#    x = byYearMonth.index
#    y = byYearMonth[yName] * 100
    # calc the trendline (it is simply a linear fitting)
    z = np.polyfit(x, y, 1)
    p = np.poly1d(z)
    ax.plot(x, p(x), '--')
        
    ax.legend(numpoints=1, loc='upper left')
    ax.set_xlabel(xName)
    ax.set_ylabel(yName)
#    ax.set_xlim(2000, 2015)
#    ax.set_ylim(9500, 11800)
    plt.gca().get_xaxis().get_major_formatter().set_useOffset(False)
    plt.title(xName + ' vs ' + yName + ' for ' + operator + ' operated wells')
    plt.show()
    fig.savefig(xName + '_' + yName + '_' + operator + '.png')

#henry = formations.loc[ (formations['OPERATOR'] == 'Henry Petroleum LP') ]
#cog = formations.loc[ (formations['OPERATOR'] == 'COG Operating LLC') ]
#smenergy = formations.loc[ (formations['OPERATOR'] == 'SM Energy') ]
#summit = formations.loc[ (formations['OPERATOR'] == 'Summit Petroleum, LLC') ]

#d = summit

## plotting mud & drill bit features with time for operators by string
#groups = d.groupby('string')
#for name in names:
#    plotWellsforOperatorbyString(groups, 'year_month', name, 'summit')
#
## plotting counts of mud loss events with time for operators by string
#plotLostMudsbyYearnString(groups, 'year_month', 'Circulation', 'summit')
#
## plotting counts of mud loss events with time for operators overall
#plotLostMudsbyYearOverall(d, 'year_month', 'Circulation', 'cog-all')

## plotting each operator on map by lat/long by year
#groups = d.groupby('year')
#plotWellsByGrp(groups, 'LONGITUDE', 'LATITUDE', 'summit', [-102.35, -101.85], [31.25, 31.8 ])

# remove wells that have TVDs < 8500
aggregated = aggregated.loc[ aggregated['TVD'] > 8000]

aggregated.to_csv(aggrfilename)

# plotting TVD box plots
henry = aggregated.loc[ (aggregated['OPERATOR'] == 'Henry Petroleum LP') ]
cog = aggregated.loc[ (aggregated['OPERATOR'] == 'COG Operating LLC') ]
smenergy = aggregated.loc[ (aggregated['OPERATOR'] == 'SM Energy') ]
summit = aggregated.loc[ (aggregated['OPERATOR'] == 'Summit Petroleum, LLC') ]

d = smenergy
ax = d[['TVD', 'year']].boxplot(by='year', figsize=(10, 8))
ax.set_ylim(8500, 18000)
##groups2 = twos.groupby('OPERATOR')
##groups3 = threes.groupby('OPERATOR')
#for name in names:
#    plotWellsforOperatorbyString(groups, 'year_month', name, 'summit')
##    plotWellsByGrp(groups, 'year_month', name, 'all')
##    plotWellsByGrp(groups2, 'year_month', name, '2')
##    plotWellsByGrp(groups3, 'year_month', name, '3')
