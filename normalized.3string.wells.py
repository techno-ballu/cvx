# -*- coding: utf-8 -*-
"""
Created on Thu Mar 12 11:44:49 2015

@author: bolaka
"""

# imports
import os
import pandas as pd
import matplotlib.pyplot as plt

# extract the names of the pandas data frame
def names(pandas_dataframe):
    return (pandas_dataframe.columns.values.tolist())

results = '/deliverables/13March'

directory = '/home/bolaka/CVX text/' + results

if not os.path.exists(directory):
    os.makedirs(directory)

# setting work directory
os.chdir(directory)
    
normalized_depth = 10500
idCol = 'WellID'

# define colors
colorCOOP2 = '#FBA073'  # COOP 2-string - light orange
colorCOOP3 = '#F12D0A'  # COOP 3-string - dark orange
colorNOJV2 = '#9ECAF5'  # NOJV 2-string - light blue
colorNOJV3 = '#0A34F1' # NOJV 3-string - deep blue

def drawTimelines(owner, data, onlyPerfect):
    print('Processing ' + owner + ' wells:')
    
    # drop rows with missing
    data = data.dropna()
    
    # drop duplicate wells
    data[idCol] = data.index
    data.drop_duplicates(subset=idCol, take_last=True, inplace=True)
    del data[idCol]
    
    colnames = names(data)
    rowslen = str(len(data.index))
    
    isThreeString = False
    if any("Inter" in s for s in colnames):
        isThreeString = True
        
    print(rowslen,isThreeString)

    # set the colors based on string & owner
    if owner == 'COOP' and isThreeString:
        groupColor = colorCOOP3
    elif owner == 'COOP' and isThreeString != True:
        groupColor = colorCOOP2
    elif owner == 'NOJV' and isThreeString: 
        groupColor = colorNOJV3
    elif owner == 'NOJV' and isThreeString != True:
        groupColor = colorNOJV2
        
    # the normalized drilling times
    normSDtimes = []
    normPDtimes = []
    if isThreeString:
        normIDtimes = []
    totalDtimes = []
        
    # the normalized drilling TVDs
    normSDtvds = []
    normPDtvds = []
    if isThreeString:
        normIDtvds = []
        
    # normalize time and TVD per well    
    for index, row in data.iterrows():
        
        # normalize drilling TVDs
        try:
            # normalization factor
            scale = normalized_depth/row['TVD']
            surfaceTVD = float(row['Surf string TVD'])
            productionTVD = float(row['Prod string TVD'])
            if isThreeString:
                interTVD = float(row['Inter string TVD'])
                
        except ValueError:
            print(row)
            
        # normalize drilling times
        normSDtimes.append(row['Surf D days'] * scale)
        normPDtimes.append(row['Prod D days'] * scale)
        if isThreeString:
            normIDtimes.append(row['Inter D days'] * scale)
            totalDtimes.append(row['Surf D days'] * scale + row['Prod D days'] * scale + row['Inter D days'] * scale)
        else:
            totalDtimes.append(row['Surf D days'] * scale + row['Prod D days'] * scale)
            
        normSDtvds.append(surfaceTVD * scale)
        normPDtvds.append(productionTVD * scale)
        if isThreeString:
            normIDtvds.append(interTVD * scale)
            
    # add the new normalized drilling times to data
    data['normalized surface drill times'] = normSDtimes
    if isThreeString:
        data['normalized intermediate drill times'] = normIDtimes
    data['normalized production drill times'] = normPDtimes
    data['Total Drilling Times'] = totalDtimes
    
    # add the new normalized drilling TVDs to data
    data['normalized surface drill TVDs'] = normSDtvds
    if isThreeString:
        data['normalized intermediate drill TVDs'] = normIDtvds
    data['normalized production drill TVDs'] = normPDtvds
    
#    directory = os.getcwd() + '/' + plots
#    if not os.path.exists(directory):
#        os.makedirs(directory)
#    os.chdir(directory)
    
    if onlyPerfect == False:
        # plot the timelines
        count = 0
        for index, row in data.iterrows():
        
            if any(pd.isnull(row)):
                continue
        
            x = []
            x.append(0)
            x.append(row['normalized surface drill times'])
            x.append(x[len(x)-1] + row['Surf C days'])
            if isThreeString:
                x.append(x[len(x)-1] + row['normalized intermediate drill times'])
                x.append(x[len(x)-1] + row['Inter C days'])
            x.append(x[len(x)-1] + row['normalized production drill times'])
            x.append(x[len(x)-1] + row['Prod C days'])
            
            y = []
            y.append(0)
            y.append(-row['normalized surface drill TVDs'])
            y.append(-row['normalized surface drill TVDs'])
            if isThreeString:
                y.append(-row['normalized intermediate drill TVDs'])
                y.append(-row['normalized intermediate drill TVDs'])
            y.append(-row['normalized production drill TVDs'])
            y.append(-row['normalized production drill TVDs'])
            
            if isThreeString:
                label = rowslen + ' ' + owner + ' 3-string wells'
            else:
                label = rowslen + ' ' + owner + ' 2-string wells'
            if count == 1:
                plt.plot(x, y, linestyle='--', color=groupColor, label=label)  
            else:
                plt.plot(x, y, linestyle='--', color=groupColor)
            
            count += 1
    
    # plotting the perfect well
    bestSDwell = data.ix[data['normalized surface drill times'].argmin()]
    bestSDtime = bestSDwell['normalized surface drill times']
    bestStvd = bestSDwell['normalized surface drill TVDs']
    bestSCtime = min(data['Surf C days'])
    if isThreeString:
        bestIDwell = data.ix[data['normalized intermediate drill times'].argmin()]
        bestIDtime = bestIDwell['normalized intermediate drill times']
        bestItvd = bestIDwell['normalized intermediate drill TVDs']
        bestICtime = min(data['Inter C days'])
        
    bestPDwell = data.ix[data['normalized production drill times'].argmin()]
    bestPDtime = bestPDwell['normalized production drill times']
    bestPtvd = bestPDwell['normalized production drill TVDs']
    bestPCtime = min(data['Prod C days'])
    
    x = []
    x.append(0)
    x.append(bestSDtime)
    x.append(x[len(x)-1] + bestSCtime)
    if isThreeString:
        x.append(x[len(x)-1] + bestIDtime)
        x.append(x[len(x)-1] + bestICtime)
    x.append(x[len(x)-1] + bestPDtime)
    x.append(x[len(x)-1] + bestPCtime)
    
    y = []
    y.append(0)
    y.append(-bestStvd)
    y.append(-bestStvd)
    if isThreeString:
        y.append(-bestItvd)
        y.append(-bestItvd)
    y.append(-bestPtvd)
    y.append(-bestPtvd)
    
    if isThreeString:
        label = 'Perfect Well ' + owner + ' 3-string'
    else:
        label = 'Perfect Well ' + owner + ' 2-string'
    plt.plot(x, y, color=groupColor, linewidth=3, label = label)
    
    plt.xlabel('Time (days)')
    plt.ylabel('Depth (feet)')
    plt.title('Time/depth plots')
    plt.legend()
    
    plotTitle = 'groupNperfect-well-' + owner + '3-string'
    if isThreeString and onlyPerfect:
        plotTitle = 'perfect-well-' + owner + '3-string'    
    elif isThreeString != True and onlyPerfect:
        plotTitle = 'perfect-well-' + owner + '2-string'
    elif isThreeString != True and onlyPerfect != True:
        plotTitle = 'groupNperfect-well-' + owner + '2-string'
    
    F = plt.gcf() 
    DefaultSize = F.get_size_inches()
    F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
    F.savefig(plotTitle + '.png', dpi = (200)) 
    print("Saving " + plotTitle)
    plt.show()
    
    # rename the columns and save the data
    mappings = {
        'Surf D days' : 'Actual Drilling time for surface string (days)',
        'Prod D days' : 'Actual Drilling time for production string (days)',
        'Surf C days' : 'Actual Case and cement time for surface string',
        'Prod C days' : 'Actual Case and cement time for production string',
        'Surf string TVD' : 'Actual Drilling depth for surface string (feet)',
        'Prod string TVD' : 'Actual Drilling depth for production string (feet)',
        'Inter D days' : 'Actual Drilling time for intermediate string (days)',
        'Inter C days' : 'Actual Case and cement time for intermediate string',
        'Inter string TVD' : 'Actual Drilling depth for intermediate string (feet)',
        'normalized surface drill times' : 'Normalized drill time for surface string',
        'normalized intermediate drill times' : 'Normalized drill time for intermediate string',
        'normalized production drill times' : 'Normalized drill time for production string',
        'normalized surface drill TVDs' : 'Normalized drill depth for surface string',
        'normalized intermediate drill TVDs' : 'Normalized drill depth for intermediate string',
        'normalized production drill TVDs' : 'Normalized drill depth for production string'
    };
    data = data.rename(columns=mappings)
    
    # reorder the columns
    colorder3 = ['TVD','Total Drilling Times','Actual Drilling time for surface string (days)',
                'Actual Drilling depth for surface string (feet)',
                'Actual Case and cement time for surface string',
                'Actual Drilling time for intermediate string (days)',
                'Actual Drilling depth for intermediate string (feet)',
                'Actual Case and cement time for intermediate string',
                'Actual Drilling time for production string (days)',
                'Actual Drilling depth for production string (feet)',
                'Actual Case and cement time for production string',
                'Normalized drill time for surface string',
                'Normalized drill depth for surface string',
                'Normalized drill time for intermediate string',
                'Normalized drill depth for intermediate string',
                'Normalized drill time for production string',
                'Normalized drill depth for production string']
        
    colorder2 = ['TVD','Total Drilling Times','Actual Drilling time for surface string (days)',
                'Actual Drilling depth for surface string (feet)',
                'Actual Case and cement time for surface string',
                'Actual Drilling time for production string (days)',
                'Actual Drilling depth for production string (feet)',
                'Actual Case and cement time for production string',
                'Normalized drill time for surface string',
                'Normalized drill depth for surface string',
                'Normalized drill time for production string',
                'Normalized drill depth for production string']
    
    drillingTimes2 = ['Total Drilling Times','Normalized drill time for surface string',
                          'Normalized drill time for production string']
    drillingTimes3 = ['Total Drilling Times','Normalized drill time for surface string',
                          'Normalized drill time for intermediate string',
                          'Normalized drill time for production string']
                
    if owner == 'NOJV':
        colorder2.append('RIGIDENTIFIER')
        colorder3.append('RIGIDENTIFIER')
        
    if isThreeString:
        data = data[colorder3]    
    else:
        data = data[colorder2]    
    
    print('Successfully normalized the wells with new columns as ', names(data))
    
    if isThreeString:
        data.to_csv(owner + 'normalized3.csv', sep=',', encoding='utf-8')
    else:
        data.to_csv(owner + 'normalized2.csv', sep=',', encoding='utf-8')
    
    if isThreeString:
        times = data[drillingTimes3]    
    else:
        times = data[drillingTimes2]        
    
    return (times)

nojv2filename = 'nojv2timelinesnew.csv'
coop2filename = 'coop2timelinesnew.csv'
nojv3filename = 'nojv3timelinesnew.csv'
coop3filename = 'coop3timelinesnew.csv'

# header is conveniently inferred by default
nojv2 = pd.read_csv(nojv2filename,  index_col=idCol) 
coop2 = pd.read_csv(coop2filename,  index_col=idCol) 
nojv3 = pd.read_csv(nojv3filename,  index_col=idCol) 
coop3 = pd.read_csv(coop3filename,  index_col=idCol) 

drillNOJV2Times = drawTimelines('NOJV',nojv2, False)
drillNOJV3Times = drawTimelines('NOJV',nojv3, False)
drillCOOP2Times = drawTimelines('COOP',coop2, False)
drillCOOP3Times = drawTimelines('COOP',coop3, False)

# histograms
total = 'Total Drilling Times'
surf = 'Normalized drill time for surface string'
inter = 'Normalized drill time for intermediate string'
prod = 'Normalized drill time for production string'

# COOP 2 vs COOP 3 for total drill times
series1 = drillCOOP2Times[total]
series2 = drillCOOP3Times[total]
plt.hist(series1, bins=20, color=colorCOOP2, label='Total COOP 2')
plt.hist(series2, bins=20, color=colorCOOP3, alpha=0.5, label='Total COOP 3')
plt.title("COOP 2 vs COOP 3 for total drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('5a Histogram.png', dpi = (200)) 
plt.show()

# NOJV 2 vs NOJV 3 for total drill times
series1 = drillNOJV2Times[total]
series2 = drillNOJV3Times[total]
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='Total NOJV 2')
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='Total NOJV 3')
plt.title("NOJV 2 vs NOJV 3 for total drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('5b Histogram.png', dpi = (200)) 
plt.show()

# NOJV 2 vs COOP 2 for total drill times
series1 = drillNOJV2Times[total]
series2 = drillCOOP2Times[total]
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='Total NOJV 2')
plt.hist(series2, bins=20, color=colorCOOP2, alpha=0.5, label='Total COOP 3')
plt.title('NOJV 2 vs COOP 2 for total drill times')
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('5c Histogram.png', dpi = (200)) 
plt.show()

# NOJV 3 vs COOP 3 for total drill times
series1 = drillNOJV3Times[total]
series2 = drillCOOP3Times[total]
plt.hist(series1, bins=20, color=colorNOJV3, alpha=0.5, label='Total NOJV 3')
plt.hist(series2, bins=20, color=colorCOOP3, alpha=0.5, label='Total COOP 3')
plt.title("NOJV 3 vs COOP 3 for total drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('5d Histogram.png', dpi = (200)) 
plt.show()

# all wells 2-string vs 3-string for total drill times
series1 = drillNOJV2Times[total].append(drillCOOP2Times[total])
series2 = drillNOJV3Times[total].append(drillCOOP3Times[total])
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='2-string all wells for total')
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='3-string all wells for total')
plt.title("all wells 2-string vs 3-string for total drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('6ai Histogram.png', dpi = (200)) 
plt.show()

# all wells COOP vs NOJV for total drill times
series1 = drillCOOP2Times[total].append(drillCOOP3Times[total])
series2 = drillNOJV2Times[total].append(drillNOJV3Times[total])
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='COOP all wells for total')
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='NOJV all wells for total')
plt.title("all wells COOP vs NOJV for total drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('6aii Histogram.png', dpi = (200)) 
plt.show()

# all wells 2-string vs 3-string for surface drill times
series1 = drillNOJV2Times[surf].append(drillCOOP2Times[surf])
series2 = drillNOJV3Times[surf].append(drillCOOP3Times[surf])
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='2-string all wells for surface')
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='3-string all wells for surface')
plt.title("all wells 2-string vs 3-string for surface drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('6bi Histogram.png', dpi = (200)) 
plt.show()

# all wells COOP vs NOJV for surface drill times
series1 = drillCOOP2Times[surf].append(drillCOOP3Times[surf])
series2 = drillNOJV2Times[surf].append(drillNOJV3Times[surf])
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='COOP all wells for surface')
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='NOJV all wells for surface')
plt.title("all wells COOP vs NOJV for surface drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('6bii Histogram.png', dpi = (200)) 
plt.show()

# 3-string wells COOP vs NOJV for intermediate drill times
series1 = drillCOOP3Times[inter]
series2 = drillNOJV3Times[inter]
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='COOP wells for intermediate')
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='NOJV wells for intermediate')
plt.title("3-string wells COOP vs NOJV for intermediate drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('6c Histogram.png', dpi = (200))
plt.show()

# 2-string wells COOP vs NOJV for production drill times
series1 = drillCOOP2Times[prod]
series2 = drillNOJV2Times[prod]
plt.hist(series1, bins=20, color=colorCOOP2, alpha=0.5, label='2-string COOP wells for production')
plt.hist(series2, bins=20, color=colorNOJV2, alpha=0.5, label='2-string NOJV wells for production')
plt.title("2-string wells COOP vs NOJV for production drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('6d Histogram.png', dpi = (200))
plt.show()

# 3-string wells COOP vs NOJV for production drill times
series1 = drillCOOP3Times[prod]
series2 = drillNOJV3Times[prod]
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='3-string COOP wells for production')
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='3-string NOJV wells for production')
plt.title("3-string wells COOP vs NOJV for production drill times")
plt.xlabel("Time in days")
plt.ylabel("Rates of drilling times")
plt.legend()
F = plt.gcf() 
DefaultSize = F.get_size_inches()
F.set_size_inches( (DefaultSize[0]*2, DefaultSize[1]*2) )
F.savefig('6e Histogram.png', dpi = (200))
plt.show()

#inputExcel = 'AllWells.xlsx'
#nojv2 = pd.read_excel(inputExcel, sheetname='NOJV 2 string wells', index_col=idCol)
#nojv3 = pd.read_excel(inputExcel, sheetname='NOJV 3 string wells', index_col=idCol)
#coop2 = pd.read_excel(inputExcel, sheetname='COOP 2 string wells', index_col=idCol)
#coop3 = pd.read_excel(inputExcel, sheetname='COOP 3 string wells', index_col=idCol)

