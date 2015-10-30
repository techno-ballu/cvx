# -*- coding: utf-8 -*-
"""
Created on Sat Mar 21 14:56:18 2015

@author: bolaka
"""

## imports
#from cvxtextproject import *
#from mlclassificationlibs import *
#import pandas as pd
#import matplotlib.pyplot as plt
#
#setPath()

## extract the names of the pandas data frame
#def names(pandas_dataframe):
#    return (pandas_dataframe.columns.values.tolist())
#
setPath('/home/bolaka/CVX text/histograms')
    
#normalized_depth = 10500
barWidth=1
#idCol = 'WELLID'

# define colors
colorCOOP2 = '#FBA073'  # COOP 2-string - light orange
colorCOOP3 = '#F12D0A'  # COOP 3-string - dark orange
colorNOJV2 = '#9ECAF5'  # NOJV 2-string - light blue
colorNOJV3 = '#0A34F1' # NOJV 3-string - deep blue

#coop2filename = 'test Code-COOP-2string.csv'
#coop3filename = 'test Code-COOP-3string.csv'

# Code timelines
#nojv2 = pd.read_csv(nojv2filenameCode,  index_col=idCol) 
#nojv3 = pd.read_csv(nojv3filenameCode,  index_col=idCol) 

# ML timelines
#nojv2 = pd.read_csv(nojv2filenameML,  index_col=idCol) 
#nojv3 = pd.read_csv(nojv3filenameML,  index_col=idCol) 
#
#coop2 = pd.read_csv(coop2filename,  index_col=idCol) 
#coop3 = pd.read_csv(coop3filename,  index_col=idCol) 
#
#
#
#drillCOOP2Times = drawTimelines('COOP',coop2, False)
#drillCOOP3Times = drawTimelines('COOP',coop3, False)


# histograms
total = 'Total Drilling Times'
surf = 'Normalized drill time for surface string'
inter = 'Normalized drill time for intermediate string'
prod = 'Normalized drill time for production string'

# COOP 2 vs COOP 3 for total drill times
series1 = drillCOOP2Times[total]
series2 = drillCOOP3Times[total]
plt.hist(series1, bins=20, color=colorCOOP2, label='Total COOP 2',width=barWidth) #
plt.hist(series2, bins=20, color=colorCOOP3, alpha=0.5, label='Total COOP 3',width=barWidth) #
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
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='Total NOJV 2',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='Total NOJV 3',width=barWidth)
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
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='Total NOJV 2',width=barWidth)
plt.hist(series2, bins=20, color=colorCOOP2, alpha=0.5, label='Total COOP 2',width=barWidth)
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
plt.hist(series1, bins=20, color=colorNOJV3, alpha=0.5, label='Total NOJV 3',width=barWidth)
plt.hist(series2, bins=20, color=colorCOOP3, alpha=0.5, label='Total COOP 3',width=barWidth)
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
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='2-string all wells for total',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='3-string all wells for total',width=barWidth)
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
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='COOP all wells for total',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='NOJV all wells for total',width=barWidth)
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
plt.hist(series1, bins=20, color=colorNOJV2, alpha=0.5, label='2-string all wells for surface',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='3-string all wells for surface',width=barWidth)
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
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='COOP all wells for surface',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='NOJV all wells for surface',width=barWidth)
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
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='COOP wells for intermediate',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='NOJV wells for intermediate',width=barWidth)
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
plt.hist(series1, bins=20, color=colorCOOP2, alpha=0.5, label='2-string COOP wells for production',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV2, alpha=0.5, label='2-string NOJV wells for production',width=barWidth)
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
plt.hist(series1, bins=20, color=colorCOOP3, alpha=0.5, label='3-string COOP wells for production',width=barWidth)
plt.hist(series2, bins=20, color=colorNOJV3, alpha=0.5, label='3-string NOJV wells for production',width=barWidth)
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

