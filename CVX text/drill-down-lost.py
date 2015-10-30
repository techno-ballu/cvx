# -*- coding: utf-8 -*-
"""
Created on Mon Apr 13 15:40:40 2015

@author: bolaka
"""

import os
os.chdir('/home/bolaka/python-workspace/CVX-timelines/')

# imports;
from cvxtextproject import *
from mlclassificationlibs import *

setPath('/home/bolaka/CVX text/COOP')

csvInput = 'depthVSothers.csv'

depthwise = pd.read_csv(csvInput, index_col='Depth end') 

#depthwise.plot(subplots=True) # x='Depth start',y=['MW','VIS'], secondary_y='VIS', 

fig, (ax1, ax2) = plt.subplots(nrows=2)
ax1.plot(depthwise.index, depthwise['MW'], 'k-')
ax1.set_ylabel('mud weight')
ax1.set_title('mud weight vs depth plot for a well (shaded region indicates mud loss)')
ax1.set_ylim([8,10])
ax1.fill_between(depthwise.index, depthwise['MW'], y2=0, where=depthwise['Lost Mud'], facecolor='blue', alpha=0.5)

ax2.plot(depthwise.index, depthwise['VIS'], 'k-')
ax2.set_ylabel('viscosity')
ax2.set_title('viscosity vs depth plot for a well (shaded region indicates mud loss)')
ax2.set_ylim([30,50])
ax2.fill_between(depthwise.index, depthwise['VIS'], y2=0, where=depthwise['Lost Mud'], facecolor='blue', alpha=0.5)
fig.tight_layout()

fig = plt.gcf()
fig.set_size_inches(11, 8)    #15
fig.set_dpi(300)