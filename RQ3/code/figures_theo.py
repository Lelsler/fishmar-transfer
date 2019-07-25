import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import os

# read data
df1 = pd.read_csv('/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/mexico/processed/laura/data_monthly_lge.csv')
coop = df1['coop_id']
name = df1['coop_name']
funct = df1['functionality']
year = df1['year']
month = df1['month']
stock_abalone = df1['stock_abalone']
stock_clams = df1['stock_clams']
stock_lobsters = df1['stock_lobsters']
stock_seacucumber = df1['stock_seacucumber']
stock_snails = df1['stock_snails']
m_abalone = df1['m_abalone']
m_clams = df1['m_clams']
m_lobsters = df1['m_lobsters']
m_seacucumber = df1['m_seacucumber']
m_snails = df1['m_snails']
bmsy_abalone = df1['Bmsy_abalone']
bmsy_clams = df1['Bmsy_clams']
bmsy_lobsters = df1['Bmsy_lobsters']
bmsy_seacucumber = df1['Bmsy_seacucumber']
bmsy_snails = df1['Bmsy_snails']
fmsy_abalone = df1['Fmsy_abalone']
fmsy_clams = df1['Fmsy_clams']
fmsy_lobsters = df1['Fmsy_lobsters']
fmsy_seacucumber = df1['Fmsy_seacucumber']
fmsy_snails = df1['Fmsy_snails']

df2 = pd.read_csv('/Users/lauraelsler/Documents/MATLAB/solutions_open_rq3.csv') # y: solutions open access
df3 = pd.read_csv('/Users/lauraelsler/Documents/MATLAB/solutions_exclusive_rq3.csv') # y: solutions exlusive access

# transpose and transform to numpy arrays
df2= df2.transpose()
df2=df2.values
# df2=np.nan_to_num(df2)
df3= df3.transpose()
df3=df3.values
# df3=np.nan_to_num(df3)

# set working directory
os.chdir("/Users/lauraelsler/Documents/SESYNC/GIT/fishmar/RQ3/")

# font in plot
hfont = {'fontname':'Helvetica'}
plt.ylabel("catch $t$",fontsize=20, **hfont)

###! Scatter plot only data
fig = plt.figure()
ax1 = fig.add_subplot(111)
plt.scatter((m_lobsters), np.log2(stock_lobsters),c=(funct), s=30, marker="s", cmap='viridis')
plt.title("lobsters", fontsize= 25, **hfont)
plt.xlabel("reference price",fontsize=20, **hfont)
plt.ylabel("log2(b/bmsy)",fontsize=20, **hfont)
plt.ylim(-1.5,1)
plt.xlim(0,1E6)
cb = plt.colorbar()
cb.set_label('functionality', rotation=270, labelpad=40, fontsize = 22, **hfont)
cb.ax.tick_params(labelsize=12)
# fig.savefig('./figures/lobsters_sm_log2.png',dpi=200)
plt.show()

fig = plt.figure()
ax1 = fig.add_subplot(111)
plt.scatter((m_lobsters),(stock_lobsters),c=(funct), s=30, marker="s", cmap='viridis')
plt.title("lobsters", fontsize= 25, **hfont)
plt.xlabel("reference price",fontsize=20, **hfont)
plt.ylabel("b/bmsy",fontsize=20, **hfont)
# plt.ylim(-1.5,1)
plt.xlim(0,1E6)
cb = plt.colorbar()
cb.set_label('functionality', rotation=270, labelpad=40, fontsize = 22, **hfont)
cb.ax.tick_params(labelsize=12)
# fig.savefig('./figures/lobsters_sm.png',dpi=200)
plt.show()

###! Scatter plot data and model
x = np.linspace(0, 5E5, 101) # x axis

fig = plt.figure()
fig.subplots_adjust(bottom=0.15, left= 0.15)
# add the first axes using subplot populated with predictions
ax1 = fig.add_subplot(111)
plt.scatter(m_lobsters, np.log2(stock_lobsters/bmsy_lobsters),c=(funct), s=30, marker="s", cmap='viridis')
# add the second axes using subplot with
ax2 = fig.add_subplot(111, sharex=ax1, frameon=False)
line1, = ax2.plot(x,df2[:,0], color="red", linewidth=3)
line2, = ax2.plot(x,df2[:,1], color="red",linewidth=3)
line3, = ax2.plot(x,df3[:,0], color="black",linewidth=3)
line4, = ax2.plot(x,df3[:,1], color="black",linewidth=3)
# x-axis
# ax1.set_xticklabels(np.arange(2001,2016,2), rotation=45, fontsize= 14)
# ax1.set_xlim(0,5E5)
# ax1.set_xlabel("Year",fontsize=20, **hfont)
# ax2.set_xticklabels(np.arange(2001,2016,2), rotation=45, fontsize= 14)
# ax2.set_xlim(10,tmax-2)
ax2.set_xlabel("reference price",fontsize=20, **hfont)
# ax2.yaxis.tick_right()
# ax2.yaxis.set_label_position("right")
# # y-axis
ax1.set_ylabel("B/Bmsy", rotation=90, labelpad=5, fontsize=20, **hfont)
ax1.set_ylim(-1,6)
# ax1.tick_params(axis='y', labelsize=14)
# ax2.set_ylabel("Mantle length $cm$", rotation=270, color='silver', labelpad=22, fontsize=20, **hfont)
ax2.yaxis.tick_right()
ax2.yaxis.set_label_position("right")
# ax2.tick_params(axis='y', colors='silver', labelsize=14)
# ax2.set_ylim(-1,2)
plt.gcf().subplots_adjust(bottom=0.15,right=0.9)
# legend
plt.title('lobsters', fontsize=20, **hfont)
plt.legend([line1, line3], ["open access", "exclusive access"], loc=1, fontsize= 12)
#colorbar
# cb = plt.colorbar()
# cb.set_label('functionality', rotation=270, labelpad=40, fontsize = 22, **hfont)
# cb.ax.tick_params(labelsize=12)
# save and show
# fig.savefig('./figures/lobsters_sm_dp.png',dpi=300)
plt.show()
