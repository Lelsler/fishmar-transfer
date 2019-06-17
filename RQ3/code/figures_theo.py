import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import os

# read data
df1 = pd.read_csv('/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data//mexico/processed/laura/data_monthly_lge.csv')
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

# set working directory
os.chdir("/Users/lauraelsler/Documents/SESYNC/GIT/fishmar/RQ3/")

# font in plot
hfont = {'fontname':'Helvetica'}
plt.ylabel("catch $t$",fontsize=20, **hfont)

###! Scatter plot
fig = plt.figure()
ax1 = fig.add_subplot(111)
plt.scatter(m_abalone, np.log2(stock_abalone),c=(funct), s=30, marker="s", cmap='viridis')
plt.title("abalone", fontsize= 25, **hfont)
plt.xlabel("marginal benefit",fontsize=20, **hfont)
plt.ylabel("stock",fontsize=20, **hfont)
plt.ylim(-3,1)
plt.xlim(0,1E6)
cb = plt.colorbar()
cb.set_label('functionality', rotation=270, labelpad=40, fontsize = 22, **hfont)
cb.ax.tick_params(labelsize=12)
# fig.savefig('./figures/abalone_sm.png',dpi=200)
plt.show()
