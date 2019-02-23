import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
from pylab import *
import os
from scipy import stats

print(os.getcwd())

###### LOAD DATA ###############################################################
# loading the new links file
df1 = pd.read_csv('./Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/bilateral_newlinks_covariates.csv')
# load columns country from new links file
new_exp = df1['new.export']
new_imp = df1['new.import']
yr = df1['year']
imp = df1['imp_iso3']
exp = df1['imp_iso3']
spp = df1['our_group_name']
df1.head()

#### PLOT ######################################################################
fig = plt.figure()
ax1 = fig.add_subplot(111)
ax1.scatter(gov, exp, s=30, color='sage', marker="s", label='imports')
plt.title("Governance predicts new exports", fontsize= 25)
plt.xlabel("governance",fontsize=20)
plt.ylabel("new exports",fontsize=20)
plt.legend(loc="best", fontsize=10)
# add trendline
z = np.polyfit(gov, imp, 1)
p = np.poly1d(z)
plt.plot(gov,p(gov),color = 'red')
# fig.savefig('./Documents/SESYNC/GIT/fishmar/RQ2/figures/gov_exports.png',dpi=200)
plt.show()

# correlation and significance
#linear regression
slope, intercept, r_value, p_value, std_err = stats.linregress(gov, imp)
print("r-squared gov imports:", r_value**2)
# pearson significance of correlation
stats.pearsonr(gov,imp) # output is  correlation coefficient and the corresponding p-value
print 'slope:', slope, 'intercept:', intercept





















# loading the nodelist file
df2 = pd.read_csv('./Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/nodeList1.csv')
# load columns country from new links file
iso2 = df2['country']
df2.head()

# sort for commodity
cav = df1[df1.commodity == "Anchovies fresh or chilled"]

# merge the two dataframes by country
vok= pd.merge(cav,df2,on= "country")
vok.head()
np.save('./Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/links_nodes_covariates', vok)
fre = vok['frequency']
im2 = vok['new_imports']

# begin calculations for PA # add calculated columns to dataframe
number = im2.sum()
vok['pk'] = vok['new_imports']/number
vok['rk'] = vok['pk']/(fre/len(iso2))

vok =  vok.fillna(0)

# assign colums
pk = vok['pk']
rk = vok['rk']
nd = vok['nodeDegree']

#### PLOT ######################################################################
###! Scatter plot
fig = plt.figure()
ax1 = fig.add_subplot(111)
ax1.scatter(nd, rk, s=30, color='sage', marker="s", label='node degree')
plt.title("Preferential attachment imports", fontsize= 25)
plt.xlabel("node degree",fontsize=20)
plt.xlim(0,max(nd)+5)
plt.ylabel("relative prob. forming new links",fontsize=20)
plt.ylim(0,5)
plt.legend(loc="best", fontsize=10)
plt.annotate('rk  = -0.197 + 0.0439* node degree', xy=(2, 3), xytext=(2.9, 3.1), color = 'red')
plt.annotate('$r^2$ = 0.49', xy=(2, 2.7), xytext=(2.9, 2.9), color = 'black')
# add trendline
z = np.polyfit(nd, rk, 1)
p = np.poly1d(z)
plt.plot(nd,p(nd),color = 'red')
# fig.savefig('./Documents/SESYNC/GIT/fishmar/RQ2/figures/rk_nd_imports.png',dpi=200)
plt.show()

# correlation and significance
#linear regression
slope, intercept, r_value, p_value, std_err = stats.linregress(nd, rk)
print("r-squared PA imports:", r_value**2)
# pearson significance of correlation
stats.pearsonr(nd,rk) # output is  correlation coefficient and the corresponding p-value
print 'slope:', slope, 'intercept:', intercept
