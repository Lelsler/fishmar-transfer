import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
from pylab import *
import os

print(os.getcwd())

###### LOAD DATA ###############################################################
# loading the new links file
df1 = pd.read_csv('./Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/newlinks_covariates.csv')
# load columns country from new links file
iso1 = df1['country']
comm = df1['commodity']
imp = df1['new.imports']

# loading the nodelist file
df2 = pd.read_csv('./Documents/SESYNC/Files/FISHMAR-data/rq2/test_preferential/nodeList1.csv')
# load columns country from new links file
iso2 = df2['country']

# sort for commodity
cav = df1[df1.commodity == "Anchovies fresh or chilled"]

# merge the two dataframes by country
vok= pd.merge(cav,df2,on= "country")
vok.head()

# begin calculations for PA
number = vok['new.imports'].sum()
