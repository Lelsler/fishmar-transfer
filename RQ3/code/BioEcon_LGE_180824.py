#### Laura Elsler: April 2017
import numpy as np
import matplotlib.pyplot as plt
import random
from scipy.integrate import quad
import matplotlib.gridspec as gridspec
import seaborn as sns
import scipy

#### Parameters (constants)
tmax = 100 # number of time steps to run model

# species dynamics
r = 0.5 # refers to the maximum rate of population growth
K = 80 # carrying capacity

# fishing
q = 1 # catchability coefficient

# price and income
p = 1 # fixed constant price
c = 0.5 # fixed cost per trip
beta = 1. #
gamma = .25 #

# cooperative functionality
m = 1

# convex price curve
# gamma = 1. # demand
# beta = 0.05 # price flexibility
# def P1_l(x, b, d):
#     P = d * np.exp(-b*x) # price species
#     return P

#### Initialize variables
# species
N = np.zeros(tmax) # biomass

# catch
C = np.zeros(tmax) # catch species

# Income and revenue for fisher
I = np.zeros(tmax) # total income
R = np.zeros(tmax) # total revenue

# effort
E = np.zeros(tmax)

# cooperative functionality
F = np.zeros(tmax) # cooperative functionality

# initial set-up
C[0] = 0.1
N[0] = 100
E[0] = 1

## define model
def model(r, K, q, gamma, beta, p, c, m):
    for t in np.arange(0,tmax-1):
        # population logistic growth dynamics
        N[t+1] = N[t] + r* N[t]*((K-N[t])/ K) - C[t]

        # fishing effort
        E[t+1]= E[t] + C[t] *p - E[t] *c #*F[t] #

        # catch
        C[t] = N[t] *E[t] *q # catch given effort and population size

        # income
        I[t] = C[t] *p - (E[t] *c)
        R[t] = C[t] *p

        # cooperative functionality
        F[t]= m *(np.exp(I[t])) # cooperative functionality is high if income is high

    return N, E, C, I, R, F # output variables

# SINGLE PARAMETER

OUT1 = np.zeros(I.shape[0])
OUT2 = np.zeros(I.shape[0])
OUT3 = np.zeros(I.shape[0])
OUT4 = np.zeros(I.shape[0])
OUT5 = np.zeros(I.shape[0])
OUT6 = np.zeros(I.shape[0])

for i in np.arange(0,tmax):
        N, E, C, I, R, F = model(r, K, q, gamma, beta, p, c, m)
        OUT1[i]= N[i]
        OUT2[i]= E[i]
        OUT3[i]= C[i]
        OUT4[i]= I[i]
        OUT5[i]= R[i]
        OUT6[i]= F[i]

# np.save("./Dropbox/PhD/OUT1.npy", OUT1)

#####! PLOT ORIGINAL MODEL
fig = plt.figure()
plt.plot(OUT1)
plt.plot(OUT3)
plt.xlim(0,tmax-3)
plt.title("TEST",fontsize=17)
plt.xlabel("Time period",fontsize=15)
plt.ylabel("Species biomass",fontsize=15)
plt.legend(['biomass', 'catch'], loc='best')
#fig.savefig('fish.png',dpi=300)
plt.show()
