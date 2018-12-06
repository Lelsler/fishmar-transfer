#### Laura Elsler: April 2017
import numpy as np
import matplotlib.pyplot as plt
import random
from scipy.integrate import quad
import matplotlib.gridspec as gridspec
import seaborn as sns
import scipy

cooperative = 1 # 0 = no effect of cooperative level on effort; 1 = effect of cooperative level on effort
### Parameters #################################################################
tmax = 200 # model runs
g = 0.2 #
K = 100 #
q = 0.03 #
alpha = 10 #
beta = 1. #
gamma = .25 #
c = 0.4
delta = .9
epsilon = 1.5
k = 0.1 # cooperative functionality
m = 40 # cooperative functionality
o = 50 # cooperative effort

# convex price curve
# gamma = 1. # demand
# beta = 0.05 # price flexibility
# def P1_l(x, b, d):
#     P = d * np.exp(-b*x) # price species
#     return P

### Variables ##################################################################
# species
N = np.zeros(tmax) # biomass
# catch
H = np.zeros(tmax) # catch species
# effort
E = np.zeros(tmax) # effort
EC = np.zeros(tmax) # effort coop
EN = np.zeros(tmax) # effort non-coop
# Income and revenue for fisher
P = np.zeros(tmax) # price
I = np.zeros(tmax) # fisher income
R = np.zeros(tmax) # total revenue
# cooperative functionality
F = np.zeros(tmax) # cooperative functionality

### Initial values #############################################################
N[0] = 100 #
E[0] = 0.5 #
EN[0] = 0.5 #
EC[0] = 0.5 #
H[0] = 2 #
P[0] = 2 #

### Define Model ###############################################################

def model(g, K, q, alpha, beta, gamma, c, k, o):
    for t in np.arange(0,tmax-1):
        # population logistic growth dynamics
        N[t+1] = N[t] + g*N[t] * (1- (N[t]/K)) -H[t]
        print N[t], "N"

        # fishing effort
        EN[t+1] = EN[t]*np.exp(alpha*(gamma**(1/beta))*((q*EN[t]*N[t])**((beta-1)/beta))-c*EN[t])
        EC[t+1] = EC[t]*np.exp(alpha*(gamma**(1/beta))*((q*EC[t]*N[t])**((beta-1)/beta))-c*EC[t])
        if cooperative == 0:
            E[t+1] = E[t]*np.exp(alpha*(gamma**(1/beta))*((q*E[t]*N[t])**((beta-1)/beta))-c*E[t])
        if cooperative == 1:
            E[t+1] = F[t]*EC[t+1] + (1-F[t])*EN[t+1]
        print E[t], "E"

        # price
        P[t] = 5+ gamma* (H[t])**(-beta)
        if P[t] >= 10:
            P[t]= 10
        if P[t] < 1:
            P[t]= 1
        print P[t], "P"

        # catch
        H[t] = q*E[t]*N[t]
        print H[t], "H"

        # income
        I[t] = H[t] *P[t] - (E[t] *c)
        print I[t], "I"
        R[t] = H[t] *P[t]

        # cooperative functionality
        F[t]= 1/(1+np.exp(-k*(I[t]-o))) # cooperative functionality is high if income is high
        print F[t], "F"

    return N, E, H, I, R, F, EN, EC # output variables

##### Run the model ############################################################

OUT1 = np.zeros(I.shape[0])
OUT2 = np.zeros(I.shape[0])
OUT3 = np.zeros(I.shape[0])
OUT4 = np.zeros(I.shape[0])
OUT5 = np.zeros(I.shape[0])
OUT6 = np.zeros(I.shape[0])

for i in np.arange(0,tmax):
        N, E, H, I, R, F, EN, EC = model(g, K, q, alpha, beta, gamma, c, k, o)
        OUT1[i]= N[i]
        OUT2[i]= E[i]
        OUT3[i]= H[i]
        OUT4[i]= I[i]
        OUT5[i]= R[i]
        OUT6[i]= F[i]

# np.save("./Desktop/RQ3_N.npy", OUT1)

#####! PLOT ORIGINAL MODEL

fig = plt.figure()
plt.plot(N)
plt.plot(H)
plt.xlim(0,tmax-3)
plt.title("TEST",fontsize=17)
plt.xlabel("Time period",fontsize=15)
plt.ylabel("Species biomass",fontsize=15)
plt.legend(['biomass', 'catch'], loc='best')
#fig.savefig('fish.png',dpi=300)
plt.show()
