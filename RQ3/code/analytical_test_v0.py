from scipy.optimize import *
import numpy as np
from sympy.solvers import solve
from sympy import Symbol
import matplotlib.pyplot as plt

C=.2 #portion of cost
xi=1
R=1.4116
b=1
V=1 #marginal benefit scaling factor
r=2 #stock growth rate
a=1 #spp competition
p=1
v=0.2 #positive valuation of stock
Es=1
L2range = .2 #effort-based cost
Lrange = 2
v = 0

# Lrange = np.arange(0,2.) #volume-based cost
# L2range = np.array([0,.25,.5]) #effort-based cost
# Vrange = np.arange(0.01,5.0,0.1) #reference price
Vrange = np.arange(0.1,10.0,0.1) #reference price
#Fsol_all = np.ones(len(Vrange))*(-100) #harvest rate solutions
#Fsol_all = np.ndarray(shape=(len(Vrange),3))*(-100)
Fsol_all = np.zeros(shape=(len(Vrange),3))-100
# variables
Fsol = np.zeros(3)-100

# define symbol
F = Symbol('F')

def excl_access(L2,L,r,a,v,V):
    du = (L2 - L*((F - r)/a + (F)/a) + v/a - (V*a*((F - r)/a + (F)/a))/(F*(F - r))) # %linear harvest cost + effort cost, slow institution exclusive access
    Fs = solve(du, F)
    Fs = np.array(Fs)
    return Fs

for i in np.arange(len(Vrange)):
        V = Vrange[i]
        L = Lrange
        L2 = L2range
        Fs = excl_access(L2,L,r,a,v,V)
        # Fs= -100 if Fs < 0 else Fs = Fs
        Fsol_all[[i],0:len(Fs)] = Fs
        # #evaluate data type if imaginary then = -100
        # if Fsol == complex():
        #     print "compl"
        #     Fsol = -100
        # if Fsol < 1:
        #     Fsol = -100
        #      #if randombool == True:np.nan

Fa = Fsol_all[:,0] # equilibrium solution one
Fb = Fsol_all[:,1] # equilibrium solution two
Fc = Fsol_all[:,2] # # equilibrium solution three

plt.scatter(Vrange,Fa,color='r') # plot matrix Fsol_all
plt.scatter(Vrange,Fb,color='b') # plot matrix Fsol_all
plt.scatter(Vrange,Fc,color='y') # plot matrix Fsol_all
plt.ylim(-1,1)
# plt.savefig('./Documents/SESYNC/GIT/fishmar/RQ3/figures/equilirbium_solutions.png',dpi=500)
plt.show()
