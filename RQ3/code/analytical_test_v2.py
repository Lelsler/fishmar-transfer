# plot solutions to harvest rate evolution for Pella-Tomlnson growth curve

from scipy.optimize import *
import numpy as np
from sympy.solvers import solve
from sympy.solvers import nsolve
from sympy import Symbol
import matplotlib.pyplot as plt
#from scipy.optimize import fsolve

V=1 #marginal benefit scaling factor
r=2 #stock growth rate
a=1 #spp competition
v=0.2 #positive valuation of stock
Lrange = 2 #harvest-volume dependent cost #in org 0 or 2
L2range = 0.2 #marginal effort dependent cost #in org 0 or .2

# Lrange = np.arange(0,2.) #harvest-volume dependent cost
# L2range = np.array([0,.25,.5]) #marginal effort dependent cost
# Vrange = np.arange(0.01,5.0,0.1) #reference price alt1
Vrange = np.arange(0.1,10.0,0.1) #reference price alt2
Fsol_excl = np.zeros(shape=(len(Vrange),3))-100 #matrix fit exclusive access solutions
Fsol_open = np.zeros(shape=(len(Vrange),3))-100 #matrix fit open access solutions

# define symbol
F = Symbol('F')

def excl_access(L2,L,r,a,v,V):
    #slow institution exclusive access
    du = (L2 - L*((F - r)/a + (F)/a) + v/a - (V*a*((F - r)/a + (F)/a))/(F*(F - r))) # linear harvest cost + effort cost, slow institution exclusive access
    #Fs = solve(du, F) # try to use nsolve
    Fs = nsolve(du,1) # try to use nsolve
    Fs = np.array(Fs)
    return Fs

def open_access(L2,L,r,a,v,V):
    #fast institution, open access
    de=(L2 - L*((F - r)/a) - (V*a*((F - r)/a ))/(F*(F - r))) # linear harvest cost + effort cost with fast institution, open access
    #Fk = solve(de, F) # try to use nsolve
    Fk = nsolve(de,1) # try to use nsolve
    Fk = np.array(Fk)
    return Fk

for i in np.arange(len(Vrange)):
        V = Vrange[i]
        L = Lrange
        L2 = L2range
        Fs = excl_access(L2,L,r,a,v,V)
        #Fk = open_access(L2,L,r,a,v,V)
        print "Fs", Fs
        print "Fk", Fk
        Fsol_excl[[i],0:3] = Fs
        Fsol_open[[i],0:3] = Fk

Fsol_excl = np.log2(Fsol_excl/(r/2)) # log transform and relative to MSY
Fsol_open = np.log2(Fsol_open/(r/2)) # log transform and relative to MSY

Fa = Fsol_excl[:,0] # equilibrium solution one
Fb = Fsol_excl[:,1] # equilibrium solution two
Fc = Fsol_excl[:,2] # equilibrium solution three

Fd = Fsol_open[:,0] # equilibrium solution one
Fe = Fsol_open[:,1] # equilibrium solution two
Ff = Fsol_open[:,2] # equilibrium solution three

plt.scatter(Vrange,Fa,color='r') # plot matrix Fsol_all
plt.scatter(Vrange,Fb,color='b') # plot matrix Fsol_all
plt.scatter(Vrange,Fc,color='y') # plot matrix Fsol_all
plt.xlabel('reference price',fontsize=15)
plt.ylabel('harvest rate',fontsize=15)
plt.ylim(0,1)
plt.xlim(0,10)
# plt.savefig('./Documents/SESYNC/GIT/fishmar/RQ3/figures/equilirbium_solutions.png',dpi=500)
plt.show()
