from scipy.optimize import *
import numpy as np

C=.2 #portion of cost
xi=1
R=1.4116
b=1
V=1
r=2
a=1
p=1
v=0.2
Es=1
L2=0 #effort-based cost

Lrange = np.arange(0,2.5,2.5/50)
L2range=np.array([0,.25,.5])

def equations(c1,c,r,a):
    dS = S*(r-a*S-F)
    du = c1-c *(F-r)/a + (F/a) + V/a -(V*a*(F-r/a))/(F*(F-r))s
    return F

for i in np.arange(Lrange):
    L = Lrange(i)
    du(F)=(L2 - L*((F - r)/a + (Es*F)/a) + v*Es/a - (V*a*((F - r)/a + (Es*F)/a))/(F*(F - r)))
    Fsol=vpasolve(du);


F = fsolve(du, F)
print equations(F)
