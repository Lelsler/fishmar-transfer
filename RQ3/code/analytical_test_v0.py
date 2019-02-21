from scipy.optimize import *

def equations(c1,c,r,a):
    dS = S*(r-a*S-F)
    du = c1-c *(F-r)/a + (F/a) + V/a -(V*a*(F-r/a))/(F*(F-r))s
    return F

F = fsolve(du, F)
print equations(F)
