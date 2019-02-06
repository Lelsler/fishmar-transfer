install.packages("EpiModel", depencencies = TRUE)

require(EpiModel)

nw = network.initialize(n=1000, directed = FALSE)
nw = set.vertex.attribute(nw, "risk", rep(0:1, each=500))
plot(nw)

myrisk = get.vertex.attribute(nw, "risk")

plot(nw, vertex.col= myrisk+4)

#predictors of formation, so in the spread of trade model this could be nodefactor governance for instance. The problem I can see with our networks is that in the disease
#networks all nodes will get sick but in our case we might have to make it into something bi-partite where the importers are spreading the disease.
#OR we model export coutries as nodes and the importing countries that they share as the links, simulating the importers as the "disease". 
#OR we add exports/or the fish stock as a predictor which will become obviously a very important one. 

# Nodematch is selective mixing by attibute, allows the probablity of an edge to depend on the attribute of both nodes.
#assortative/disassortative mixing between groups
formation = ~edges + nodefactor("risk") + nodematch("risk") + concurrent

#data are passed to the package estimation function in summary form, as target statistics for each 
#predictor

# so the edges term here signifies that there are 250 edges in the network. formation depends on the factor risk and nodematch risk. but don't really know
# what these numbers mean here + what is concurrent??
target.stats = c(250, 375, 225, 100)

coef.diss = dissolution_coefs(dissolution = ~offset(edges), duration = 50)

coef.diss

est1 = netest(nw, formation, target.stats, coef.diss)

dx = netdx(est1, nsims=10, nsteps=1000)

par(mfrow = c(1,2))

plot(dx, type ="duration")
abline(v=200, col=2)
plot(dx, type="dissolution")


init = init.net(i.num = 50)


#the generic SIS model requires three parameters.
####the infection probablity (inf.prob) is the rik of transmission given
#an act between a susceptible and an infected person
# the act rate (act.rate) is the mean number of sexual acts that
#occur within each active partnership during each time step
#the recovery rate (rec.rate) is the probability that an infected person recovers at each times tep

#what if importers are the disease connecting exporting countries stocks. this directly speaks to the idea of sequential 
#exploitation where a stock in one location is connected to a stock in another location. Thing is of course, how does this work
#with the timestep? it's not ideal, ideal would be a bi-partite network of importers and exporters. 



#in an SIS model nodes become susceptible again after recover
#these are set using the function param.net :
param = param.net(inf.prob = 0.1, act.rate=5, rec.rate=0.02)

control = control.net(type = "SIS", nsteps = 500, nsims=10, epi.by = "risk")

sim1 = netsim(est1, param, init, control)

summary(sim1, at=500)

mySimData = as.data.frame(sim1)

plot(sim1)

plot(sim1, y = c("si.flow", "is.flow"), leg =T)

plot(sim1, y= c("i.num.risk0", "i.num.risk1"), leg =TRUE)



