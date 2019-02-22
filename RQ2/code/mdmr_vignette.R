
# Packages
library(MDMR)

# Load example data
# Response (Y.mdmr): 10 outcome variables (columns) and 500 subjects (rows)
# Predictors (X.mdmr): 3 predictor variables (columns) and 500 subjects (rows)
# Predictors were standardized to mean zero and unit variance
# Note: we can standardize our matrix using scale(mat, center=T, scale=T)
data(mdmrdata)

# Compute dissimilarity matrix
D <- dist(Y.mdmr, method = "manhattan")

# There are two ways to regress outcomes (D) against predictors (M)


# Option 1
################################################################################

# Fit model
mdmr.res <- mdmr(X = X.mdmr, D = D)

# Interpreting model results
# There will always be p+1 rows where p=number of predictors
# STATISTIC = test statistic for the omnibus effect of all predictors on the 
# distance matrix (first row) and the conditional effects of each predictor (remaining rows).
# NUMER.DF = number of degrees of freedom 
# PSEUDO.R2 = the omnibus and conditional pseudo-R2 statistics, which are defined
# as the proportion of total sums of squares of D attributable to each corresponding effect. 
# ANALYTIC.P.VALUE = analytical p-value corresponding to that predictor.
summary(mdmr.res)


# Option 2 - more efficient when n is large (number of subjects)
################################################################################

# Calculate Gower-transformed distance matrix and eigenvalues of G
G <- gower(D)
lambda <- eigen(G, only.values = T)$values

# Fit model using G matrix and associated eigenvalues
mdmr.res2 <- mdmr(X = X.mdmr, G = G, lambda = lambda)

# View fit
summary(mdmr.res2)


# This is computationally time-saving when you want to evaluate a different
# set of predictors for the same response (i.e., doing model selection?)

# To demonstrate, we simulate new predictors and refit the model using the 
# G matrix and eigenvalues already calculated
set.seed(102)
x1 <- rnorm(500)
mdmr.tmp <- mdmr(X = x1, D = D, return.lambda = T)
lambda <- mdmr.tmp$lambda
mdmr.res3 <- mdmr(X = X.mdmr, G = G, lambda = lambda)
summary(mdmr.res3)


# Calculate and visualize effect size
################################################################################

# Option 1
# Calcuate and plot effect size of each predictor V (from X) on each outcome from Y via D
par(mar = c(5, 5, 4, 2) + 0.1)
es <- delta(X = X.mdmr, Y = Y.mdmr, 
            dtype = "manhattan", plot.res = T,
            niter = 1, seed = 12345)

# Deepnes of green and size of value indicates strength of effect






