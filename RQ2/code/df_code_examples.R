### how to concatenate two columns in a new column ###
dat <- read.table(textConnection(
  "C1  C2  C3  C4  C5
  A   B   F   C   Q
  G   H   I   J   T
  K   D   R   S   E
  P   L   M   N   O"
), header = TRUE)
closeAllConnections()

dat$NewCol <- do.call(paste, c(dat[c("C3", "C4")], sep = ""))
dat 

### merging dataframes ###
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)))
df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("Alabama", 2), rep("Ohio", 1)))

k1 = merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
k2 = merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
k3 = merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE) # merges only what exists in y/df2
k4 = merge(x = df1, y = df2, by = NULL) # joins both dataframes along both customerIDs

##! merge only certain columns in a dataframe EXAMPLE

DF1 <- read.table(textConnection(
  "Client    Q   Sales  Date
  A    2   30     01/01/2014
  A    3   24     02/01/2014
  A    1   10     03/01/2014
  B    4   10     01/01/2014
  B    1   20     02/01/2014
  B    3   30     03/01/2014"
), header = TRUE)
closeAllConnections()

DF2 <- read.table(textConnection(
  "Client  LO  CON
   A    12  CA
  B    11  US
  C    12  UK
  D    10  CA
  E    15  AUS
  F    91  DD"
), header = TRUE)
closeAllConnections()

merge(x = DF1, y = DF2[ , c("Client", "LO")], by = "Client", all.x=TRUE)

### merge data frames based on more than one column
x <- data.frame(k1=c(NA,NA,3,4,5), k2=c(1,NA,NA,4,5), data=1:5)
y <- data.frame(k1=c(NA,2,NA,4,5), k2=c(NA,NA,3,4,5), data=1:5)
merge(x, y, by=c("k1","k2")) # NA's match
z = left_join(x, y, by=c("k1","k2")) 

### add a column and merge into the df
#mean for 2001
mean_gov_eff_2001 = data.frame(gov_eff$effectiveness[gov_eff$year==2000]+gov_eff$effectiveness[gov_eff$year==2002]/2, 2001, gov_eff$iso3)
colnames(mean_gov_eff_2001) = c("effectiveness", "year", "iso3")

gov_eff = rbind(mean_gov_eff_2001, gov_eff)

