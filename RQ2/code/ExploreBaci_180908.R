### THIS WORKS ON THE ONLINE VERSION OF SESYNC RSTUDIO 
# set working directory to public data
setwd("/nfs/public-data/Baci")
# then in more > go to working directory

# list all files in WD
list.files()

# look at product codes
product= read.csv("product_code_baci07.csv")
