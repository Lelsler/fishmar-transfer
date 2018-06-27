#___________________________________________________________________________________________________________#
# Comtrade data cleaning
#___________________________________________________________________________________________________________#

#setwd("C:\\Users\\jgephart\\Dropbox\\SESYNC\\Mentoring\\Grad Pursuit")
setwd("C:\\Users\\Jessica\\Dropbox\\SESYNC\\Mentoring\\Grad Pursuit")

#___________________________________________________________________________________________________________#
# Load packages and data
#___________________________________________________________________________________________________________#
library(dplyr)
library(stringr)
library(countrycode)

trade_data <- read.csv("Data\\CT_SoleCodHaddock_annual.csv")

# Drop 'world' values
trade_data <- filter(trade_data, !Partner.Code==0)

#___________________________________________________________________________________________________________#
# Find max of reported imports/exports on each edge
#___________________________________________________________________________________________________________#
Imp <- filter(trade_data, Trade.Flow=="Import")
Imp <- dplyr::select(Imp, -(Trade.Flow))
names(Imp)[names(Imp) == 'Reporter.Code'] <- 'Importer.Code'
names(Imp)[names(Imp) == 'Reporter'] <- 'Importer'
names(Imp)[names(Imp) == 'Reporter.ISO'] <- 'Importer.ISO'
names(Imp)[names(Imp) == 'Partner.Code'] <- 'Exporter.Code'
names(Imp)[names(Imp) == 'Partner'] <- 'Exporter'
names(Imp)[names(Imp) == 'Partner.ISO'] <- 'Exporter.ISO'
names(Imp)[names(Imp) == 'Qty'] <- 'Import.Qty'
names(Imp)[names(Imp) == 'Netweight..kg.'] <- 'Import.Netweight.kg'
names(Imp)[names(Imp) == 'Trade.Value..US..'] <- 'Import.Value.US'


Exp <- filter(trade_data, Trade.Flow=="Export")
Exp <- dplyr::select(Exp, -(Trade.Flow))
names(Exp)[names(Exp) == 'Reporter.Code'] <- 'Exporter.Code'
names(Exp)[names(Exp) == 'Reporter'] <- 'Exporter'
names(Exp)[names(Exp) == 'Reporter.ISO'] <- 'Exporter.ISO'
names(Exp)[names(Exp) == 'Partner.Code'] <- 'Importer.Code'
names(Exp)[names(Exp) == 'Partner'] <- 'Importer'
names(Exp)[names(Exp) == 'Partner.ISO'] <- 'Importer.ISO'
names(Exp)[names(Exp) == 'Qty'] <- 'Export.Qty'
names(Exp)[names(Exp) == 'Netweight..kg.'] <- 'Export.Netweight.kg'
names(Exp)[names(Exp) == 'Trade.Value..US..'] <- 'Export.Value.US'

trade_data_max <- full_join(Imp, Exp)
trade_data_max <- mutate(trade_data_max, 
                         Max.Quantity = pmax(Import.Qty, Export.Qty, na.rm = TRUE),
                         Max.Weight = pmax(Import.Netweight.kg, Export.Netweight.kg, na.rm = TRUE), 
                         Max.Value = pmax(Import.Value.US, Export.Value.US, na.rm = TRUE))

#___________________________________________________________________________________________________________#
# Output data file for use in figures
#___________________________________________________________________________________________________________#
# Save output
write.csv(trade_data_max, "Data\\CTclean_SoleCodHaddock.csv", row.names = FALSE)
