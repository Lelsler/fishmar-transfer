# Build database of relationships between FAO landing / FAO trade / COMTRADE
# By Alfredo Giron - Scripps Institution of Oceanography
# 17/April/2018
# --
rm(list = ls())
# Read data
data <- read.csv('data/matching/fao_landings_codes.csv')

# Go through each record of id_comtrade and separate in different records. Generate final
# dataset as :
# R = data.frame(id_fao,id_comtrade)

R = data.frame() # Data frame to store final dataset
n <- NROW(data) # Number of records in the data
for (i in 1:n){
  # If there is a semicolon at the end of the string, delete it to homogenize 
  #idc <- as.character(data$id_comtrade[i]) # Id_comtrade or Id_fao_trade
  idc <- as.character(data$id_fao_trade[i]) # Id_comtrade or Id_fao_trade
  nidc <- nchar(as.character(idc)) # Length of string
  
  # Find semicolons in text
  pos = gregexpr(';',idc) # Transform to numer instead of list
  pos = as.numeric(pos[[1]])
  npos = length(pos) # Number of semicolons
  posF = pos[npos]
  
  if (!is.na(posF)){ # In case that there is not a semicolon, don't do anything
    if (posF == nidc){ # In case there is a semicolon, earse it when it is the last character
      idc = substr(idc,1,nidc-1)
    }
  }
  
  # Now that last semicolon was removed, identify remaining ones
  nidc <- nchar(as.character(idc)) # Length of string
  
  # Find semicolons in text
  pos = gregexpr(';',idc) # Transform to numer instead of list
  pos = as.numeric(pos[[1]])
  if (!is.na(pos[1])){
    if (pos[1] < 0){pos = NA} # Transform to NA when it gives -1 (non existing)
    npos = length(pos) # Number of semicolons
    posF = pos[npos]
  }
    
  # Run this section if there is still at least one semicolon
  if (!is.na(pos[1])){
    # Go through each semicolon and get the characters in between as numbers
    # Identify the first element and extract
    id_output = rep(0,npos)
    id_output[1] = as.numeric(substr(idc,1,pos[1]-1))
    
    for (j in 1:npos){
      pi = pos[j] + 1  # Initial position
      pf = pos[j+1] - 1 # Final position
      id_output[j+1] = as.numeric(substr(idc,pi,pf))
    }
    
    # Get last element
    id_output[npos + 1] = as.numeric(substr(idc,pos[npos]+1,nidc))
  } else { # Else, when the position of semicolons was NA, just get the first element as a number
    id_output = as.numeric(idc)
  }
  
  # Use id_output to build the new matrix
  # id_fao | id_comtrade
  id_fao = data$id_fao[i]
  n_out = length(id_output)
  id_fao = rep(id_fao,n_out)
  
  output = data.frame(id_fao,id_output)
  R = rbind(R,output)
  
}

