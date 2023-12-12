####### Program for getting most recent data from HMD
############# Written by JMA, thanks to Tim Riffe
############# 18/08/2020
library(HMDHFDplus)
library(data.table)


# Log in info for HMD -----------------------------------------------------

# get all countries in HMD
XYZ <- getHMDcountries()
# set your username for HMD
us <- ""
# set your password
pw <- ""



# Get data from HMD -------------------------------------------------------
# get all the lifetables available from HMD
HMDLT <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

HMD.DxEx <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Deaths         <- readHMDweb(x,"Deaths_1x1",username=us,password=pw)
  Exposures      <- readHMDweb(x,"Exposures_1x1",username=us,password=pw)
  Deaths$Type    <- "Deaths"
  Exposures$Type  <- "Exposures"
  CTRY         <- rbind(Deaths, Exposures)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

# convert to data.table
HMDLT    <- data.table(HMDLT)
HMD.DxEx <- data.table(HMD.DxEx)


# Change data structure ---------------------------------------------------
Deaths <- melt.data.table(data = HMD.DxEx[Type %in% 'Deaths'],
                id.vars = c('PopName','Year','Age'),
                measure.vars = c('Female','Male','Total'),
                variable.name = 'Sex',value.name = 'Deaths')

Exposures <- melt.data.table(data = HMD.DxEx[Type %in% 'Exposures'],
                          id.vars = c('PopName','Year','Age'),
                          measure.vars = c('Female','Male','Total'),
                          variable.name = 'Sex',value.name = 'Exposures')

HMD.Deaths <- merge.data.table(Deaths,Exposures,all.x = T)



# Save data ---------------------------------------------------------------
save(HMD.Deaths,HMDLT,file="Data/HMD_Data.RData")


