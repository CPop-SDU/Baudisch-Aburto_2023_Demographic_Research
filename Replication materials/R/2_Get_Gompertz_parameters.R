############# Written by JMA
############# 20/03/2023
#rm(list=ls(all=TRUE))
library(data.table)
library(reshape)
library(reshape2)
library(ggplot2)
library(MortalityLaws)
library(expint)
library(viridis)

# Load data ---------------------------------------------------------------
load("Data/HMD_Data.RData")

HMD.Deaths <- HMD.Deaths[Sex != 'Total']

# Some useful parameters --------------------------------------------------
const     <- list()

const$age.range.1 <- 15:90
const$age.range.2 <- 30:90
const$age.range.3 <- 35:90

const$min.age.1 <- min(const$age.range.1)
const$min.age.2 <- min(const$age.range.2)
const$min.age.3 <- min(const$age.range.3)

const$min.year <- min(HMDLT$Year) # for now all dataset, but we could also set to 1950


# Example of fitting a model to a vector of deaths ------------------------
#availableLaws(law = NULL)
Dx <- HMD.Deaths[Age %in% const$age.range.1 & 
                   PopName %in% 'AUS' & 
                   Year %in% 2000 &
                   Sex == 'Female']$Deaths
Ex <- HMD.Deaths[Age %in% const$age.range.1 & 
                   PopName %in% 'AUS' & 
                   Year %in% 2000 &
                   Sex == 'Female']$Exposures
model1 <- MortalityLaw(x = const$age.range.1,Dx = Dx,Ex = Ex,
             law = 'gompertz',)
model1$coefficients

model1$opt.diagnosis

# Fit a Gompertz to all HMD deaths ----------------------------------------
Gompertz.parameters.1 <- HMD.Deaths[Age %in% const$age.range.1, .(Gompertz.parameters = MortalityLaw(x = Age,
                                                                    Dx = Deaths,
                                                                    Ex = Exposures,
                                                                    law = 'gompertz')$coefficients,
                                                                  Parameters = c('A','B')),
                                 by =.(PopName,Year,Sex)]

Gompertz.parameters.2 <- HMD.Deaths[Age %in% const$age.range.2, .(Gompertz.parameters = MortalityLaw(x = Age,
                                                                                                     Dx = Deaths,
                                                                                                     Ex = Exposures,
                                                                                                     law = 'gompertz')$coefficients,
                                                                  Parameters = c('A','B')),
                                    by =.(PopName,Year,Sex)]

Gompertz.parameters.3 <- HMD.Deaths[Age %in% const$age.range.3, .(Gompertz.parameters = MortalityLaw(x = Age,
                                                                                                     Dx = Deaths,
                                                                                                     Ex = Exposures,
                                                                                                     law = 'gompertz')$coefficients,
                                                                  Parameters = c('A','B')),
                                    by =.(PopName,Year,Sex)]


Gompertz.parameters.1 <-  dcast.data.table(data = Gompertz.parameters.1,
                   formula = PopName + Year + Sex ~ Parameters,
                   value.var = 'Gompertz.parameters')

Gompertz.parameters.2 <-  dcast.data.table(data = Gompertz.parameters.2,
                                           formula = PopName + Year + Sex ~ Parameters,
                                           value.var = 'Gompertz.parameters')

Gompertz.parameters.3 <-  dcast.data.table(data = Gompertz.parameters.3,
                                           formula = PopName + Year + Sex ~ Parameters,
                                           value.var = 'Gompertz.parameters')

save(Gompertz.parameters.1,file = 'Output/GompertzParam_HMD_1.RData')
save(Gompertz.parameters.2,file = 'Output/GompertzParam_HMD_2.RData')
save(Gompertz.parameters.3,file = 'Output/GompertzParam_HMD_3.RData')

?MortalityLaw

availableLF()

MortalityLaw()


