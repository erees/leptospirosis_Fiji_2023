############################################################################
## Weekly models whole country together and by division
## E Rees
############################################################################

library(tidyverse)
library(INLA)
library(hydroGOF)
library(cowplot)
library(data.table)

############################################################################
## Read in and process weekly data ##
############################################################################

## Includes case data (number of ELISA positive cases by week and division)
## Population size estimates by division
## Climate info (inclluding prcp, sst and tmin)
data <- readRDS("data/casesClimDatElisa.rds") # standardised data

data$div <- as.numeric(data$ID.div)

## Create seperate datasets by division

dataCentral <- data %>% filter(division == "central")
dataWestern <- data %>% filter(division == "western")
dataNorthern <- data %>% filter(division == "northern")

############################################################################
## Define INLA model function
############################################################################

run.mod <- function(form,data){
  mod <- INLA::inla(as.formula(form), 
                    family="nbinomial", 
                    offset=log(pop/100000),
                    control.inla=list(strategy='adaptive'),
                    control.compute=list(waic=TRUE, dic = TRUE, cpo=TRUE, config=TRUE),
                    control.predictor=list(link=1, compute=TRUE),
                    verbose=FALSE,
                    data=data)
  return(mod)
}

############################################################################
## Define the different random effects
############################################################################


## Random effects where all the data is one model, with yearly RE replicated by division
week.re <- " + f(ID.week, model='rw1', cyclic=TRUE)"
year.re <- " + f(ID.year, model='iid', replicate = div)" ## yearly RE replicated by division
res <- paste0(week.re, year.re)

## Random effects for individual models by division (no replicated yearly RE)
res_div <- " + f(ID.week, model='rw1', cyclic=TRUE) + f(ID.year, model='iid')"

############################################################################
## Define models that appear in the paper
############################################################################

## 1. Model with all data together, and yearly RE replicated by division

## RANDOM EFFECTS ONLY MODEL ##
re.mod <-  run.mod(paste0("cases~1", res),data)
saveRDS(re.mod,"weeklyModel/savedModels/re.mod.RDS")

## NULL MODEL ##
null.mod <- run.mod(paste0("cases~1"),data)
saveRDS(null.mod,"weeklyModel/savedModels/null.mod.RDS")

## RE + CLIMATE VARS ##
tmin.mod <- run.mod(paste0("cases~1", res,"+tmin.1"),data)
saveRDS(tmin.mod,"weeklyModel/savedModels/tmin.mod.RDS")

prcp.mod <- run.mod(paste0("cases~1", res,"+ prcpCum6.1"),data)
saveRDS(prcp.mod,"weeklyModel/savedModels/prcp.mod.RDS")

sst.mod <- run.mod(paste0("cases~1", res,"+ sst34.4"),data)
saveRDS(sst.mod,"weeklyModel/savedModels/sst.mod.RDS")

prcp.sst.mod <- run.mod(paste0("cases~1", res,"+ sst34.4 + prcpCum6.1"),data)
saveRDS(prcp.sst.mod,"weeklyModel/savedModels/prcp.sst.mod.RDS")

prcp.tmin.mod <- run.mod(paste0("cases~1", res,"+ tmin.1 + prcpCum6.1"),data)
saveRDS(prcp.tmin.mod,"weeklyModel/savedModels/prcp.tmin.mod.RDS")


full.mod <- run.mod(paste0("cases~1", res,"+ tmin.1 +sst34.4 + prcpCum6.1"),data)
saveRDS(full.mod,"weeklyModel/savedModels/full.mod.RDS")


## 2. Seperate models by division

## FULL MODEL (RE + climate vars) ##
full.mod.c <- run.mod(paste0("cases~1", res_div,"+ tmin.1 +sst34.4 + prcpCum6.1"),dataCentral)
saveRDS(full.mod.c,"weeklyModel/savedModels/full.mod.c.RDS")

full.mod.w <- run.mod(paste0("cases~1", res_div,"+ tmin.1 +sst34.4 + prcpCum6.1"),dataWestern)
saveRDS(full.mod.w,"weeklyModel/savedModels/full.mod.w.RDS")

full.mod.n <- run.mod(paste0("cases~1", res_div,"+ tmin.1 +sst34.4 + prcpCum6.1"),dataNorthern)
saveRDS(full.mod.n,"weeklyModel/savedModels/full.mod.n.RDS")

## RANDOM EFFECTS ONLY MODEL ##
re.mod.c <- run.mod(paste0("cases~1", res_div),dataCentral)
saveRDS(re.mod.c,"weeklyModel/savedModels/re.mod.c.RDS")

re.mod.w <- run.mod(paste0("cases~1", res_div),dataWestern)
saveRDS(re.mod.w,"weeklyModel/savedModels/re.mod.w.RDS")

re.mod.n <- run.mod(paste0("cases~1", res_div),dataNorthern)
saveRDS(re.mod.n,"weeklyModel/savedModels/re.mod.n.RDS")

## NULL MODEL ##
null.mod.c <- run.mod(paste0("cases~1"),dataCentral)
saveRDS(null.mod.c,"weeklyModel/savedModels/null.mod.c.RDS")

null.mod.w <- run.mod(paste0("cases~1"),dataWestern)
saveRDS(null.mod.w,"weeklyModel/savedModels/null.mod.w.RDS")

null.mod.n <- run.mod(paste0("cases~1"),dataNorthern)
saveRDS(null.mod.n,"weeklyModel/savedModels/null.mod.n.RDS")


full.mod.country <- run.mod(paste0("cases~1", res_div,"+ tmin.1 +sst34.4 + prcpCum6.1"),data)
saveRDS(full.mod.country,"weeklyModel/savedModels/full.mod.country.RDS")
