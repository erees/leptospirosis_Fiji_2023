library(tidyverse)
library(INLA)
library(hydroGOF)
library(cowplot)
library(data.table)

## Defining final models chosen from model fitting process
data <- readRDS("data/casesClimDatMonth.rds") # Standardised data
# data <- readRDS("data/casesClimDatMonthElisaRdt.rds") # Standardised data

# dataNS <- readRDS("data/casesClimDatMonthNS.rds") # non-standardised data
data$div <- as.numeric(data$ID.div)
# dataNS$div <- as.numeric(dataNS$ID.div)

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

## Random effects
month.re <- " + f(ID.month, model='rw1', cyclic=TRUE)"
year.re <- " + f(ID.year, model='iid', replicate = div)"

res <- paste0(month.re, year.re)
res_div <- " + f(ID.month, model='rw1', cyclic=TRUE) + f(ID.year, model='iid')"



## RANDOM EFFECTS ONLY MODEL ####
re.mod <-  run.mod(paste0("cases~1", res),data)
saveRDS(re.mod,"monthlyModel/savedModels/re.mod.RDS")

## NULL MODEL ####
null.mod <- run.mod(paste0("cases~1"),data)
saveRDS(null.mod,"monthlyModel/savedModels/null.mod.RDS")

tmin.mod <- run.mod(paste0("cases~1", res,"+tmin.1"),data)
saveRDS(tmin.mod,"monthlyModel/savedModels/tmin.mod.RDS")

prcp.mod <- run.mod(paste0("cases~1", res,"+ prcpCum2"),data)
saveRDS(prcp.mod,"monthlyModel/savedModels/prcp.mod.RDS")

sst.mod <- run.mod(paste0("cases~1", res,"+ sst34.2"),data)
saveRDS(sst.mod,"monthlyModel/savedModels/sst.mod.RDS")

prcp.sst.mod <- run.mod(paste0("cases~1", res,"+ sst34.2 + prcpCum2"),data)
saveRDS(prcp.sst.mod,"monthlyModel/savedModels/prcp.sst.mod.RDS")

prcp.tmin.mod <- run.mod(paste0("cases~1", res,"+ tmin.1 + prcpCum2"),data)
saveRDS(prcp.tmin.mod,"monthlyModel/savedModels/prcp.tmin.mod.RDS")

full.mod <- run.mod(paste0("cases~1", res,"+ tmin.1 +sst34.2 + prcpCum2"),data)
saveRDS(full.mod,"monthlyModel/savedModels/full.mod.RDS")



## By division

null.mod.c <-  run.mod(paste0("cases~1"),dataCentral)
saveRDS(null.mod.c,"monthlyModel/savedModels/null.mod.c.RDS")

null.mod.w <-  run.mod(paste0("cases~1"),dataWestern)
saveRDS(null.mod.w,"monthlyModel/savedModels/null.mod.w.RDS")

null.mod.n <-  run.mod(paste0("cases~1"),dataNorthern)
saveRDS(null.mod.n,"monthlyModel/savedModels/null.mod.n.RDS")

re.mod.c <-  run.mod(paste0("cases~1", res_div),dataCentral)
saveRDS(re.mod.c,"monthlyModel/savedModels/re.mod.c.RDS")

re.mod.w <-  run.mod(paste0("cases~1", res_div),dataWestern)
saveRDS(re.mod.w,"monthlyModel/savedModels/re.mod.w.RDS")

re.mod.n <-  run.mod(paste0("cases~1", res_div),dataNorthern)
saveRDS(re.mod.n,"monthlyModel/savedModels/re.mod.n.RDS")

full.mod.c <- run.mod(paste0("cases~1", res_div,"+ tmin.1 +sst34.2 + prcpCum2"),dataCentral)
saveRDS(full.mod.c,"monthlyModel/savedModels/full.mod.c.RDS")

full.mod.w <- run.mod(paste0("cases~1", res_div,"+ tmin.1 +sst34.2 + prcpCum2"),dataWestern)
saveRDS(full.mod.w,"monthlyModel/savedModels/full.mod.w.RDS")

full.mod.n <- run.mod(paste0("cases~1", res_div,"+ tmin.1 +sst34.2 + prcpCum2"),dataNorthern)
saveRDS(full.mod.n,"monthlyModel/savedModels/full.mod.n.RDS")

