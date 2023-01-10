############################################################################
## Outputs for monthly model by division and by country
## E Rees
############################################################################

## load libraries 
library(tidyverse)
library(INLA)
library(hydroGOF)
library(cowplot)
library(data.table)
library(scales)
library(MetBrewer)

source("functions/outputFunctionsMonthly.R")

data <- readRDS("data/casesClimDatMonthElisaRdt.rds") # standardised data



################################################################################
## Read in models
################################################################################

modelVars <- c("null.mod","re.mod","tmin.mod","prcp.mod","sst.mod","prcp.sst.mod","prcp.tmin.mod","full.mod")
path <- "monthlyModel/savedModels/"

## 1. Read in models for the monthly case data with RE by year replicated by division
##    and monthly random effects
for(i in 1:length(modelVars)){
  modName <- modelVars[[i]]
  pathName <- paste(path, modName,".RDS",sep = "")
  mod <- readRDS(pathName)
  assign(modelVars[[i]],mod)
}


## 2. Read in models for the monthly case data separately by each division 

modelVars <- c("re.mod.c","re.mod.w","re.mod.n","null.mod.c","null.mod.w","null.mod.n","full.mod.c","full.mod.w","full.mod.n")

for(i in 1:length(modelVars)){
  modName <- modelVars[[i]]
  pathName <- paste(path, modName,".RDS",sep = "")
  mod <- readRDS(pathName)
  assign(modelVars[[i]],mod)
}

## 3. Read in full model by country (no divison)
full.model.country <- readRDS("monthlyModel/savedModels/country/full.mod.RDS")


################################################################################
## Create table of model outputs
################################################################################


modelNames <- c("RE",
                "RE + tmin.1",
                "RE + sst34.2",
                "RE + prcpCum2",
                "RE + prcpCum2 + sst34.2",
                "RE + prcpCum2 + sst34.2 + tmin.1")

mods <- list(re.mod,tmin.mod,sst.mod,prcp.mod,prcp.sst.mod,full.mod)

gofTable <- extractGOF(mods)

## Extract various of goodness of fit statistics - used for Supplementary table 3 and 4
gofTable <- gofTable %>%
  mutate(dic=round(dic),
         waic=round(waic),
         rsq.null = round(rsq.null*100,2),
         rsq.re = round(rsq.re*100,2),
         cpo = round(cpo,3),
         mean.mae = round(mean.mae,3)
  )

colnames(gofTable) <- c("Number","Model","WAIC","DIC","CV log score", "R-Squared (Null) %", "R-Squared (RE) %", "MAE")
gofTable
write_csv(gofTable, "monthlyModel/savedModelOutputs/gofTable_monthly.csv") 


################################################################################
### Plotting model outputs
################################################################################

################################################################################
###### Create monthly time-series cases and model
###### Supplementary figure 7
################################################################################

div_names <- c(`central` = "Central Division",
               `western` = "Western Division",
               `northern` = "Northern Division"
)


## Plot time series with full model and random effects only model by division
plotTimeSeries <- function(model1, model2,data,ttl) {
  model1$summary.fitted.values %>% ## Prepare data for plotting
    bind_cols(cases=data$cases,
              date=data$date,
              division=data$division,
              mean2 = model2$summary.fitted.values$mean,
              lci = model2$summary.fitted.values$`0.025quant`,
              uci = model2$summary.fitted.values$`0.975quant`) %>% 
    ggplot(aes(x=date)) +
    geom_line(aes(y=cases, col="Cases")) +
    geom_line(aes(y=mean, col="Full model")) +
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), alpha=0.5, fill = "#7C865B") +
    geom_line(aes(y=mean2, col="RE model"),linetype = "dashed") +
    facet_wrap(~division,ncol=1, labeller = as_labeller(div_names),scales = "free") +
    scale_color_manual(name="", 
                       values = c("Cases"="black", "Full model"="#7C865B", "RE model" = "#AD590B")) +
    labs(title = "", x = "Time",y="Leptospirosis cases") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

tsPlot <- plotTimeSeries(prcp.sst.mod,re.mod, data,"RE + tmin.1 + prcpCum2 + sst34.2")
tsPlot


################################################################################
### Plotting effect sizes
### Supplementary figure 6
################################################################################

## Extract monthly effect estimates for table 
effectEst <- prcp.sst.mod$summary.fixed
effectEst
effectEst <- as_tibble(effectEst) %>% round(digits = 2)
write_csv(effectEst, "monthlyModel/savedModelOutputs/effectEstMonthly.csv")

## Prepare data for plotting
# Bind together the full model (with yearly RE replicated by division) and the full model
# (no replication by division)
effectEstCountry <- full.model.country$summary.fixed %>%
  rbind(full.mod$summary.fixed) %>%
  mutate(model = rep(c("Country","Division"),each=4)) %>%
  mutate(var = rep(c("int","tmin.1","sst34.2","prcpCum2"),2)) %>%
  filter(var !="int") ## remove the intercept

## Effect estimate plot for monthly data
effectPlotCountry <- effectEstCountry %>%
  ggplot(aes(x=var, y=mean, ymin=`0.025quant`, ymax=`0.975quant`,colour = model)) +
  geom_pointrange( position = position_dodge(-0.7)) + 
  labs(x="", y="") +
  geom_hline(yintercept=0,linetype = "dashed", colour = "grey40") +
  theme_classic() +
  theme(axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill = NA, size=0.5),
        strip.background = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0, size = 11),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 7.5)) +
  coord_flip() +
  scale_color_manual(values=met.brewer("Kandinsky",4,direction = -1)) +
  scale_x_discrete(labels=c( "Total precipitation \n (six weeks)","Niño 3.4","Minimum \n Temperature")) 
effectPlotCountry

