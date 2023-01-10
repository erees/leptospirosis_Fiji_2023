############################################################################
## Outputs for weekly model by division and by country
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

source("functions/outputFunctions.R")
## Read in case data
data <- readRDS("data/casesClimDatElisa.rds") # standardised data

## Split data by Central, Western and Northern division
dataCentral <- data %>% filter(division == "central")
dataWestern <- data %>% filter(division == "western")
dataNorthern <- data %>% filter(division == "northern")


############################################################################
#### Read in saved models ####
############################################################################

## 1. Read in models for the weekly case data with RE by year replicated by division
##    and weekly random effects

modelVars <- c("null.mod","re.mod","tmin.mod","prcp.mod","sst.mod","prcp.sst.mod","prcp.tmin.mod","full.mod")
path <- "weeklyModel/savedModels/"

for(i in 1:length(modelVars)){
  modName <- modelVars[[i]]
  pathName <- paste(path, modName,".RDS",sep = "")
  mod <- readRDS(pathName)
  assign(modelVars[[i]],mod)
}


## 2. Read in models for the weekly case data separately by each division 

modelVars <- c("re.mod.c","re.mod.w","re.mod.n","null.mod.c","null.mod.w","null.mod.n","full.mod.c","full.mod.w","full.mod.n")

for(i in 1:length(modelVars)){
  modName <- modelVars[[i]]
  pathName <- paste(path, modName,".RDS",sep = "")
  mod <- readRDS(pathName)
  assign(modelVars[[i]],mod)
}


############################################################################
#### Create table of model outputs ####
############################################################################

modelNames <- c("RE",
                "RE + tmin.1",
                "RE + sst34.4",
                "RE + prcpCum6.1",
                "RE + prcpCum6.1 + sst34.4",
                "RE + prcpCum6.1 + sst34.4 + tmin.1")

mods <- list(re.mod,tmin.mod,sst.mod,prcp.mod,prcp.sst.mod,full.mod)

gofTable <- extractGOF(mods)


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
write_csv(gofTable, "weeklyModel/savedModelOutputs/gofTable_weekly.csv")


############################################################################
#### Extracting values for individual models for each division division ####
############################################################################

## Extracting R2 (null and baseline model)
extractR2(full.mod,re.mod,null.mod) * 100
extractR2(full.mod.c,re.mod.c,null.mod.c) * 100
extractR2(full.mod.w,re.mod.w,null.mod.w)* 100
extractR2(full.mod.n,re.mod.n,null.mod.n)* 100

## Extracting Cross-Validated log score
(-mean(log(re.mod$cpo$cpo))/-mean(log(full.mod$cpo$cpo)) *100)-100
(-mean(log(re.mod.c$cpo$cpo))/-mean(log(full.mod.c$cpo$cpo)) *100)-100
(-mean(log(re.mod.w$cpo$cpo))/-mean(log(full.mod.w$cpo$cpo)) *100)-100
(-mean(log(re.mod.n$cpo$cpo))/-mean(log(full.mod.n$cpo$cpo)) *100)-100


############################################################################
#### Plotting model outputs ####
############################################################################


############################################################################
#### Time-series plot ####
#### Figure 3  ####
############################################################################

div_names <- c(`central` = "Central Division",
               `western` = "Western Division",
               `northern` = "Northern Division"
)


## Plot time series with random effect 
plotTimeSeries <- function(model1, model2,data,ttl) {
  model1$summary.fitted.values %>%
    bind_cols(cases=data$cases,
              date=data$date,
              division=data$division,
              mean2 = model2$summary.fitted.values$mean,
              lci = model2$summary.fitted.values$`0.025quant`,
              uci = model2$summary.fitted.values$`0.975quant`) %>% 
    ggplot(aes(x=date)) +
    geom_line(aes(y=cases, col="Cases")) +
    geom_line(aes(y=mean2, col="Baseline model"),linetype = 1) +
    geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.4, fill = "#ce9642") +
    geom_line(aes(y=mean, col="Final model")) +
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), alpha=0.5, fill = "#3b7c70") +
    facet_wrap(~division,ncol=1, labeller = as_labeller(div_names),scales = "free") +
    scale_color_manual(name="", 
                       values = c("Cases"="grey60", "Final model"="#3b7c70", "Baseline model" = "#ce9642")) +
    labs(title = "", x = "Time",y="Leptospirosis cases") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    #ylim(0,60) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}


tsPlot <- plotTimeSeries(full.mod,re.mod, data,"RE + tmin.1 + prcpCum6.1 + sst34.4")
tsPlot ## Figure 3


############################################################################
#### Plotting effect sizes
#### Figure 2
############################################################################

effectEst <- full.mod$summary.fixed
effectEst <- as_tibble(effectEst) %>% round(digits = 2)
write_csv(effectEst, "weeklyModel/savedModelOutputs/effectEstWeekly.csv")

fullEffectEst <- full.mod$summary.fixed %>%
  rbind(full.mod.c$summary.fixed) %>%
  rbind(full.mod.w$summary.fixed) %>%
  rbind(full.mod.n$summary.fixed) %>%
  mutate(model = rep(c("All divisions","Central division","Western division","Northern division"),each=4)) %>%
  mutate(var = rep(c("int","tmin.1","sst34.4","prcpCum6.1"),4)) %>%
  filter(var !="int")

effectPlot <- fullEffectEst %>%
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
  # scale_color_manual(values = c("grey30","#AF6B46","#A5A58D","#6B705C")) +
  
  # scale_color_manual(values = c("grey30","indianred3", "darkcyan","darkgoldenrod")) +
  scale_x_discrete(labels=c( "Total precipitation \n (six weeks)","Niño 3.4","Minimum \n Temperature")) 

effectPlot ## Figure 2


############################################################################
#### Random effect plots 
## Supplementary Figures 3 and 4 ###
############################################################################

## Supplementary Figure 3 ###
### Weekly Random effect
label0 <- bquote(paste("RE only model"))
label1 <- bquote(paste("Full model"))


re.week <- re.mod$summary.random$ID.week %>%
  rbind(full.mod$summary.random$ID.week) %>%
  mutate(model = as.factor(rep(c("RE","Full"),each = 52))) %>%
  mutate(ID = as.numeric(ID))

weeklyRE <- ggplot(re.week, aes(ID, mean, colour = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(ymin = `0.025quant`, ymax = `0.975quant`), position = position_dodge(width = 0.6)) +
  geom_point(aes(), shape = 21, position = position_dodge(width = 0.6)) +
  theme_classic() +
  ylab("Posterior random effect") + xlab("Week") +
  theme(axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill = NA, size=0.5),
        strip.background = element_blank(),
        legend.position = c(0.83,0.90),
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0, size = 11),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 7.5)) +
  scale_colour_manual(labels = c(label1,label0), values =met.brewer("Kandinsky",4))

weeklyRE ## Supplementary figure 3 

## Supplementary Figure 4 ###
### Yearly Random effect
yearlyRE <- re.mod$summary.random$ID.year %>%
  rbind(full.mod$summary.random$ID.year) %>%
  mutate(division = rep(rep(c("central","western","northern"),each=12),2)) %>%
  mutate(model = as.factor(rep(c("RE","full"),each = 36))) %>%
  mutate(yearID = rep(2006:2017,6))


yearlyREPlot <- ggplot(yearlyRE, aes(as.factor(yearID), mean, colour = division, linetype = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbar(aes(ymin = `0.025quant`, ymax = `0.975quant`), position = position_dodge(width = 0.7)) +
  geom_point(aes(), shape = 21, position = position_dodge(width = 0.7)) +
  theme_classic() +
  ylab("Posterior random effect") + xlab("Year") +
  theme(axis.line = element_blank(),
        panel.background = element_rect(colour = "black", fill = NA, size=0.5),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(hjust = 0, size = 11),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 7.5)) +
  scale_colour_manual(values =met.brewer("Kandinsky",4))

yearlyREPlot ## Supplementary figure 4


############################################################################
#### Plotting difference between full and RE model ####
## Supplementary Figure 2 ###
############################################################################

div_names <- c(`central` = "Central Division",
               `western` = "Western Division",
               `northern` = "Northern Division"
)

observedFittedDat <- full.mod$summary.fitted.values$mean %>%
  bind_cols(cases=data$cases,
            date=data$date,
            division=data$division,
            meanRE = re.mod$summary.fitted.values$mean) %>%
  rename(meanFull = `...1`) %>%
  mutate(diffRE = cases-meanRE,
         diffFull = cases-meanFull,
         improvement = abs(diffRE) - abs(diffFull),
         ID = rep(1:623,3),
         div = rep(c("Central","Western","Northern"),each=623)) %>%
  mutate(dateFactor = as.factor(date))

observedFittedDatGrouped <- observedFittedDat %>%
  group_by(date) %>%
  summarise(meanRE = mean(meanRE),
            meanFull = mean(meanFull),
            diffRE = mean(diffRE),
            diffFull = mean(diffFull),
            improvement = mean(improvement)) %>%
  mutate(ID = 1:623)

improvementDat <- observedFittedDatGrouped %>%
  mutate(fullModel = ifelse(improvement >= 0.001,1,0),
         neutral = ifelse(improvement <=0.001 & improvement >=-0.001,1,0),
         REModel = ifelse(improvement <=-0.001,1,0))

sum(improvementDat$fullModel)
sum(improvementDat$neutral)
sum(improvementDat$REModel)
## Transform into data.table for the plot to work
obGrouped <- as.data.table(observedFittedDatGrouped)

relPlot <- ggplot(data = obGrouped, aes(x = ID, y = improvement, width=1)) +
  geom_col(data = obGrouped[improvement <= 0], fill = "#bb3e03") +
  geom_col(data = obGrouped[improvement >= 0], fill = "#005f73") +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "", x = "Time",y="Relative difference") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # facet_wrap(~division,ncol=1, labeller = as_labeller(div_names)) +
  scale_x_continuous(breaks = c(1, 53,105,157,209,261,313,365,417,469,521,573), labels = c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")) +
  ylim(c(-4.9,4.9)) +
  annotate("text", x = 20, y = 4.5, label = "Full model",colour = "#005f73") +
  annotate("text", x = 20, y = 3.8, label = "RE model",colour = "#bb3e03") 

relPlot ## Supplementary figure 2

