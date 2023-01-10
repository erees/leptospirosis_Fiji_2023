############################################################################
## Weekly models whole country together and by division
## E Rees
## Creating supplementary Figure 5
############################################################################

library(tidyverse)
library(INLA)
library(hydroGOF)
library(cowplot)
library(data.table)
library(zoo)


## Read in case data (includes non-standardised climate data)
data <- readRDS("data/casesClimDatNonStdElisa.rds") # Non standardised data

data <- data %>%
  select(date,division,cases,pop,ID.year, ID.month, ID.div, ID.week, tmin.1, sst34.4,prcpCum6.1)

data$div <- as.numeric(data$ID.div)

############################################################################
## Define INLA model function
############################################################################

## Re-run the model using the non-standardised climate data
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
week.re <- " + f(ID.week, model='rw1', cyclic=TRUE)"
year.re <- " + f(ID.year, model='iid', replicate = div)"
res <- paste0(week.re, year.re)

## RE model
re.mod <-  run.mod(paste0("cases~1", res),data)

## Full model include climate covariates selected from fitting process
full.mod <- run.mod(paste0("cases~1", res,"+ tmin.1 +sst34.4 + prcpCum6.1"),data)

## Extract the fixed effects
full.fixed <- full.mod$summary.fixed

## Create a df of the extracted fixed effects
data <- data %>%
  mutate(pred = full.fixed[1,4] + full.fixed[2,4]*tmin.1 + full.fixed[3,4]*sst34.4 + full.fixed[4,4]*prcpCum6.1,
         upper = full.fixed[1,5] + full.fixed[2,5]*tmin.1 + full.fixed[3,5]*sst34.4 + full.fixed[4,5]*prcpCum6.1,
         lower = full.fixed[1,3] + full.fixed[2,3]*tmin.1 + full.fixed[3,3]*sst34.4 + full.fixed[4,3]*prcpCum6.1,
         predRain = full.fixed[1,4] +  full.fixed[4,4]*prcpCum6.1,
         predtmin = full.fixed[1,4] + full.fixed[2,4]*tmin.1,
         predsst = full.fixed[1,4] + full.fixed[3,4]*sst34.4)

data <- data %>% 
  mutate(division = rep(c("Central Division","Western Division", "Northern Division"),each = 623))

## Mutate case data so that predicted cases are on the same scale as cases
data <- data %>% 
  mutate(predCases = (exp(pred)/100000)*pop) %>%
  mutate(predCases2 = exp(pred)*pop/100000) %>%
  mutate(incidence = (cases/pop)*100000)

## Plot 1 of predicted case data (climate variables and model parameter estimates, but no random effects)
p1 <- ggplot(data, aes(x=date)) +
  geom_line(aes(y=predCases), col="#bb3e03") +
  facet_wrap(~division, ncol=1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "", x = "Time",y="Predicted cases") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 

## Plot 2 of predicted case data (full model including climate vars and random effects)
p2 <- ggplot(data, aes(x=date)) +
  geom_line(aes(y=cases), col="#005f73") + 
  facet_wrap(~division, ncol=1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "", x = "Time",y="Cases") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 


data <- data %>%
  bind_cols(model = full.mod$summary.fitted.values$mean)

## Plot 3 case data
p3 <- ggplot(data, aes(x=date)) +
  geom_line(aes(y=model),col = "darkGreen") +
  facet_wrap(~division,ncol=1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "", x = "Time",y="Predicted cases") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 


## Create a panal grid plot for supplementary figure 5
plot_grid(p1,p3,p2,ncol=1,labels = "AUTO")
