
extractGOF <- function(mods){
  adeq.stats <- NULL
  for(i in 1:length(mods)){
    model <- mods[[i]]
    
    ## mean absolute error
    mae <- hydroGOF::mae(model$summary.fitted.values$mean, data$cases, na.rm=T)
    
    # R-SQUARE
    dev <- model$dic$deviance.mean
    n <- nrow(model$summary.fitted.values)
    
    # In reference to null model
    nulldev <- null.mod$dic$deviance.mean
    x1 <- round(1 - exp((-2/n)*((dev/-2) - (nulldev/-2))), 3)
    
    # In reference to random effects model
    redev <- re.mod$dic$deviance.mean
    x2 <- round(1 - exp((-2/n)*((dev/-2) - (redev/-2))), 3)
    
    r2.null <- x1
    r.null <- x2
    
    add <- data.frame(mod=i+1,
                      modelNames[i],
                      waic=model$waic$waic, 
                      dic = model$dic$dic,
                      cpo=-mean(log(model$cpo$cpo)),
                      rsq.null=x1,
                      rsq.re=x2,
                      mean.mae=mae)
    
    adeq.stats <- bind_rows(adeq.stats, add)
  }
  return(adeq.stats)
}
