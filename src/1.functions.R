library(data.table)
library(ggplot2)
library(fastDummies)
library(dplyr)
library(moments)
library(stats)
library(tseries)
library(lmtest)
library(car)

getLogLinModel <- function(dt,DependantVar,PredictorVar,PredictorVarName){
  explication <- paste(unlist(PredictorVarName[PredictorVar]), collapse = " + ")
  formula <- as.formula(sprintf("log(%s) ~ %s",DependantVar,explication))
  model <- lm(formula, dt)
  model
}

#attention fonction faite à la va vite, il faut que les poids de chaque entrées soit stocké dans une colone qui se nomme "poids"
getLogLinModelPond <- function(dt,DependantVar,PredictorVar,PredictorVarName){
  explication <- paste(unlist(PredictorVarName[PredictorVar]), collapse = " + ")
  formula <- as.formula(sprintf("log(%s) ~ %s",DependantVar,explication))
  model <- lm(formula=formula,data= dt,weights=dt$poids)
  model
}

getLinLinModel <- function(dt,DependantVar,PredictorVar,PredictorVarName){
  explication <- paste(unlist(PredictorVarName[PredictorVar]), collapse = " + ")
  formula <- as.formula(sprintf(" %s  ~ %s",DependantVar,explication))
  model <- lm(formula, dt)
  model
}

getDATAPresent <- function(dt,RefTable,id){
  dt_res <- dt
  for (covar in names(RefTable)[-1]){
    if (RefTable[ID==id,get(covar)]){
      dt_res <- subset(dt_res, !is.na(dt_res[,get(covar)]))
    }
  }
  dt_res
}

getFittedValue <- function(dt,linmod){
  dt_work <- dt[,.SD,.SDcols=names(linmod$coefficients[-1])]
  dt_work <- cbind(1,dt_work)
  dt_work <- as.matrix(dt_work)
  coefs <- as.matrix(linmod$coefficients)
  res <- c(dt_work%*%coefs)
  res
}

#attention, ne fonctionne que pour les régression lin. non pondéré
getRsqr <- function(reelval,fittedval){
  r2 <- cor(reelval,fittedval)^2
  r2
}

#attention, ne fonctionne que pour les régression lin. non pondéré
getAdjRsqr <- function(reelval,fittedval,linmod){
  r2 <- getRsqr(reelval,fittedval)
  p <- length(linmod$coefficients)-1
  n <- length(fittedval)
  adj.r2 <- 1-(1-r2)*(n-1)/(n-p-1)
  adj.r2
}