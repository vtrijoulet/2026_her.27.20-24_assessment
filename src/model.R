library(stockassessment)
setwd("../")
load("run/data.RData")
load(file = "data/fit15_bench.RData")
fitBench <- stockassessment:::refit(fit); rm(fit)
save(fitBench, file="run/fitBench.RData")
conf<-fitBench$conf
par<-defpar(dat,conf)

par$rec_pars <- c(17.78250, 12.65671)
map <- list()
map$rec_pars <- factor(c(NA,NA))

fit<-sam.fit(dat,conf,par, map=map)
if(fit$opt$convergence!=0) stop("Model did not converge.")
save(fit, file="run/model.RData") 


