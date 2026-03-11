library(stockassessment)
load("run/model.RData")

### LEAVEOUT --------------------
#LO<-leaveout(fit)

library(parallel)
myleaveout <- function (fit, fleet = as.list(2:fit$data$noFleets), ncores = detectCores(), ...) {
  myrunwithout <- function(fit, year=NULL, fleet=NULL, ...){
    data <- reduce(fit$data, year=year, fleet=fleet, conf=fit$conf)      
    data$sumKey <- data$sumKey[-fleet,] # added
    conf <- attr(data, "conf")
    pd <- defpar(data,conf)
    par <- pd # added
    par$rec_pars[] <- fit$pl$rec_pars # added
    par$missing<-NULL
    attr(par, "what") <- NULL
    ret <- sam.fit(data, conf, par, ...) # deleted map
    return(ret)
  }
  if (ncores > 1) {
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl))
    clusterExport(cl, varlist = "fit", envir = environment())
    lib.ver <- dirname(path.package("stockassessment"))
    clusterExport(cl, varlist = "lib.ver", envir = environment())
    clusterEvalQ(cl, {
      library(stockassessment, lib.loc = lib.ver)
    })
    runs <- parLapply(cl, fleet, function(f) myrunwithout(fit, fleet = f, ...))
  }
  else {
    runs <- lapply(fleet, function(f) myrunwithout(fit, fleet = f, ...))
  }
  converg <- unlist(lapply(runs, function(x) x$opt$conv))
  if (any(converg != 0)) 
    warning(paste0("leavout run(s) ", paste0(which(converg != 0), collapse = ","), " did not converge."))
  names(runs) <- paste0("w.o. ", lapply(fleet, function(x) paste(attr(fit$data, "fleetNames")[x], collapse = " and ")))
  attr(runs, "fit") <- fit
  class(runs) <- "samset"
  runs
}
LO <- myleaveout(fit, map=fit$obj$env$map)

save(LO, file="run/leaveout.RData")


### RETROS --------------------
library(stockassessment)
load("run/model.RData")
# Retros fixing SRR parameters:
map <- fit$obj$env$map
map$rec_pars <- factor(c(NA,NA))

RETRO<-retro(fit, year=5, map=map, ncores=1)
save(RETRO, file="run/retro.RData")

### RESIDUALS --------------------
library(stockassessment)
load("run/model.RData")
RES<-residuals(fit)
RESP<-procres(fit)
save(RES, RESP, file="run/residuals.RData")
