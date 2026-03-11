library(stockassessment)
source("src/common.R")
# load what has been saved
setwd("run")
for(f in dir(pattern="RData"))load(f) 
setwd("..")

# basefit<-NULL
# if(file.exists("baserun/model.RData")){
#   local({load("baserun/model.RData"); basefit<<-fit})
# }else{
#   basefit <- fit
# }
# local({load("../../user3/WBSS_HAWG_2025_sf/run/model.RData"); basefit<<-fit})
# fits <- c(Original=basefit,current=fit)
basefit<-fitBench
fits <- c(Bench=fitBench,current=fit)

# exfitname <- scan("conf/viewextra.cfg", what="", comment.char="#", quiet=TRUE)
# for(nam in exfitname){
#   local({
#     fit<-urlLoadFit(paste0("https://www.stockassessment.org/datadisk/stockassessment/userdirs/user3/",nam,"/run/model.RData"))
#     if(!is.null(fit)){
#       i <- length(fits)
#       fits[[i+1]] <<- fit
#       names(fits)[i+1] <<- nam
#     }else{
#       warning(paste0("View extra stock ", nam, " not found or of incompatible format (skipped)"))
#     }
#   })
# }

plotcounter<-1
tit.list<-list()

setcap<-function(title="", caption=""){   
 tit.list[length(tit.list)+1]<<-paste("# Title",plotcounter,"\n")
 tit.list[length(tit.list)+1]<<-paste(title,"\n")
 tit.list[length(tit.list)+1]<<-paste("# Caption",plotcounter,"\n")
 tit.list[length(tit.list)+1]<<-paste(caption,"\n")
 plotcounter<<-plotcounter+1 
}


############################## plots ##############################
plots<-function(){
  par(cex.lab=1, cex.axis=1, mar=c(5,5,1,1))
    
  if(exists("fits")){
    ssbplot(fits, addCI=TRUE, ylim=c(0,6E+5))
    stampit(fit)
    setcap("Spawning stock biomass", "Spawning stock biomass. 
            Estimates from the current run and point wise 95% confidence 
            intervals are shown by line and shaded area.")
    
    fbarplot(fits, addCI=TRUE)
    stampit(fit)
    setcap("Average fishing mortality", "Average fishing mortality for the shown age range. 
            Estimates from the current run and point wise 95% confidence 
            intervals are shown by line and shaded area.")

    recplot(fits, addCI=TRUE, las=0, ylim=c(0,1.2E+8))
    stampit(fit)
    setcap("Recruitment", "Yearly recruitment. 
        Estimates from the current run and point wise 95% confidence 
        intervals are shown by line and shaded area.")

    catchplot(fits, addCI=TRUE)
    stampit(fit)
    setcap("Catch", "Total catch in weight. 
        Prediction from the current run and point wise 95% confidence 
        intervals are shown by line and shaded area. The yearly
        observed total catch weight (crosses) are calculated as Cy=sum(WayCay).")

    srplot(fit)
    stampit(fit)
    setcap("Spawner-recruits", "Estimated recruitment as a function of spawning stock biomass.")

    plot(ypr(fit))
    stampit(fit)
    setcap("Yield per Recruit", "Yield per recruit (solid line) and spawning stock biomass plotted against different levels of fishing")
   
    if(!all(fit$conf$obsCorStruct=="ID")){ 
      corplot(fit)			  
      setcap("Estimated correlations", "Estimates correlations between age groups for each fleet")
      stampit(fit)
    }
     
    for(f in 1:fit$data$noFleets){
      fitplot(fit, fleets=f)
      setcap("Fit to data", "Predicted line and observed points (log scale)")
      stampit(fit)
    }
    
    sdplot(fit)
    stampit(fit)
    setcap("Standard deviation", "Standard deviation on the observations estimated in the model")
    
    col <- rep(1:6,fit$data$noYears)
    matplot(y=t(faytable(fit)), x=as.numeric(rownames(t(faytable(fit)))), type="l", xlab="Age", ylab="Fishing mortality at age")
    for (i in 1:fit$data$noYears) text(y=t(faytable(fit)[i,]), x=as.numeric(rownames(t(faytable(fit)))), labels = fit$data$years[i], cex = 0.7, col=col[i])
    stampit(fit)
    setcap("Fishing mortality at age", "Estimated fishing mortality at age for each model year")
    
    
    qtableplot(qtable(fit))
    #stampit(fit)
    setcap("Catchability", "Estimated catchability for the surveys with confidence intervals")
    
    qplot <-function(fit, ...){
      Q <- fit$conf$keyLogFpar
      Q[] <- exp(fit$pl$logFpar)[ifelse(Q<0,NA,Q+1)]
      ages <- fit$conf$minAge:fit$conf$maxAge
      Q <- Q/rowMeans(Q, na.rm=T)
      idx <- apply(Q,1,function(x)!all(is.na(x)))
      Q <- Q[idx,]
      matplot(ages, t(Q), type="b", lwd=4, ylab="Standardized catchability", pch=1, col=1:10, lty=1:10, ...)
      fn<-attr(fit$data, "fleetNames")[idx]
      legend("topright", legend=fn, lty=1:10, lwd=4, col=1:10, bty="n")
    }
    
    qplot(fit)
    stampit(fit)
    setcap("Standardized catchability", "Comparison of estimated catchability for the surveys with confidence intervals")
    

    #Q<-fit$pl$logFpar
    #Qsd<-fit$plsd$logFpar
    #key<-fit$conf$keyLogFpar
    #fun<-function(x)if(x<0){NA}else{Q[x+1]}
    #FF<-Vectorize(fun)
    #ages<-fit$conf$minAge:fit$conf$maxAge
    #matplot(ages, exp(t(matrix(FF(key), nrow=5))), type="l", lwd=5, lty="solid", xlab="Ages", ylab="Q")
    #legend("topright", lwd=5, col=2:5, legend=attr(fit$data, "fleetNames")[2:5])
    #stampit(fit)

  }  
  
  if(exists("RES")){  
    plot(RES)
    setcap("One-observation-ahead residuals", "Standardized one-observation-ahead residuals.")
    stampit(fit)
    par(mfrow=c(1,1))
    
    ##empirobscorrplot(RES)	
    ##setcap("Residual correlations", "Empirical between-age correlations of OOA-residuals .")
    ##stampit(fit)
  }
  
   
  if(exists("RESP")){  
    plot(RESP)
    setcap("Process residuals", "Standardized single-joint-sample residuals of process increments")
    stampit(fit)
    par(mfrow=c(1,1))
  } 
 
  
  if(exists("LO")){  
    ssbplot(LO)
    setcap("Leaveout (SSB)", "")
    stampit(fit)
    
    fbarplot(LO)
    setcap("Leaveout (Average F)", "")
    stampit(fit)

    recplot(LO)
    setcap("Leaveout (Recruitment)", "")
    stampit(fit)

    catchplot(LO)
    setcap("Leaveout (Catch)", "")
    stampit(fit)
    
  } 
  
  
  if(exists("RETRO")){
    rho<-mohn(RETRO)
    ssbplot(RETRO, las=0, drop=0)
    legend("topright", legend=substitute(rho[Mohn]==RR~'%', list(RR=round(100*rho[2]))), bty="n")
    setcap("Retrospective (SSB)", "")
    stampit(fit)
    
    fbarplot(RETRO, las=0, drop=0)
    legend("topright", legend=substitute(rho[Mohn]==RR~'%', list(RR=round(100*rho[3]))), bty="n")
    setcap("Retrospective (Average F)", "")
    stampit(fit)
    
    recplot(RETRO, las=0, drop=0)
    legend("topright", legend=substitute(rho[Mohn]==RR~'%', list(RR=round(100*rho[1]))), bty="n")
    setcap("Retrospective (Recruitment)", "")
    stampit(fit)

    catchplot(RETRO)
    setcap("Retrospective (Catch)", "")
    stampit(fit)
  } 
  if(exists("FC")){  
    lapply(FC, function(f){plot(f); title(attr(f,"label"), outer=TRUE, line=-1); stampit(fit)})
  }  
  
}


setwd('res')
file.remove(dir(pattern='png$'))
stamp<-gsub('-[[:digit:]]{4}$','',gsub(':','.',gsub(' ','-',gsub('^[[:alpha:]]{3} ','',date()))))
png(filename = paste(stamp,"_%03d.png", sep=''), width = 480, height = 480,
    units = "px", pointsize = 10, bg = "white")
  plots()    
dev.off()

writeLines(unlist(tit.list),'titles.cfg') 

png(filename = paste("big_",stamp,"_%03d.png", sep=''), width = 1200, height = 1200, 
    units = "px", pointsize = 20, bg = "white")
  plots()    
dev.off()

#pdf(onefile=FALSE, width = 8, height = 8)
#  plots()    
#dev.off()

file.remove(dir(pattern='html$'))

tsb<-tsbtable(fit)
colnames(tsb)<-c("TSB","Low", "High")
tab.summary <- cbind(summary(fit), tsb)
xtab(tab.summary, caption=paste('Table 1. Estimated recruitment, spawning stock biomass (SSB), 
     and average fishing mortality','.',sep=''), cornername='Year', 
     file=paste(stamp,'_tab1.html',sep=''), dec=c(0,0,0,0,0,0,3,3,3,0,0,0))

ftab <- faytable(fit)
xtab(ftab, caption=paste('Table 2. Estimated fishing mortality at age','.',sep=''), cornername='Year \ Age', 
     file=paste(stamp,'_tab2.html',sep=''), dec=rep(3,ncol(ftab)))

ntab <- ntable(fit)
xtab(ntab, caption=paste('Table 3. Estimated stock numbers at age','.',sep=''), cornername='Year \ Age', 
     file=paste(stamp,'_tab3.html',sep=''), dec=rep(0,ncol(ntab)))

ptab <- partable(fit)
xtab(ptab, caption=paste('Table 4. Table of model parameters','.',sep=''), cornername='Parameter name', 
     file=paste(stamp,'_tab4.html',sep=''), dec=rep(3,ncol(ptab)))

mtab <- modeltable(c(Current=fit, base=basefit))
mdec <- c(2,0,2,6)[1:ncol(mtab)]
xtab(mtab, caption=paste('Table 5. Model fitting','.',sep=''), cornername='Model', 
     file=paste(stamp,'_tab5.html',sep=''), dec=mdec)
     
sdState<-function(fit, y=max(fit$data$years)-1:0){
  idx <- names(fit$sdrep$value) == "logR"
  sdLogR<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logssb"
  sdLogSSB<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logfbar"
  sdLogF<-fit$sdrep$sd[idx][fit$data$years%in%y]
  ret<-cbind(sdLogR, sdLogSSB, sdLogF)
  rownames(ret)<-y
  colnames(ret)<-c("sd(log(R))", "sd(log(SSB))", "sd(log(Fbar))")
  return(ret)
}

sdtab <- sdState(fit)
xtab(sdtab, caption=paste('Table 6. Table of selected sd','.',sep=''), cornername='Year', 
     file=paste(stamp,'_tab6.html',sep=''), dec=rep(3,ncol(sdtab)))


if(exists("FC")){  
    ii<-0
    lapply(FC, function(f){
       ii<<-ii+1;
       tf<-attr(f,"tab");
       dec<-c(3,3,3,rep(0,ncol(tf)-3));
       xtab(tf, caption=paste0('Forecast table ',ii,'. ', attr(f,"label"),'.'), 
       cornername='Year', file=paste(stamp,'_tabX',ii,'.html',sep=''), dec=dec);      
       })
}  

setwd("..") 

