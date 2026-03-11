library(stockassessment)

oldwd<-setwd("data")

cn<-read.ices("cn.dat")
cw<-read.ices("cw.dat")
dw<-read.ices("dw.dat")
lw<-read.ices("lw.dat")
mo<-read.ices("mo.dat")
nm<-read.ices("nm.dat")
pf<-read.ices("pf.dat")
pm<-read.ices("pm.dat")
sw<-read.ices("sw.dat")
lf<-read.ices("lf.dat")
surveys<-read.ices("survey.dat")


## CVs for IBTS/BITS Q1
tmp <- read.table("WBSSher-trawlQ1CV.dat")
tmp <- tmp[,3:5]

ibtsbitsCV <- tmp  / mean(as.matrix(tmp),na.rm=TRUE)
colnames(ibtsbitsCV) <- as.character(3:5)
# attributes(surveys[[4]])$weight = (1/ibtsbitsCV^2)
##


#setwd(oldwd)

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf)

## for laptop:
load(file = "fit15_bench.RData")
tmp <- fit$data$aux
tmp <- cbind(tmp, "weight"=NA)
for (y in rownames(ibtsbitsCV)){
  for (a in colnames(ibtsbitsCV)){
    tmp[which(tmp[, "fleet"]==which(attr(fit$data, "fleetNames")=="IBTS/BITSQ1") & tmp[,"year"]==y & tmp[,"age"]==a),]["weight"] <- (1/ibtsbitsCV[y, which(colnames(ibtsbitsCV)==a),]^2)
  }
}

datCV <- dat
datCV$weight <- tmp[,"weight"]
dat <- datCV
##

save(dat, file="../run/data.RData")
