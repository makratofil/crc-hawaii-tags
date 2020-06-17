## Script for fitting (1) Bayes SSM for location estimation only (DCRW.ssm) and (2) Bayes SSM for location and behavioural state estimation (DCRWS.ssm)
## 25.10.2011
rm(list=ls())
require(bsam)
library(plyr)

fn = "PcAllGPSArgosGIS20190711"
steps = c(4, 6, 8, 12, 24)

df = data.frame()
posfile = read.csv(file.path("GISoutput", paste(fn, ".csv", sep="")))
#posfile$date = as.POSIXct(paste(as.character(posfile$date), as.character(posfile$time)), format = "%m/%d/%Y %H:%M:%S", tz="GMT")
posfile$date = as.POSIXct(posfile$datetimeUTC, tz="GMT")
posfile = with(posfile, data.frame(id=animal, date, lc=lc94, lon=longitud, lat=latitude))
posfile$lc = revalue(posfile$lc, c(GPS = 3, DP = "3", L3 = "3", L2 = "2", L1 = "1", L0 = "0", LA = "A", LB = "B", LZ = "Z"))
#tags = unique(posfile$id)
#tags = c("SaTag006")
#tags = "GmTag168"
tags = c("PcTagP06", "PcTag028", "PcTag008")


for(tag in tags) {
	cat("Processing ", tag, "\n")
	argos = subset(posfile, id == tag)
	for(step in steps) {
	  fname = file.path("summary", paste(tag, "SSM", as.character(step), ".csv", sep=""))
	  if (!file.exists(fname)) {
	    ## Estimate track using MCMC via JAGs - this could tak 20-30 minutes
	    cat("Processing ", tag, " with a time step of ", step, " hours.\n")
	    DCRWS.ssm = fit_ssm(data=argos, model="DCRWS", tstep=step/24, adapt=30000, samples=10000, thin=10)
	    
	    ## Save summary results to .csv file 
	    fname = file.path("summary", paste(tag, "SSM", as.character(step), ".csv", sep=""))
	    cat("Writing ", fname, ".\n")
	    write.csv(DCRWS.ssm[[1]]$summary, fname, row.names=FALSE)
	  }
	}
} 


