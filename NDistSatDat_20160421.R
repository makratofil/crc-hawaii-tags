#Script NDistSatDat.r...Megan C. Ferguson...April 21, 2016

#################################################################################
#     USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE!  USER BEWARE!      #
#I have done all that I can think of to check for errors in this code, but that #
#does not mean that it is completely bug-free.  It would be a good idea to check#
#some of the output records with calculations done by hand each time you modify #
#the input dataset.  If the input dataset is continually being added to, it is  #
#a good idea to check to make sure that subsequent runs of the program provide  #
#identical results for the old records.                                         #
#################################################################################

#OVERVIEW
#This script inputs satellite position data as a .csv file in the following 
#format:
#  ANIMAL,LINE,PTT,DATE_,time,LATITUDE,LONGITUD,LC94
#  Md4,Md420080710:174700XDP,83742,7/10/2008,17:47:00,19.580000,-156.019000,DP
#If the "LINE" variable has more than 18 characters after the animal ID, the 
#code will not work.  Specifically, it is important for the "LINE" variable to
#have the following format:
#  <ID, any number of characters><YYYY><MM><DD>:<HH><MM><SS><CCC>
#The output is a .csv file that identifies all individuals (and the quality code 
#for the satellite data associated with each) that surfaced within the same 
#satellite overpass, and it provides the great circle distance (km) 
#between all pairs of individuals in the surfacing.  If data exists for only a 
#single individual in a given pass, it is not represented in the output file 
#(although this can easily be changed).  

#HOW IT WORKS
#The script applies the following general steps to accomplish these tasks:
#  1. The time (in minutes) from 00:00.00 hrs on January 1, 2006 is computed for
#     each record.
#  2. The records are sorted according to tyme.
#  3. Time elapsed (delta.t) between successive records is computed, based upon 
#     tyme.
#  4. Records with delta.t <= mn.in.pass (minutes in pass, which the user can
#     set below) are assigned the same pass number.  Pass numbers are incremented
#     with increasing tyme.
#  5. Great circle distances (km) are computed between all pairs of individuals 
#     assigned to each pass.  
#     a. If a single individual has more than one record in a given pass, the
#        record with the best location data is used to compute distances to other
#        animals in the pass.  The quality of the location data, from best to
#        worst, is as follows: 3 > 2 > 1 > 0 > A > B > Z  
#          i.  In the case of an individual having more than one record in a 
#              given pass and the quality of the location data are equivalent
#              among records for that individual, then the last record that is closest
#              in time to the neighboring animal's record is used to compute
#              distance.  For passes in which this occurs and more than two animals
#              are present, the "best" record to use for the individual with 
#              tied location quality data will be determined independently by
#              comparison with each neighbor's time stamp.  Hypothetical Example:
#                    HYPOTHETICAL EXAMPLE:
#                  id    lc94   tyme               delta.t                pass
#                  Md99  L3     1939888.71666667   5.05236083333333e+05   <###>
#                  Md6   L2     1939888.73333333   1.66666668374091e-02   <###>
#                  Md4   L0     1939889.60000000   8.66666666697711e-01   <###>
#                  Md6   L2     1939898.25000000   8.64999999990687e+00   <###>
#                  Md4   L1     1939898.30000000   5.00000000465661e-02   <###>
#                    In this case, distances would be computed between the 
#                    following:
#                     1. Md6 (tyme=1939898.25000000) and Md4 (lc94=L1)
#                     2. Md6 (tyme=1939888.73333333) and Md99
#                     3. Md99 and Md4 (lc94=L1)
#               
#  6. Results are output into file specified by user below as variable outfile
#        DEFINITIONS of columns in output file:
#          1. rec.1 : LINE for one individual in neighbor pair
#          2. lc.1 : lc94 for individual 1 
#          3. rec.2 : LINE for second individual in neighbor pair
#          4. lc.2 : lc94 for individual 2
#          5. gc.dist.km : Great cirlce distance (km) between individual 
#                          associated with rec.1 and its neighbor in rec.2.  
#          6. pass : Satellite overpass to which the record belongs.  This
#                    really refers to a particular 15-min window of time
#                    because more than one satellite could pass over the 
#                    whales in a given "pass."  Passes in which only a 
#                    single individual is present are not output (although
#                    this can easily be changed).
  
#########################################################################
#                      USER TASKS ARE IN THIS BOX                       #
#########################################################################
  #1. Identify input filename                                           
  #  infile <- "Mddata.csv"                                           
  #  infile <- "2013GmMItagassoc.csv"
     infile <- "TtTags_2020FEBKauai_forNDistSatDat.csv"
    
  #2. Define total number of days (365 or 366) in each year of data.  The
  #   following assumes that the first year of data is 2006.     
  #   dy.in.yr <- c(365,  #2006
    #                365,  #2007
     #               366,  #2008 was a leap year                         '
      #              365,  #2009
       #             365,  #2010
        #            365,  #2011
         #           366,  #2012
          #          365)  #2013
     dy.in.yr <- 366 # 2020 was a leap year
                    
  #3. Designate number of minutes that encompass a single satellite 
  #   overpass in order to group individuals that surface in the
  #   same overpass.
      mn.in.pass <- 15  #This assumes a 15-minute time window                  
      
  #4. MAKE SURE THAT THE sp PACKAGE HAS BEEN DOWNLOADED TO THE VERSION OF R 
  #   THAT WILL BE RUNNING THIS SCRIPT.  THE sp PACKAGE WILL BE ACCESSED VIA 
  #   THE library() COMMAND BELOW.      
  
  #5. Designate order of location quality codes, from poorest quality to 
  #   best quality.  
  #   NOTE: Records with lc94 == DP are deleted automatically by the 
  #         program because they refer to deployment locations.
      loc.qual.cod <- c("LZ","LB","LA","L0","L1","L2","L3") 
      
  #6. Designate output filename
      outfile <- "TtTags_2020FEBKauai_NDistSatDat_out.csv"    
###########################################################################
#                          END USER TASKS                                 #
###########################################################################      

  library(sp)

  options(digits=15)      #Show more significant digits when values are printed to 
                          #console

  #Input data
    satdat.all <- read.csv(file=infile, 
                       col.names=c("id", 
                                   "rec", 
                                   "ptt", 
                                   "mdy", 
                                   "tm", 
                                   "lat", 
                                   "long", 
                                   "lc94"),
                       colClasses=c("factor",
                                    "character",
                                    "numeric",
                                    "character",
                                    "character",
                                    "numeric",
                                    "numeric",
                                    "factor"))
    #Ck
      summary(satdat.all)
      summary(satdat.all$lc94)
      
  #Omit records with lc94=="DP"
    not.DP <- which(satdat.all$lc94 != "DP")
    satdat <- satdat.all[not.DP,]
    satdat$lc94 <- ordered(satdat$lc94, levels=loc.qual.cod)
    #Ck
      summary(satdat)
      summary(satdat$lc94)
  
  #Set up accounting for time
    #Define number of days per month
      dy.in.mo <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      dy.in.mo.lp <- c(31,29,31,30,31,30,31,31,30,31,30,31)
    #Define number of minutes per day
      mn.dy <- 60*24                
  
  #Separate year, month, day, hour, minute, second in data
    yr.strt <- nchar(satdat$rec) - 17 
    yr <- as.numeric(substr(satdat$rec, start=yr.strt, stop=(yr.strt+3)))
    mo <- as.numeric(substr(satdat$rec, start=(yr.strt+4), stop=(yr.strt+5)))
    dy <- as.numeric(substr(satdat$rec, start=(yr.strt+6), stop=(yr.strt+7)))
    hr <- as.numeric(substr(satdat$rec, start=(yr.strt+9), stop=(yr.strt+10)))
    mn <- as.numeric(substr(satdat$rec, start=(yr.strt+11), stop=(yr.strt+12)))
    sc <- as.numeric(substr(satdat$rec, start=(yr.strt+13), stop=(yr.strt+14)))
    satdat <- cbind.data.frame(satdat, yr, mo, dy, hr, mn, sc)
    #Ck
      summary(satdat)
  
  #Compute minutes since January 1, 2006 (or whatever year)
  #  NOTE: The start date/time needs to be set to January 1 of the earliest year
  #        of data.
    tyme <- sapply(1:nrow(satdat), function(i){
              #Number of days that have passed in complete years since 2006
                if(satdat$yr[i] == 2020){
                  days <- 0
                } else {
                  days <- sum(dy.in.yr[1:(satdat$yr[i]-2020)])
                }
              #Number of days that have passed since Jan 1st of current year
                if(satdat$mo[i] == 1){
                  #For January
                  d <- satdat$dy[i] - 1
                } else {
                  #For all months except January
                  if(dy.in.yr[(satdat$yr[i]-2019)] == 365){
                    #Not a leap year
                    d <- sum(dy.in.mo[1:(satdat$mo[i]-1)]) + satdat$dy[i] - 1
                  } else {
                    d <- sum(dy.in.mo.lp[1:(satdat$mo[i]-1)]) + satdat$dy[i] - 1
                  }
                }
              t.elapse <- (days + d)*mn.dy + satdat$hr[i]*60 + satdat$mn[i]
              if(satdat$sc[i] > 0){
                t.elapse <- t.elapse + (satdat$sc[i]/60)
              }
              return(t.elapse)
            })    
    satdat <- cbind.data.frame(satdat, tyme)
    #Ck
      summary(satdat)  
      
  #Reorder records based on tyme 
    tyme.ordr.idx <- rank(satdat$tyme, ties.method="random") 
    satdat.ord <- satdat
    satdat.ord[tyme.ordr.idx,] <- satdat  

  #Compute time elapsed between successive records  
    prev.tyme <- c(satdat.ord$tyme[1],satdat.ord$tyme[1:(nrow(satdat)-1)])
    delta.t <- satdat.ord$tyme - prev.tyme
    satdat.ord <- cbind.data.frame(satdat.ord, delta.t)
    #Ck
      summary(satdat.ord$delta.t)
      
  #Group records by satellite pass.  Assume all records within mn.in.pass
  #minutes of each other are within the same overpass.    
    pass <- rep(1, length=nrow(satdat.ord))
    for (i in 2:nrow(satdat.ord)) {
      if(satdat.ord$delta.t[i] > 15){
        #Record i is not in same satellite overpass as record (i - 1)
        pass[i:nrow(satdat.ord)] <- pass[i] + 1
      }
    }    
    satdat.ord <- cbind.data.frame(satdat.ord, pass)
    
  #Compute great circle distances (km) between all individuals in the same pass
    NDist <- lapply(1:max(pass), function(i){
    #NDist <- lapply(68, function(i){
      
               #Find indices of all records in pass i
                 idx <- which(pass == i)
               #If more than one record in pass i, compute all pairwise distances
                 if(length(idx) > 1){
                   #Extract records in pass i
                     pass.i <- satdat.ord[idx,]
                   #Determine if there are duplicate records for any individuals
                     id.dup <- names(which(summary(pass.i$id) > 1))
                   #Determine whether any duplicate records can be omitted 
                   #due to poor quality location data.  Create variable
                   #rec.to.keep that contains records that should be used
                   #to compute neighbor distances.  
                     if(length(id.dup) > 0){  
                       rec.to.keep <- unlist(sapply(1:length(id.dup), 
                                                    function(j, PASS.i=pass.i){
                         pass.j <- PASS.i[which(PASS.i$id == id.dup[j]),]
                         lq.j <- pass.j$lc94
                         keep.rec.j <- pass.j$rec[which(rank(pass.j$lc94) == 
                                                        max(rank(pass.j$lc94)))]
                         return(keep.rec.j)
                       }))
                       #Extract idx of individuals in pass i represented by only 
                       #one record
                         id.i.solo <- names(which(summary(pass.i$id) == 1))
                       #Extract indices of records in pass i for individuals with
                       #duplicate records   
                         idx.i.to.keep <- unlist(sapply(1:length(rec.to.keep), function(j){
                           return(match(rec.to.keep[j], pass.i$rec))
                         }))                           
                       #Create vector of ALL records in pass.i to keep
                         if(length(id.i.solo) > 0){       
                           idx.i.solo <- sapply(1:length(id.i.solo), function(j){
                             return(which(pass.i$id == id.i.solo[j]))
                           })
                           idx.i.to.keep <- c(idx.i.to.keep, idx.i.solo)
                         }
                       #Create new pass.i dataframe
                         pass.ii <- pass.i[idx.i.to.keep,]
                     } else {
                       pass.ii <- pass.i
                     }        
                   #Compute great circle distances (km) and keep ONE set of
                   #distances for each pair of records
                     gc.dist.mat <- sapply(1:nrow(pass.ii), 
                                           function(j, PASS.ii=pass.ii){
                       pt.j <- c(PASS.ii$long[j], PASS.ii$lat[j])
                       pts.j <- cbind(PASS.ii$long, PASS.ii$lat)  
                       gc.dist.j <- spDistsN1(pts.j, pt.j, longlat=TRUE)
                       return(gc.dist.j)
                     })
                     if(is.null(nrow(gc.dist.mat)) == FALSE){
                       idx.out <- matrix(rep(FALSE,length(gc.dist.mat)),
                                         nrow=nrow(gc.dist.mat))
                       idx.out <- lower.tri(idx.out)
                       gc.dist.mat[idx.out == FALSE] <- NA
                     }
                     rex.out <- pass.ii$rec
                     lc.out <- as.vector(pass.ii$lc94)
                   #Return distances for appropriate neighbor pairs
                     #Test for duplicates again, now that filtering by location 
                     #quality has occurred 
                       id.dup.lq <- names(which(summary(pass.ii$id) > 1))
                       if(length(id.dup.lq) > 0){
                         #Figure out which neighbors to use for pairs based
                         #on minimal differences in tyme, and extract the
                         #associated entries of gc.dist.mat.  First, compute
                         #all pairwise differences in tyme.
                           d.tyme <- sapply(1:nrow(pass.ii), 
                                            function(j, PASS.ii=pass.ii){
                                       tyme.j <- rep(PASS.ii$tyme[j], nrow(PASS.ii))
                                       delta <- abs(tyme.j - PASS.ii$tyme)  
                                       return(delta)
                                     }) 
                         #Determine which elements of gc.dist.mat to omit based
                         #on duplicate id criteria.  In the following, index
                         #j is assiciated with index of the duplicate id and k is
                         #associated with all other ids.            
                           jk.omit <- lapply(1:length(id.dup.lq), 
                                             function(j,PASS.ii=pass.ii,d.t=d.tyme){
                             dup.idx <- which(PASS.ii$id == id.dup.lq[j])
                             #Extract delta(tyme) associated with duplicate id=j
                               dup.d.t <- d.t[dup.idx,]
                             #Determine which record for id=j is farthest in time
                             #to every other record in the pass  
                               jk.max <- sapply(1:ncol(dup.d.t), 
                                                function(k,ddt=dup.d.t){
                                 max.tyme.idx <- which(dup.d.t[,k] == max(dup.d.t[,k])) 
                                 return(max.tyme.idx[1]) #Need to designate a scalar value in case of ties
                               })     
                               k.idx <- 1:ncol(dup.d.t)
                               j.idx <- dup.idx[jk.max] 
                               jk.mat <- cbind(j.idx,k.idx)
                               JK <- t(sapply(1:nrow(jk.mat), function(r, m=jk.mat){
                                        return(sort(jk.mat[r,], decreasing=T))
                                      })) 
                               return(JK)            
                           })
                         #Identify the other elements of gc.dist.mat that 
                         #shouldn't be output but that didn't have to do with the 
                         #duplicate id issue.
                           #idx.out <- matrix(rep(FALSE,length(gc.dist.mat)),   #
                           #                  nrow=nrow(gc.dist.mat))           #
                           #idx.out <- lower.tri(idx.out)                       #
                           for(m in 1:length(id.dup.lq)){
                             idx.out[jk.omit[[m]]] <- FALSE
                           }
                           gc.dist.mat[idx.out == FALSE] <- NA       
                       }
                 } else {
                   gc.dist.mat <- NA
                   rex.out <- c(satdat.ord$rec[idx],NA)
                   lc.out <- c(satdat.ord$lc94[idx],NA)
                 }
               #Return matrix of neighbor distances and records of individuals
               #used to compute each distance to be output                 
                 out.list <- list("gc.dists"=gc.dist.mat,"rex"=rex.out,"lc"=lc.out) 
                 return(out.list)  
             })
  
  #Output results 
  #  Step through elements of gc.dists in NDist.  For each entry that is not NA,
  #  output the gc.dist value and the records associated with the row and column
  #  indices of that entry.
    NDist.out <- cbind.data.frame("rec.1"="DUMMY",
                                  "lc.1"="DUMMY",  
                                  "rec.2"="DUMMY",
                                  "lc.2"="DUMMY", 
                                  "gc.dist.km"=-9.9,
                                  "pass"=-99)
    for(i in 1:max(pass)){
      if(is.null(nrow(NDist[[i]]$gc.dists)) == FALSE){                              
        for(j in 1:nrow(NDist[[i]]$gc.dists)){
          for(k in 1:ncol(NDist[[i]]$gc.dists)){
            gc.d <- NDist[[i]]$gc.dists[j,k]
            if(is.na(gc.d) == FALSE){
              outpt <- cbind.data.frame("rec.1"=NDist[[i]]$rex[j], 
                                        "lc.1"=NDist[[i]]$lc[j],
                                        "rec.2"=NDist[[i]]$rex[k],
                                        "lc.2"=NDist[[i]]$lc[k], 
                                        "gc.dist.km"=gc.d,
                                        "pass"=i)
              NDist.out <- rbind.data.frame(NDist.out,outpt)
            }
          }
        } 
      }       
    }
    NDist.out <- NDist.out[2:nrow(NDist.out),]
    
    write.csv(NDist.out, file=outfile, quote=FALSE, row.names=FALSE)
  
   