## FormatKalman_forMovebank.R: Format batched Kalman filtered/smoothed data
## reprocessed thru Argos CLS for importing into Movebank to run the Douglas
## Filter

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 26 August 2021

## ========================================================================== ##

# load libraries
library(lubridate)
library(dplyr)

# specify species 
spp = "Gm"
tag_range = "Tag004-231"
smooth_file = paste0(spp, tag_range, "_smoothing/")
deploy_file = paste0("Summary files/Location info for R/", spp, tag_range, "_DeployInfoForR.csv")

# import all douglas filtered files for species
files <- list.files(path = paste0("Raw Argos files/", spp),
                    pattern = ".smoothing.csv",
                    full.names = T, recursive = T)

files
# sub files if need
files <- files[c(16:22)]
files

# import deployment information for tags 
deploy <- read.csv(deploy_file, header = T)
deploy$date <- as.POSIXct(deploy$date, tz = "UTC")
#deploy$LC <- "DP"
deploy <- filter(deploy, animal %in% c("GmTag026",
                                       "GmTag027",
                                       "GmTag028",
                                       "GmTag029",
                                       "GmTag030",
                                       "GmTag031",
                                       "GmTag032"))


## Function to format for Movebank ##
format <- function(x, u) {
  # read in the tag file
  d <- read.csv(x, header = T, sep = ";", stringsAsFactors = F)
  
  # rename/add columns
  colnames(d)[colnames(d) == "Platform.ID.No."] <- "ptt"
  d$ptt <- as.character(d$ptt)
  colnames(d)[colnames(d) == "Loc..date.yyyy.MM.dd.HH.mm.ss"] <- "date"
  colnames(d)[colnames(d) == "Loc..quality"] <- "LC"
  
  # make date into POSIXct class, no need to worry about TZ at this step.
  d$date <- ymd_hms(d$date, tz = "UTC")
  d$IQ <- rep(11)# dummy variable for Douglas Filter; this would be 66 for fastloc gps positions
  
  # add deploy ID from deploy info file
  u = deploy
  y = filter(u, ptt %in% d$ptt)
  d$animal = y$animal
  
  return(d)
}

# apply the function
dfs <- lapply(files, format)
all <- bind_rows(dfs)

## Look at tag locations individually and check for clearly erraneous locations that may
## need to be removed prior to import into Movebank ##
unique(all$animal)
t <- all[all$animal == "GmTag026",]
summary(t$Latitude)
summary(t$Longitude)
summary(all)
#t_sub <- t[c(1:148),] # remove locations prior to deployment if necessary

# remove locations or datasets if needed

# write csv
write.csv(all, paste0("For Douglas Filter/", spp, "/", "GmTag066-069_Lanai", "_Kalman_smooth.csv"),
          row.names = F, na = "")

## Function to summarize counts of LC classes (checks for uploading errors in Movebank) ##
summ <- function(x) {
  
  N = x %>%
    group_by(animal) %>%
    summarise(N = n())
  
  EndDate = x %>%
    group_by(animal) %>%
    summarise(EndDate = dplyr::last(date))
    
  EndLat = x %>%
    group_by(animal) %>%
    summarise(EndLat = dplyr::last(Latitude))
  
  EndLon = x %>%
    group_by(animal) %>%
    summarise(EndLon = ((360 - dplyr::last(Longitude))*-1))
  
  LC3 = x %>%
    group_by(animal) %>%
    filter(LC == "3") %>%
    summarise(LC3 = n())
  
  LC2 = x %>%
    group_by(animal) %>%
    filter(LC == "2") %>%
    summarise(LC2 = n())
  
  LC1 = x %>%
    group_by(animal) %>%
    filter(LC == "1") %>%
    summarise(LC1 = n())
  
  LC0 = x %>%
    group_by(animal) %>%
    filter(LC == "0") %>%
    summarise(LC0 = n())
  
  LCA = x %>%
    group_by(animal) %>%
    filter(LC == "A") %>%
    summarise(LCA = n())
  
  LCB = x %>%
    group_by(animal) %>%
    filter(LC == "B") %>%
    summarise(LCB = n())
  
  LCZ = x %>%
    group_by(animal) %>%
    filter(LC == "Z") %>%
    summarise(LCZ = n())
  
  
  f_sum <- left_join(N, EndDate, by = "animal") %>%
    left_join(EndLat, by = "animal") %>%
    left_join(EndLon, by = "animal") %>%
    left_join(LC3, by = "animal") %>%
    left_join(LC2, by = "animal") %>%
    left_join(LC1, by = "animal") %>%
    left_join(LC0, by = "animal") %>%
    left_join(LCA, by = "animal") %>%
    left_join(LCB, by = "animal") %>%
    left_join(LCZ, by = "animal") 
}

# apply function 
Sum <- summ(all)

# save summary file 
write.csv(Sum, "Summary files/Location info for R/GmTag066-069_KalmanSmoothed_RawLCSum.csv", row.names = F, na = "")


