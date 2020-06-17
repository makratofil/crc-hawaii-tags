# Script for formatting Kalman filtered argos data for Movebank (Douglas filter) #


# Michaela A. Kratofil
# 28 APR 2020

# set working directory 
#setwd("E:\\MfGCrw Project - MAK")

# load libraries
library(lubridate)
library(dplyr)

# specify species 
spp = "Gm"
tag_range = "Tag070-231"
deploy_file = paste0(spp, tag_range, "_DeployInfoForR.csv")

# import all douglas filtered files for species
files <- list.files(path = paste0("Raw Argos files/", spp),
                    pattern = "-Locations.csv",
                    full.names = T, recursive = T)

# sub files if need
files <- files[c(9,10,12,14,16,17,19,21,23,25,27,29,31,34,36,37,38,40,41,43,44,46,47)]

# import deployment information for tags 
deploy <- read.csv(paste0("Summary files/", deploy_file), header = T)
deploy$date <- as.POSIXct(deploy$date, tz = "UTC")
#deploy$LC <- "DP"
deploy <- filter(deploy, animal %in% c("GmTag184",
                                       "GmTag185",
                                       "GmTag186",
                                       "GmTag187",
                                       "GmTag188",
                                       "GmTag189",
                                       "GmTag190",
                                       "GmTag191",
                                       "GmTag192",
                                       "GmTag193",
                                       "GmTag194",
                                       "GmTag195",
                                       "GmTag196",
                                       "GmTag219",
                                       "GmTag220",
                                       "GmTag221",
                                       "GmTag222",
                                       "GmTag223",
                                       "GmTag224",
                                       "GmTag225",
                                       "GmTag226",
                                       "GmTag227",
                                       "GmTag228"))

## Function to format for Movebank ##
format <- function(x, u) {
  # read in the tag file
  d <- read.csv(x, header = T, stringsAsFactors = F)
  
  # rename/add columns
  colnames(d)[colnames(d) == "Ptt"] <- "ptt"
  d$ptt <- as.factor(d$ptt)
  colnames(d)[colnames(d) == "Date"] <- "date"
  colnames(d)[colnames(d) == "Quality"] <- "LC"
  
  # make date into POSIXct class, no need to worry about TZ at this step.
  d$date <- as.POSIXct(d$date, format = "%H:%M:%S %d-%b-%Y", tz = "UTC")
  d$IQ <- rep(11)# dummy variable for Douglas Filter; this would be 66 for fastloc gps positions
  d$NMsg <- rep(11)# dummy variable for Douglas Filter
  
  # make deployID as character
  d$DeployID <- as.character(d$DeployID)
  
  # add deploy ID from deploy info file
  u = deploy
  y = filter(u, ptt %in% d$ptt)
  d$animal = y$animal
  
  # remove locations flagged by WC (recorded in "Comment" column)
  s = filter(d, is.na(Comment) | Comment == "")
  
  # filter out User (deploy) and Mote locations 
  f = filter(s, Type == "Argos")
  
  return(f)
}

# apply the function
dfs <- lapply(files, format)
all <- bind_rows(dfs)

# select columns 
all <- select(all, animal, ptt, date, LC, Latitude, Longitude, Error.radius, Error.Semi.major.axis,
              Error.Semi.minor.axis, Error.Ellipse.orientation, IQ, NMsg)

# complete cases (remove locations with incomplete error ellipse info)
final <- all[complete.cases(all),]

# check each tag for WC Office positions (Redmond, WA)
t <- final[final$animal == "GmTag186",]

# remove erraneous locations if need to
final_sub <- final[c(1:296, 307:1381, 1383:1910, 1912:4241, 4243:4840, 4842:5536),]

# write csv
write.csv(final_sub, paste0("For Douglas Filter/", spp, "/", "GmTag184-228_Hawaii", "_WC_KalmanFiltered.csv"),
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
    summarise(EndLon = dplyr::last(Longitude))
  
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
Sum <- summ(final_sub)

# save summary file 
write.csv(Sum, "Summary files/GmTag184-228_RawLCSum.csv", row.names = F, na = "")


