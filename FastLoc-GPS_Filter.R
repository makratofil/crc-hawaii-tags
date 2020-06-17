# Script for restricting Fastloc-GPS data #
# Residual < 35, time error < 10 seconds


# Michaela A. Kratofil
# 10 JUN 2020

# load libraries
library(lubridate)
library(tidyverse)

# specify species 
spp = "Gm"
tag_range = "Tag070-231"
deploy_file = paste0(spp, tag_range, "_DeployInfoForRv2.csv")

# import all douglas filtered files for species
files <- list.files(path = paste0("Fastloc-GPS/Raw files/"),
                    pattern = "-FastGPS.csv",
                    full.names = T, recursive = T)

files <- files[c(1:5, 7:10, 12:16)]

## Function to format and select all locations ##
format <- function(x) {
  # read in the tag file
  d <- read.csv(x, header = T, stringsAsFactors = F)
  
  # make date column
  d$date <- as.POSIXct(paste(d$Day, d$Time), format = "%d-%b-%y %H:%M:%S", tz = "UTC")
  
  # select columns
  s <- select(d, Name, date, Latitude, Longitude, Satellites, Bad.Sats, Residual, Time.Error)
  
  # compute total satellites
  s$Bad.Sats[is.na(s$Bad.Sats)] <- 0
  s <- s %>% mutate(TotSats = Satellites - Bad.Sats)
  
  # complete cases
  c <- s[complete.cases(s),]
  
  return(c)

}

# apply the function
dfs1 <- lapply(files, format)
all1 <- bind_rows(dfs1)

## Function to filter ##
filter <- function(x) {
  # read in the tag file
  d <- read.csv(x, header = T, stringsAsFactors = F)
  
  # make date column
  d$date <- as.POSIXct(paste(d$Day, d$Time), format = "%d-%b-%y %H:%M:%S", tz = "UTC")
  
  # select columns
  s <- select(d, Name, date, Latitude, Longitude, Satellites, Bad.Sats, Residual, Time.Error)
  
  # compute total satellites
  s$Bad.Sats[is.na(s$Bad.Sats)] <- 0
  s <- s %>% mutate(TotSats = Satellites - Bad.Sats)
  
  # complete cases
  c <- s[complete.cases(s),]
  
  # restrict locations to those with residual < 35 and time error < 10 secs
  f <- c %>%
    filter(Residual < 35) %>%
    filter(abs(Time.Error) < 10) 
  
  return(f)
}

# apply the function
dfs <- lapply(files, filter)
all <- bind_rows(dfs)

## Function to summarize counts ##

summ <- function(x) {
  
  N = x %>%
    group_by(Name) %>%
    summarise(N = n())
}

# apply function 
Sum <- summ(all)
Sum1 <- summ(all1)

write.csv(all, "FastLoc-GPS/Processed/GmTag185-231_FastGPS_Restricted_r35te10.csv", row.names = F)
