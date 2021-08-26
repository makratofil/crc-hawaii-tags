## Script for restricting Fastloc-GPS data 
## Residual < 35, time error < 10 seconds

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 26 Aug 2021

## ========================================================================== ##

## load libraries
library(lubridate)
library(dplyr)
library(purrr)
library(data.table)

## import all Fastloc GPS files for species
files <- list.files(path = paste0("Fastloc-GPS/Raw files/"),
                    pattern = "-FastGPS.csv",
                    full.names = T, recursive = T)

# select files you need to filter
files
files <- files[17]
files

# read in files using purrr. skip empty rows 
file_dfs <- purrr::map_df(files, ~fread(.x, skip = "Name"))

## format data ## =========================================================== ##
# datetime 
file_dfs$date <- as.POSIXct(paste0(file_dfs$Day, " ", file_dfs$Time),
                                    format = "%d-%b-%Y %H:%M:%S", tz = "UTC")

# check
tz(file_dfs$date)

## select columns we need 
sub <- dplyr::select(file_dfs, Name, date, Latitude, Longitude, Satellites, `Bad Sats`,
                     Residual, `Time Error`)

## compute the total number of satellites (sats minus bad sats)
# first make all NA Bad Sats = 0
sub <- sub %>%
  mutate(
    `Bad Sats` = ifelse(is.na(`Bad Sats`), 0, `Bad Sats`),
    TotSats = Satellites - `Bad Sats`
    
  )

## now remove any record without decoded location
sub_comp <- sub[complete.cases(sub),]

## Now filter the data. We'll want to restrict locations to those with residual
## less than 35 and time error less than 10 seconds 
filt <- sub_comp %>%
  filter(Residual < 35) %>%
  filter(round(abs(`Time Error`),0) < 10)

## for batches: summarise data, and update location summary files ##
sum <- filt %>%
  group_by(Name) %>%
  summarise(N = n())

# add ptt column
filt$ptt <- 180168

## save file 
write.csv(filt, "FastLoc-GPS/Processed/MdTag020_FastGPS_Restricted_r35te10.csv", row.names = F)


