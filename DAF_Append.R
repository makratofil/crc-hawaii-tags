## DAF_Append.R: Append newly processed Douglas Filtered track data to species
## batch file.

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 27 Aug 2021

## ========================================================================== ##

## load pacakge libraries
library(dplyr)
library(lubridate)

## read in most recent version of species batch file
old <- read.csv("Douglas Filtered/SbTag001-022_DouglasFiltered_KS_r20d3lc2_2020MAYv1.csv")
str(old)

# format date
old$date <- as.POSIXct(old$date, tz = "UTC")
tz(old$date)

## read in new tags, formatted after downloading from movebank
tag1 <- read.csv("Douglas Filtered/SbTag023_DouglasFiltered_KF_r20d3lc2_Formatted.csv")
tag1$date <- as.POSIXct(tag1$date, tz = "UTC")
tz(tag1$date)

tag2 <- read.csv("Douglas Filtered/SbTag024_DouglasFiltered_KF_r20d3lc2_Formatted.csv")
tag2$date <- as.POSIXct(tag2$date, tz = "UTC")
tz(tag2$date)

## now bind everything together, arrange by tagID/animal, and date
all <- bind_rows(old, tag1) %>%
  bind_rows(., tag2) %>%
  arrange(animal, date)

# review file to make sure arrangement worked, then save
write.csv(all, "Douglas Filtered/SbTag001-024_DouglasFiltered_r20d3lc2_2021AUGv1.csv", row.names = F)
