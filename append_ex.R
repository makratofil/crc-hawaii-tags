## append_ex: example of appending newly processed data to compiled/batch
## data file.

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 16 Feb 2021

## ========================================================================== ##

## load packages
library(dplyr)
library(lubridate)

## read in current batch file
batch_old <- read.csv("Directory_folder/file_name.csv")
summary(batch_old)
str(batch_old)

## read in processed data to append to batch file
new_data <- read.csv("Directory_folder/file_name.csv")
summary(new_data)
str(new_data)

## since we'll want to keep data arranged by date and/or by another variable
## (e.g., Tag ID), we need to format the date for both datasets that we'll
## be binding together. The binding function won't work if any of the variables/
## columns are of different class (e.g., character date won't bind with POSIXct date)

batch_old$datetimeHST <- as.POSIXct(batch_old$datetimeHST, tz = "Pacific/Honolulu") # assuming yyyy-mm-dd hh:mm:ss format
new_data$datetimeHST <- as.POSIXct(new_data$datetimeHST, tz = "Pacific/Honolulu")

## now combine
batch_new <- dplyr::bind_rows(batch_old, new_data) %>%
  dplyr::arrange(datetimeHST)

## if working with tag data or some other data where we want to arrange by another group:
batch_new <- dplyr::bind_rows(batch_old, new_data) %>%
  dplyr::arrange(animal, datetimeHST)

## review and save the file
summary(batch_new)
str(batch_new)
write.csv(batch_new, "Directory_folder/new_file_name.csv", row.names = F)