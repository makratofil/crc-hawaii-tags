## bsam: fit satellite tag location data
## to switching state space model in bsam.

## Author: Michaela A. Kratofil, Cascadia Research Collective
## Updated: 16 Feb 2021

##=========================================================================== ##

## Before running bsam, you'll need to install rjags and JAGs. See the github
## page for this package for more details: github.com/ianjonsen/bsam

## Should also read the following papers before running a bsam model:
# (1) Jonsen et al. 2005 Robust state-space modeling of animal movement data
# (2) Jonsen et al. 2013 State-space models for bio-loggers: A methodological road map
# (3) Jonsen & Taylor 2016 Joint estimation over multiple individuals improves behavioral state inference from animal movement data

## FYI - depending on how you set up your model and how many observations you have,
## this will take a long time to run. I typically run these over night (if you do, and if you have a cat, put your keyboard somewhere
## where your cat can't lay on it) or during a time I won't need to do a lot on my computer.

## This example was used to fit a hierarchical switching state-space model to 
## spotted dolphin satellite tag data

## load packages
#packrat::init(".") # initialize packrat library for spotteds project
library(tidyverse)
library(lubridate)
library(rjags)
library(bsam)

## read in location data and format
locs <- read.csv("SaTag001-009_DouglasFiltered_KS_r20d3lc2_2020APRv1.csv", header = T)
str(locs)
locs$animal <- as.factor(locs$animal)
summary(locs)
locs$LC <- as.factor(locs$LC)
summary(locs$LC)
sub <- filter(locs, animal != "SaTag009") # remove SaTag009 from analyses
sub$date <- as.POSIXct(sub$date, tz = "GMT") # format date column

## summarize location data
sub$ymd <- date(sub$date)
sum <- sub %>%
  group_by(animal, ymd) %>%
  tally()
summ <- sum %>%
  group_by(animal) %>%
  summarise(day_avg = mean(n))

## format for input into bsam
bdf <- sub %>%
  rename(
    id = animal,
    lc = LC,
    lon = longitud,
    lat = latitude
  ) %>%
  select(id, date, lc, lon, lat)

bdf$lc <- recode(bdf$lc, DP = "3", # recode lc values 
                 L3 = "3",
                 L2 = "2",
                 L1 = "1",
                 L0 = "0",
                 LA = "A",
                 LB = "B",
                 LZ = "Z")

## fit hierarchical first difference correlated random walk switching state space model
## for location filtering, estimation, and behavioral state estimation across multiple
## deployments (hDCRWS). Jointly estimating across multiple datasets may provide better
## state estimation by borrowing strengths.

## NOTE: This is not a cookie-cutter model. Model function arguments will need to be adjusted to what 
## is most appropriate for your research questions and dataset, and will likely need to be adjusted after
## your first model (i.e., if 1st model doesn't converge). See package documentation for model arguments.

hfit5 <- fit_ssm(bdf, model = "hDCRWS", tstep = 1/24, adapt = 200000, samples = 110000, thin = 20, span = 0.4)
results.hfit5 <- get_summary(hfit5)

## save results to verify later
write.csv(results.hfit5, "SaTag001-008_bsam1hr_hfit5_2020Sep10.csv", row.names = F)
saveRDS(hfit5, "SaTag001-008_bsam1hr_hfit5.rds")

## I usually save the data and analyze outputs in separate script, but after your model is done fitting,
## analyze diagnostics appropriately (i.e., check for convergence, within-chain autocorrelation, etc.)
## These functions help
diag_ssm(hfit5)
map_ssm(hfit5)
plot_ssm(hfit5)
