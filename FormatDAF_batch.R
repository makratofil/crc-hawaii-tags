#######################################
#
#       Format Douglas-filtered
#           location data
#
#         Michaela A. Kratofil
#           April 28, 2020
#
#######################################

# load packages
library(tidyverse)
library(lubridate)

# define species batch
spp = "Gm"
tag_range = "Tag070-231"

# determine filename pattern based on max rate (r) for species
r20 = c("Sa", "Pc", "Gg", "Sb", "Tt", "Oo")
r15 = c("Gm","Pe", "Pm", "Fa")
r10 = c("Md", "Zc")

if (spp %in% r20) {
  pattern = "_DouglasFiltered_KS_r20d3lc2"
} else if (spp %in% r15) {
  pattern = "DouglasFiltered_KS_r15d3lc2"
} else if (spp %in% r10) {
  pattern = "DouglasFiltered_KS_r10d3lc2"
}

pattern = "DouglasFiltered"

# import all douglas filtered files for species
files <- list.files(path = paste0("Douglas Filtered/", spp, "/"), pattern = pattern,
                    full.names = T, recursive = F)

files <- files[c(1:21, 23:35, 37)]

# import deployment information for tags 
deploy <- read.csv(paste0("Summary files/", spp, tag_range, "_DeployInfoForRv2.csv"), header = T)
deploy$date <- as.POSIXct(deploy$date, tz = "UTC")
deploy$LC <- "DP"
#deploy <- filter(deploy, animal != "TtTag034")
#deploy <- filter(deploy, animal != "TtTag035")
#deploy <- dplyr::select(deploy, animal, ptt, date, latitude, longitud, LC)


## Format function for Douglas-filtered files ##
format <- function(x, u) {
  # read in file
  d <- read.csv(x, header = T, stringsAsFactors = F)
  
  # format files
  colnames(d)[colnames(d)== "individual.local.identifier"] <- "animal"
  colnames(d)[colnames(d)== "tag.local.identifier"] <- "ptt"
  d$date <- as.POSIXct(d$timestamp, tz = "UTC")
  colnames(d)[colnames(d)=="location.lat"] <- "latitude"
  colnames(d)[colnames(d)=="location.long"] <- "longitud"
  colnames(d)[colnames(d) == "argos.error.radius"] <- "error_radius"
  colnames(d)[colnames(d) == "argos.orientation"] <- "ellipse_orient"
  colnames(d)[colnames(d) == "argos.semi.major"] <- "semi_major"
  colnames(d)[colnames(d) == "argos.semi.minor"] <- "semi_minor"
  colnames(d)[colnames(d) == "argos.lc"] <- "LC"
  d$LC <- paste("L", d$LC, sep = "")
  d$LC <- factor(d$LC, levels = c("L3","L2","L1","L0","LA","LB","LZ"))
  
  # add deployment info
  u = deploy
  y = dplyr::filter(u, animal %in% d$animal)
  f = bind_rows(y, d)
  
  # format 'visible' column
  f$visible <- as.character(f$visible)
  return(f)
}

## apply to all files using lapply() ##
dfs <- lapply(files, format)
df_batch <- bind_rows(dfs)

# select columns of interest 
batch_final <- dplyr::select(df_batch, animal, ptt, date, longitud, latitude,
                      LC, error_radius, ellipse_orient, semi_major,
                      semi_minor)

# add column for location type 
batch_final$LocType <- NA

# specify location type
KS = c("GmTag070", "GmTag080", "GmTag081", "GmTag082", "GmTag083", "GmTag115", "GmTag152",
       "GmTag153", "GmTag214", "GmTag231")

batch_final <- batch_final %>%
  mutate(
    LocType = ifelse(animal %in% KS, "Kalman smoothed", NA)
  )

batch_final$LocType[is.na(batch_final$LocType)] <- "Kalman filtered"


# write csv
write.csv(batch_final, file = paste0("Douglas Filtered/", spp, tag_range,"_", pattern,"_2020JUNv1.csv"), row.names = F,
          na = "")

## Function to summarize counts of LC classes (checks for uploading errors in Movebank) ##
summ <- function(x) {
 
   N = x %>%
    group_by(animal) %>%
    summarise(N = n())
   
  LC3 = x %>%
    group_by(animal)%>%
    filter(LC == "L3") %>%
    summarise(LC3 = n())
  
  LC2 = x %>%
    group_by(animal) %>%
    filter(LC == "L2") %>%
    summarise(LC2 = n())
  
  LC1 = x %>%
    group_by(animal) %>%
    filter(LC == "L1") %>%
    summarise(LC1 = n())
  
  LC0 = x %>%
    group_by(animal) %>%
    filter(LC == "L0") %>%
    summarise(LC0 = n())
  
  LCA = x %>%
    group_by(animal) %>%
    filter(LC == "LA") %>%
    summarise(LCA = n())
  
  LCB = x %>%
    group_by(animal) %>%
    filter(LC == "LB") %>%
    summarise(LCB = n())
  
  LCZ = x %>%
    group_by(animal) %>%
    filter(LC == "LZ") %>%
    summarise(LCZ = n())
  
  # this one is to check for LCs interpreted as blanks in Movebank
  L_ = x %>%
    group_by(animal) %>%
    filter(LC == "L") %>%
    summarise(LC_ = n())
  
  DP = x %>%
    group_by(animal) %>%
    filter(LC == "DP") %>%
    summarise(DP = n())
  
  f_sum <- left_join(N, LC3, by = "animal") %>%
    left_join(LC2, by = "animal") %>%
    left_join(LC1, by = "animal") %>%
    left_join(LC0, by = "animal") %>%
    left_join(LCA, by = "animal") %>%
    left_join(LCB, by = "animal") %>%
    left_join(LCZ, by = "animal") %>%
    left_join(L_, by = "animal") %>%
    left_join(DP, by = "animal")
}

# apply function 
Sum <- summ(batch_final)

# write .csvs to check for LC errors in Movebank
write.csv(Sum, paste0(spp, tag_range, "_DouglasFilter_LCSumm_2020MAYv1.csv"), row.names = F, na = "")

# summarize number of least squares locations from older files
old <- read.csv("SaTag001-009_GIS_LeastSquares.csv", header = T)
colnames(old)[colnames(old) == "lc94"] <- "LC"
oldSum <- summ(old)
