
## load packages ##
library(sf)
library(rgeos)
library(rgdal)
library(tidyverse)
library(ggspatial)
library(ggplot2)
library(lubridate)
library(ggpubr)

# file objects
spp = "Pc"
tag_range = "Tag001-065"
doug_folder = "Douglas Filtered/"
vers = "_2020JUNv2"
dist_file = paste0("Dist between pairs/", spp, "_DistBTPairs_SummCalcs_2020JUNv1.csv")

# determine filename pattern based on max rate (r) for species
r20 = c("Sa", "Pc", "Gg", "Sb", "Tt", "Oo")
r15 = c("Gm","Pe", "Pm", "Fa")
r10 = c("Md", "Zc")

if (spp %in% r20) {
  pattern = "_DouglasFiltered_KS_r20d3lc2"
} else if (spp %in% r15) {
  pattern = "_DouglasFiltered_KS_r15d3lc2"
} else if (spp %in% r10) {
  pattern = "_DouglasFiltered_KS_r10d3lc2"
}

pattern = "_DouglasFiltered_r20d3lc2"

input_file = paste0(doug_folder, spp, tag_range, pattern, vers, ".csv")


## import location data ##
locs <- read.csv(input_file, header = T, stringsAsFactors = F)

## import file from distance between pairs analysis ##
dpairs <- read.csv(dist_file, header = T)

# format
str(dpairs)
dpairs$Date <- date(dpairs$Date)
dpairs <- dpairs %>%
  mutate(Hour = substr(rec.1, 18, 19)) %>%
  mutate(Min = substr(rec.1, 20, 21)) %>%
  mutate(Sec = substr(rec.1, 22, 23)) %>%
  mutate(Time = paste(Hour, Min, Sec, sep = ":")) %>%
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

## Demographic info file ##
Kalsum <- read.csv("Summary files/PcTag001-065_LocationSummary.csv", header = T)

# format demographic info file
colnames(Kalsum)[colnames(Kalsum) == "Individual.ID"] <- "WhaleID"
sum <- select(Kalsum, TagID, WhaleID, Population, Cluster, AgeClass, Sex, Haplotype )

## Distance between pairs summary stats ##
stats <- read.csv("Summary files/PcTag001-065_DistBTPairs_SummStats.csv", header = T)

# format 
stats$Mean <- round(stats$Mean, digits = 2)
stats$SD <- round(stats$SD, digits = 2)
stats$Median <- round(stats$Median, digits = 2)
stats$Min <- round(stats$Min, digits = 2)
stats$Max <- round(stats$Max, digits = 2)

# select columns
stats_sub <- select(stats, Pair, Mean, SD, Median, Min, Max, DaysOver)

## get coasline shapefile of islands ##
prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
coast <- readOGR("Shapefiles", layer = "Coastline")
coast <- st_as_sf(coast)# make sf object
coastr <- st_transform(coast, crs = prj)# transform to projection of data 

# filter out islands don't need
KaNih <- coastr %>%
  dplyr::filter(isle != "Hawaii") %>%
  dplyr::filter(isle != "Oahu") %>%
  dplyr::filter(isle != "kahoolawe") %>%
  dplyr::filter(isle != "Lanai") %>%
  dplyr::filter(isle != "Maui") %>%
  dplyr::filter(isle != "Molokai")

KaNihOh <- coastr %>%
  dplyr::filter(isle != "Hawaii") %>%
  dplyr::filter(isle != "kahoolawe") %>%
  dplyr::filter(isle != "Lanai") %>%
  dplyr::filter(isle != "Maui") %>%
  dplyr::filter(isle != "Molokai")

KaNihOhMau <- coastr %>%
  dplyr::filter(isle != "Hawaii")

HI <- coastr %>%
  filter(isle != "Kauai") %>%
  filter(isle != "Oahu") %>%
  filter(isle != "kahoolawe") %>%
  filter(isle != "Lanai") %>%
  filter(isle != "Maui") %>%
  filter(isle != "Molokai") %>%
  filter(isle != "Niihau")


HIMauNu <- coastr %>%
  filter(isle != "Kauai") %>%
  filter(isle != "Oahu") %>%
  filter(isle != "Niihau")

HIMauNuOh <- coastr %>%
  filter(isle != "Kauai") %>%
  filter(isle != "Niihau")

MauNu <- coastr %>%
  filter(isle != "Kauai") %>%
  filter(isle != "Oahu") %>%
  filter(isle != "Niihau") %>%
  filter(isle != "Hawaii")

MauNuOh <- coastr %>%
  filter(isle != "Kauai") %>%
  filter(isle != "Niihau") %>%
  filter(isle != "Hawaii")

OhMolLan <- coastr %>%
  filter(isle != "Kauai") %>%
  filter(isle != "Maui") %>%
  filter(isle != "Niihau") %>%
  filter(isle != "Hawaii") %>%
  filter(isle != "kahoolawe")


# Ocean basemap if want
#esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
#                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

## format location data ##
str(locs)
locs$date <- as.POSIXct(locs$date, tz = "UTC")
locs$LC <- factor(locs$LC, levels = c("DP","L3","L2","L1","L0","LA","LB","LZ"))
locs$animal <- as.factor(locs$animal)
summary(locs)
summary(locs$animal)
length(unique(locs$animal))

## add day and month columns
locs$month <- month(locs$date)
locs$day <- day(locs$date)
locs$Date <- date(locs$date)

## select pairs with common overlapping satellites as identified in the distance b/t pairs analysis ##
tag1 = "PcTagP04"
tag2 = "PcTagP06"
t1 <- locs[locs$animal == tag1,]
t2 <- locs[locs$animal == tag2,]
#t1 <- t1[c(1:777),]

Start = "2017-10-09"
End = "2018-03-09"

## subset data to only show period of overlap
t1 <- t1 %>%
  filter(between(Date, as.Date(Start), as.Date(End))) 

t2 <- t2 %>%
  filter(between(Date, as.Date(Start), as.Date(End)))

# check 
summary(t1$Date)
summary(t2$Date)

## convert to sf object ##
sf_t1 <- st_as_sf(t1, coords = c("longitud","latitude")) %>%
  st_set_crs(4326)

sf_t2 <- st_as_sf(t2, coords = c("longitud","latitude")) %>%
  st_set_crs(4326)

sf_lines_t1 <- sf_t1 %>%
  #group_by(date) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

sf_lines_t2 <- sf_t2 %>%
  #group_by(date) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")


# Robins map
map <- ggplot() +
        theme_bw() +
        layer_spatial(data = coastr, fill = "grey88", lwd = 1.1) +
        annotation_scale(location = "bl", width_hint = 0.4) +
        annotation_north_arrow(location = "tr", which_north = "true",
                               style = north_arrow_fancy_orienteering()) +
        geom_sf(data = sf_lines_t1, lwd = 0.75,  show.legend = T, aes(color = "blue")) +
        geom_sf(data = sf_lines_t2, lwd = 0.75,  show.legend = T, aes(color = "red")) +
        scale_color_discrete("Tag", labels = c(tag1, tag2)) +
        theme(
          axis.title = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.border = element_rect(colour = "black", fill = NA, size = 2)
        ) 
map


## Distance between pairs over time plots ##

# select pair
pair <- dplyr::filter(dpairs, Pair %in% c(paste0(tag1, "-", tag2), paste0(tag2, "-", tag1)))

# plot
plot <- ggplot(pair, aes(x = datetime, y = gc.dist.km)) + 
          geom_point() +
          geom_line() +
          xlab("Date (UTC)") +
          ylab("Distance between pair (km)") +
          theme_classic() +
          theme(
            axis.text = element_text(color = "black"),
            axis.title = element_text(color = "black", face = "bold")
          ) +
          scale_y_continuous(expand = c(0,0), limits = c(0,NA))

plot


## Demographic info table
sum_sub <- filter(sum, TagID %in% c(tag1,tag2)) # select tags

# make  table
tab <- ggtexttable(sum_sub, rows = NULL, theme = ttheme("blank"))
tab

## Distance b/t pairs summary stats table
stats_pair <- filter(stats_sub, Pair == paste0(tag1, ",", tag2)) # select tags
stats_pair_sub <- select(stats_pair, Mean, SD, Median, Min, Max, DaysOver) # select variables

# make table
tab_stats <- ggtexttable(stats_pair_sub, rows = NULL, cols = c("Mean (km)", "SD (km)", "Median (km)", "Min (km)",
                                                           "Max (km)", "# Days overlap"),
                         theme = ttheme("blank"))
tab_stats

## Arrange plots
fig <- ggarrange(tab, map, plot, tab_stats, ncol = 1, nrow = 4, heights = c(0.5, 1.7, 1, 0.5)) %>%
          annotate_figure( 
                       top = text_grob(paste0(tag1, " & ", tag2), color = "black", face = "bold.italic", size = 12,
                                       hjust = 2))
fig

ggsave(path = "Dist between pairs/Plots/",paste0(tag1, "-", tag2, "_DistBTPairs.jpg"), width = 8.5, height = 8, units = "in")
