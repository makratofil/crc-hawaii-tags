## photoID_summary_plots.R: Make plots summarizing photoID metrics. An example
## here with the Pc catalog

## Author: Michaela A. Kratofil, Cascadia Research
## Updated: 11 May 2022

## ========================================================================== ##

## load packages
library(dplyr)
library(lubridate)
library(ggplot2)

## read in photo ID data ## ================================================= ##
all <- read.csv("pseudorca_photoID_2022APRv3_for_R_v3.csv", header = T)
summary(all) # review

# make year column a numeric class if not already
all$year_num <- as.numeric(all$Year)

# create datetime column so is easier to work with throughout (use dummy time value)
all$datetime_ch <- paste0(all$Date, " 00:00:00")

# IF the date column has different formats, parse different formats by listing
# the format types in the orders argument below
all$parse_dt <- lubridate::parse_date_time(all$datetime_ch, orders = c("%d-%b-%y %H:%M:%S",
                                                                       "%m/%d/%Y %H:%M:%S"),
                                           tz = "Pacific/Honolulu")
tz(all$parse_dt) # check

# add month
all$month <- month(all$parse_dt, label = T)
summary(all$month)

# if any NAs in the year or month values above, subset and see what's going on

# subset population of interest (if needed)
unique(all$population)
mhi <- filter(all, population == "MHI")

# subset span of years of interest
unique(mhi$Year)
mhi_sub <- filter(mhi, year_num %in% c(1999:2021))
unique(mhi_sub$year_num)
range(mhi_sub$year_num)

# restrict distinctiveness and photo quality
unique(mhi_sub$distinct)
unique(mhi_sub$photo_qual)

# check out weird ones
hm <- filter(mhi_sub, photo_qual %in% c("G",NA))

# G = genetic and NA is only one record from video encounter. exclude these 7 records total). 
mhi_dist <- filter(mhi_sub, distinct %in% c(3,4)) %>%
  filter(., photo_qual %in% c(3,4))

## number of dist individuals by year and cluster ## ======================== ##

## for this plot, we will EXCLUDE within-year resightings
unique(mhi_dist$sight_type)
mhi_dist2 <- filter(mhi_dist, sight_type %in% c("Within-island between-year",
                                                "Between-island between-year",
                                                "Original",
                                                "Final"))

# 790 records total 

# recode clusters
unique(mhi_dist2$cluster)
mhi_dist2$cluster <- recode(mhi_dist2$cluster, 
                            `2*` = "2")
unique(mhi_dist2$cluster)

# make cluster a factor
mhi_dist2$clust_fact <- factor(mhi_dist2$cluster, levels = c("5","4","3","2","1"))
summary(mhi_dist2$clust_fact)

# check/recode island
unique(mhi_dist2$Island)
mhi_dist2$island_area <- recode(mhi_dist2$Island,
                                Maui = "Maui Nui",
                                Lanai = "Maui Nui",
                                Molokai = "Maui Nui",
                                #`Maui/Kahoolawe` = "Maui Nui",
                                Kauai = paste0("Kaua\u02BBi"),
                                Oahu = paste0("O\u02BBahu"),
                                Hawaii = paste0("Hawai\u02BBi"))
unique(mhi_dist2$island_area)

# make island area a factor 
mhi_dist2$island_area <- factor(mhi_dist2$island_area, levels = c(paste0("Kaua\u02BBi"),
                                                                  paste0("O\u02BBahu"),
                                                                  "Maui Nui",
                                                                  paste0("Hawai\u02BBi")))

summary(mhi_dist2$island_area)
summary(mhi_dist2)


# now summarize the number of distinctive individuals by year and cluster
mhi_dist2 <- mhi_dist2 %>%
  rename(id = `ï..ID`)

length(unique(mhi_dist2$id)) #  206

# summarise
mhi_dist_sum <- mhi_dist2 %>%
  group_by(year_num, clust_fact) %>%
  summarise(n = length(unique(id))) # this gets the # of IDs

# make palette the same as Bradford et al. 2018
clust_pal <- c("#984EA3","#FF7F00", "#4DAF4A", "#E41A1C", "#377EB8")

# plot 
n_dist <- ggplot(mhi_dist_sum, aes(x = year_num, y = n, fill = clust_fact)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = clust_pal,
                    labels = c("Cluster 5","Cluster 4","Cluster 3","Cluster 2","Cluster 1")) +
  xlab("Year") +
  ylab("No. of distinctive individuals") +
  scale_y_continuous(limits = c(0,100), expand = c(0,0),
                     breaks = seq(0,100, by = 10)) +
  scale_x_continuous(breaks = seq(1999, 2021, by = 1),
                     expand = c(0,0)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, color = "black", size = 11, vjust = .5),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, color = "black", face = "bold"),
    legend.text = element_text(size = 11)
    #panel.border = element_rect(size = 2, fill = NA)
  )
n_dist

# save
ggsave("FKW_Abundance_PIFSC/Pc_MHI_1999-2021_Dist34_PQ34_nDistinct_byYear_v1_corrected.jpg",
       width = 8.5, height = 6, units = "in")

## number of identifications by year and cluster ## ========================= ##

# for IDENTIFICATIONS, we INCLUDE within-year resightings. so use the original 
# dataframe that is restricted by distinctiveness and photo-quality

# recode clusters
unique(mhi_dist$cluster)
mhi_dist$cluster <- recode(mhi_dist$cluster, 
                           `2*` = "2")
unique(mhi_dist$cluster)

# make cluster a factor
mhi_dist$clust_fact <- factor(mhi_dist$cluster, levels = c("5","4","3","2","1"))
summary(mhi_dist$clust_fact)

# check/recode island
unique(mhi_dist$Island)
mhi_dist$island_area <- recode(mhi_dist$Island,
                               Maui = "Maui Nui",
                               Lanai = "Maui Nui",
                               Molokai = "Maui Nui",
                               `Maui/Kahoolawe` = "Maui Nui",
                               Kauai = paste0("Kaua\u02BBi"),
                               Oahu = paste0("O\u02BBahu"),
                               Hawaii = paste0("Hawai\u02BBi"))
unique(mhi_dist$island_area)

# make island area a factor 
mhi_dist$island_area <- factor(mhi_dist$island_area, levels = c(paste0("Kaua\u02BBi"),
                                                                paste0("O\u02BBahu"),
                                                                "Maui Nui",
                                                                paste0("Hawai\u02BBi")))

summary(mhi_dist$island_area)

# fix ID name
mhi_dist <- mhi_dist %>%
  rename(id = `ï..ID`)

length(unique(mhi_dist$id)) #  206

# summarise
mhi_dist_sum2 <- mhi_dist %>%
  group_by(year_num, clust_fact) %>%
  summarise(n = n()) # this gets the total number of records

# make palette the same as Bradford et al. 2018
clust_pal <- c("#984EA3","#FF7F00", "#4DAF4A", "#E41A1C", "#377EB8")

# plot 
n_dist2 <- ggplot(mhi_dist_sum2, aes(x = year_num, y = n, fill = clust_fact)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = clust_pal,
                    labels = c("Cluster 5","Cluster 4","Cluster 3","Cluster 2","Cluster 1")) +
  xlab("Year") +
  ylab("No. of identifications") +
  scale_y_continuous(limits = c(0,180), expand = c(0,0),
                     breaks = seq(0,180, by = 20)) +
  scale_x_continuous(breaks = seq(1999, 2021, by = 1),
                     expand = c(0,0)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, color = "black", size = 11, vjust = .5),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, color = "black", face = "bold"),
    legend.text = element_text(size = 11)
    #panel.border = element_rect(size = 2, fill = NA)
  )
n_dist2

# save
ggsave("FKW_Abundance_PIFSC/Pc_MHI_1999-2021_Dist34_PQ34_nIdentifications_byYear_v1.jpg",
       width = 8.5, height = 6, units = "in")


## number of identifications by month and cluster ## ======================== ##

## will work with the restricted dataset that includes within-year re-sightings 

# now summarize the number of records per month and by cluster 
mhi_month_sum <- mhi_dist %>%
  group_by(month, clust_fact) %>%
  summarise(n = n()) 


# plot 
n_month <- ggplot(mhi_month_sum, aes(x = month, y = n, fill = clust_fact)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = clust_pal,
                    labels = c("Cluster 5","Cluster 4","Cluster 3","Cluster 2","Cluster 1")) +
  xlab("Month") +
  ylab("No. of identifications (1991-2021)") +
  scale_y_continuous(limits = c(0,300), expand = c(0,0),
                     breaks = seq(0,300, by = 50)) +
  # scale_x_continuous(breaks = seq(1999, 2021, by = 1),
  #                    expand = c(0,0)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, color = "black", face = "bold"),
    legend.text = element_text(size = 11)
    #panel.border = element_rect(size = 2, fill = NA)
  )
n_month

# save
ggsave("FKW_Abundance_PIFSC/Pc_MHI_1999-2021_Dist34_PQ34_nIdentifications_byMonth_v2.jpg",
       width = 8.5, height = 6, units = "in")


## number of identifications by island and cluster ## ======================= ##

## will work with the restricted dataset that includes within-year re-sightings 

# now summarize the number of records per month and by cluster 
mhi_island_sum <- mhi_dist %>%
  group_by(island_area, clust_fact) %>%
  summarise(n = n()) 


# plot 
n_island <- ggplot(mhi_island_sum, aes(x = island_area, y = n, fill = clust_fact)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  scale_fill_manual(values = clust_pal,
                    labels = c("Cluster 5","Cluster 4","Cluster 3","Cluster 2","Cluster 1")) +
  xlab("Island") +
  ylab("No. of identifications (1991-2021)") +
  scale_y_continuous(limits = c(0,750), expand = c(0,0),
                     breaks = seq(0,750, by = 50)) +
  # scale_x_continuous(breaks = seq(1999, 2021, by = 1),
  #                    expand = c(0,0)) +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(color = "black", size = 11),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, color = "black", face = "bold"),
    legend.text = element_text(size = 11)
    #panel.border = element_rect(size = 2, fill = NA)
  )
n_island

# save
ggsave("FKW_Abundance_PIFSC/Pc_MHI_1999-2021_Dist34_PQ34_nIdentifications_byIsland_v2.jpg",
       width = 7, height = 6, units = "in")
