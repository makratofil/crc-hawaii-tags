## Robin's map and plot themes

## Michaela A. Kratofil
## 23 JUN 2020

###############################

## Description:
# Robin likes pretty basic maps with black text, thick borders, and grayscale themes.
# The following functions are provided for different maps/plots that meet his 
# criteria, in addition to a few other arguments that should be specified in plots.
# For example, a lot of default ggplot aesthetics don't align with Robin's

## load packages
library(ggplot2)
library(ggspatial)

## for maps 
theme_map <- function() {
  theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black', size = 1.25),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(colour = 'black', face = 'bold')) #+
  
}

# other:
geom_sf(data = coastline_shapefile, lwd = 1.1) # something around 1.1-1.5, likes thicker lines around coastline shapefile
annotation_scale(location = 'bl') # add scale bar in bottom left corner
annotation_north_arrow(location = 'tr', which_north = 'true',
                       style = north_arrow_fancy_orienteering()) # north arrow in upper right corner

# Also good to include labels for islands when plotting maps, using correct spelling/accents
# Add the final product (labs.sf) to ggplot object

# for map island labels:
island <- c(paste0("Kaua\u02BBi"), paste0("Ni\u02BBihau"), paste0("O\u02BBahu"),
            paste0("Moloka\u02BBi"), "Maui", paste0("L\u0101na\u02BBi"), paste0("Kaho\u02BBolawe"),
            paste0("Hawai\u02BBi"))

# coordinates for map labels
lat <- c(22.095198, 21.708437, 21.517844, 21.310436, 20.749506, 20.681225, 20.439420, 19.591887)
lon <- c(-159.534170, -160.0059865, -158.040082, -156.992012, -156.257611, -156.946387, -156.626687, -155.535081)

# make into dataframe and project
lon.df <- as.data.frame(lon)
labs <- cbind(island, lat)
labs.df <- as.data.frame(labs)
labs.df <- bind_cols(labs.df, lon.df)
labs.sf <- st_as_sf(labs.df, coords = c("lon","lat"), crs = 4326) # project

# also not a huge fan of titles on plots, so just name the file you save clearly to indicate what the 
# plot is. exception for dive profile plots that include title for tag and period plotted.

## for plots
plot_theme <- function() {
  theme_bw() + # or classic, but prefers to have a rectangle/box around the entire plot
    theme(axis.text = element_text(color = 'black'),
          axis.title = element_text(color = 'black', face = 'bold')) +
    scale_y_continuous(expand = c(0,0)) # if have a continuous y-axis, make start at 0 (ggplot includes gap)
}

# other:
geom_boxplot(color = 'black', outlier.color = 'black') # for boxplots; maybe make thicker lines
# apply similar aesthetics for other geom_x type arguments, black outlines/points


## unicode characters
okina = '\u02BB'
kahako = '\u014d'
abar = '\u0101'

# try using the package 'ggpattern' if need to fill patterns for different categories (avoids color)
# Viridis colors are also photo-copy and color-blind friendly 