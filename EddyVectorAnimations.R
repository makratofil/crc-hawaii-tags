### Create animation of plots, and write video or GIF file ###

## Michaela A. Kratofil
## 22 JUN 2020

##############################################################

## WARNING: the magick package seems to be pretty glitchy when
## using lapply() to apply all the image editing functions
## to all daily plots. This code is not bug proof, use at your
## own risk.

# load packages
library(magick)
library(magrittr)
library(dplyr)

# file and folder objects
TagID = "OoTag045"
folder = "Eddy vector plots and animations/"

# read in CRC logo
logo <- image_read("CRC logo no background.png")
print(logo)

# list of files to import
files <- list.files(path = paste0(folder, TagID, "/"), pattern = "EddyVectorPlot",
                    full.names = T, recursive = F)
files # check

# function to read image files using magick's function
# locations of logo and website link will have to be adjusted for each plot
read.image <- function(x, u) {
  
  # CRC logo, scale
  u = logo
  u <- image_scale(u, "x200")
  
  # read images, annotate with link
  i <- image_read(x)
  
  
  v <- image_annotate(i, "www.cascadiaresearch.org/projects/hawaii", size = 40,
                    gravity = 'west', location = '+650+420')
  
  # composite/combine images
  c <- image_composite(v, u, offset = '+140+600')
  
  return(c)
  
}

# apply function
images <- lapply(files, read.image)

# group and scale images for full deployment
all <- image_scale(c(images[[1]], images[[2]], images[[3]], images[[4]], images[[5]], images[[6]],
                     images[[7]], images[[8]], images[[9]], images[[10]], images[[11]], images[[12]],
                     images[[13]], images[[14]], images[[15]], images[[16]], images[[17]], images[[18]],
                     images[[19]], images[[20]], images[[21]], images[[22]], images[[23]], images[[24]],
                     images[[25]]))

# check placement of logo and website link
images[[20]]

# write gif file 
image_write_gif(all, paste0(folder, TagID, "/", TagID, "_EddyVectorAnimation_FullDply_LogoLink.gif"),
                delay = 1)

