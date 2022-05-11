# crc-hawaii-tags
R scripts for various types of analyses and processing of satellite tag data for Cascadia Research Collective Hawaiian odontocete projects.

## CSVtoKML.R
Script that reads data from CSV file and writes a KML file to be viewed in Google Earth.

## DistBTPairs_MapsPlots.R
Script that generates maps and plots of tracklines for pairs of tags with overlapping satellite passes (within 15 mins of eachother). First need to run **NDistSatDat_20160421.R**,
a script made by Megan Ferguson that calculates to distance between pairs with common satellite passes. Use summary statistics file as input into this script.

## DistBTSightings.R
Script that calculates distances between all possible pairs of sightings. This analysis has been done previously at CRC and was done for the recent Kogia sima paper (submitted 2021). This script includes the analysis applied to various subsets of the data (e.g., individuals pooled, by island). 

## DiveProfilePlots.R
Script that plots the dive profile of a SPLASH/Mk-10 tag. Pay particular attention to datetimes and time zones of tag/location data (tags programmed in HST will have behavior data
records in HST). Options to plot dives for entire deployment or by specified time period.

## DiveSeriesCheck.R
Script uses the same functions in DiveProfilePlots.R to plot behavior data dive profiles against time series file depth readings. This script is a part of the error check protocol/SOP - time series depths should align with behavior data dive depths (for example, a time series depth at/near the surface during a behavior file deep dive would indicate some error with the recorded data). 

## Drone_DistBearing.R
Script that calculates the distance and bearing between consecutive locations of a drone flight track, writes csv and KML files. 

## EddyVectorAnimations.R
Script that reads in all daily eddy vector plots (**EddyVectorTrackPlots.R**), annotates them with CRC logo and website link, and compiles them into an animation.
**WARNING** For large animations, easily subject to bugs (e.g., annotations not applied to all images).

## EddyVectorTrackPlots.R
Script that extracts SSH and current vector data from ERDDAP server and maps them along with individual tag trackline. 

## FastLoc-GPS_Filter.R
Script that reads in Fastloc-GPS files, summarizes raw data, and then removes locations with a residual greater than 35 and/or time error longer than 10 seconds. 

## FormatDAF_batch.R
Script that formats several Douglas-filtered location data downloaded from Movebank at once. 

## FormatKalman_forMovebank_batch.R
Script that formats Kalman reprocessed data received from Argos CLS for input into Movebank/Douglas Filter.

## FormatWC_forMovebank_batch.R
Script that formats location data obtained from Wildlife Computers portal for input into Movebank/Douglas Filter.

## GPXtoCSV.R
Script that reads GPX file from research vessel and extracts different variables (e.g., coordinates, times, waypoints), and writes csv file and/or KML file. 

## Hawaii_Geoprocess.R
Script that co-locates positional data with various geospatial variables (depth, slope, distance to shore, time of day, moon phase), as well as a number of variables 
related to different studies (e.g., monuments, HARPs).

## NDistSatDat_20160421.R
Script created by Megan Ferguson that calculates the distance between pairs of tags with overlapping satellite passes, to assess potential coordination of individuals'
movements.

## Robin's_MapPlot_Themes.R
Script that includes custom ggplot themes and other map and plot aesthetics that Robin likes.

## Shapefiles.zip
Shapefiles used in **Hawaii_Geoprocess.R**.

## append_ex.R
Example of appending newly processed data to batch/compiled file. 

## bsam.R
Script that shows example of fitting satellite tag data to a hierarchical switching state-space model using the *bsam* package. 

## dive_pseudotracks.R
Script that uses positional data to assign locations to behavior (dive) data based on time, bearings, etc. and then populates dive locations with geospatial variables.

## moveVis.R
General script for making animations of animal trajectories. 

## crawl_single.R
Script that fits telemetry data from a single tag deployment to a continuous-time correlated random walk model using crawl, and predicts locations at specified time interval. 

## crawl_batch.R
Script that fits continuous-time correlated random walk model using crawl to several deployments at once (wrapper function, tidy, nested data) and predicts locations at specified time interval. 

## crawl_fixpath.R
Script that fits crawl model (as in **crawl_batch.R** and **crawl_single.R**) and shows an example of re-routing predicted paths around land using the fix_path() function. Will need to use **crawl_fixpath_mak.R** script for this process to work. 

## crawl_fixpath_mak.R
Script that includes the base functions for the fix_path() function from the devel branch of crawl, with minor adjustments/edits. Changes to original function code include making an output object a list (rather than data frame, fixes small tibble bug) and adjusting barrier buffer in fix segments function (might need to be adjusted later). Run the code in this script prior to re-routing predicted paths in the script: **crawl_fixpath.R**

## foieGras.R
Script that fits continuous-time state-space model (random walk or correlated random walk) to either a single deployment or several deployments at once, and predicts locations at a specified time interval. Script also includes code to fit a simple time-varying move persistence model. 

## photoID_summary_plots.R
Script that summarizes and creates 4 plots for photo-ID metrics (restricted by distinctiveness and photo-quality), using Pc as an example: (1) number of distinctive individuals by year and colored by social cluster; (2) number of identifications by year and colored by social cluster; (3) number of identifications by month (across all years) and colored by social cluster; and (4) number of identifications by island-area (across all years) and colored by social cluster. 

## TravelSpeedEst.R
Script that calculates deltaT (time difference between successive locations), calculates the distance between successive points, and then calculates horizontal travel speed using the two computed metrics. **Can also get travel speed estimates from predicted output in crawl (return.type = 'flat') after fitting model. see *crawl_single.R* and *crawl_batch.R* scripts** 

## erddap_hycom_extract_traj.R
Script that extracts variables from selected datasets available on ERDDAP servers for locations along a tagged animal's movement trajectory. 

