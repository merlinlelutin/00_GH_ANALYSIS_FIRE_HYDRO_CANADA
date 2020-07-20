###########
## Script name: "HYDAT_EXTRACT_STATIONS.R"
## Description:
## Project: "CANADA_INVENTORY-HYDRO-FIRE-DATA"
## Date: 2020-07-09
## Auteur: Francois-Nicolas Robinne
## Contact: robinne@ualberta.ca
## Licence: GNU General Public License
## Notes: 
############

# Main code ---------------------------------------------------------------
# Libraries -------------------
  library(tidyhydat)
  library(dplyr)
  library(ggplot2)
  library(sf)

# Working directory ----------------
  setwd("D:/PROJECTS/21_2018_CANADA_INVENTORY-HYDRO-FIRE-DATA_ACTIVE") # For user to change as necessary


# Load data -------------------------------------------------------------
  wcs <- st_read("02_TRANSFORMED_DATA/NRCAN/WSC_Watersheds_2015.shp")

  
# Extract HYDAT gauges ---------------
# This part extracts the location (LAT,LON) of gauges active in 1980
  stns_1980_list <- hy_annual_stats(start_year = 1980, end_year = 1980) %>%
    select(STATION_NUMBER, Parameter, Year) %>%
    filter(Parameter == "Flow") %>% # Only keeping one parameter to limit station redudancy
    pull_station_number() %>%
    hy_stations() %>%
    select(STATION_NUMBER, STATION_NAME, DRAINAGE_AREA_GROSS, LATITUDE, LONGITUDE) %>% # Will remove the info on drainage area when I have the watersheds
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    
  ggplot() +
    geom_sf(data = stns_1980_list) # Plot point layer showing stations in 1980
  st_write(stns_1980_list, "02_TRANSFORMED_DATA/HYDAT/stns_1980_location.shp", append = F) # Write point shapefile
  
# This part extracts the location (LAT,LON) of gauges active between 1981 and 2019
# This part extracts the location (LAT,LON) of gauges active between 1981 and 2019
  stns_1981_2019_list <- hy_annual_stats(start_year = 1981, end_year = 2019) %>%
    select(STATION_NUMBER, Parameter, Year) %>%
    filter(Parameter == "Flow") %>% # Only keeping one parameter to limit station redudancy
    pull_station_number() %>%
    hy_stations() %>%
    select(STATION_NUMBER, STATION_NAME, DRAINAGE_AREA_GROSS, LATITUDE, LONGITUDE) %>% # Will remove the info on drainage area when I have the watersheds
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  ggplot() +
    geom_sf(data = stns_1981_2019_list) # Plot point layer showing stations between 1981 and 2019
  st_write(stns_1981_2019_list, "02_TRANSFORMED_DATA/HYDAT/stns_1981-2019_location.shp", append = F) # Write point shapefile
  
# This part extracts the location (LAT,LON) of gauges active in 1980 and still active (for at least one more year) between 1981 and 2019
  stns_1980_no_geo <- st_drop_geometry(stns_1980_list)
  stns_1981_2019_no_geo <- st_drop_geometry(stns_1981_2019_list)
  stns_1980_2019 <- intersect(stns_1980_no_geo, stns_1981_2019_no_geo) %>% # returns 1765 gauges, the number of gauges that exist in 1980 and after
    pull_station_number() %>%
    hy_stations() %>%
    select(STATION_NUMBER, STATION_NAME, DRAINAGE_AREA_GROSS, LATITUDE, LONGITUDE) %>% # Will remove the info on drainage area when I have the watersheds
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  diff_stns_1980_2019 <- setdiff(stns_1980_no_geo, st_drop_geometry(stns_1980_2019))# returns 57, meaning that 57 gauges were lost between 1980 and 2019. No direct use at the moment
  
  ggplot() +
    geom_sf(data = stns_1980_2019, col = 'red') # Plot point layer showing the 1765 stations existing in 1980 and after
  st_write(stns_1980_2019, "02_TRANSFORMED_DATA/HYDAT/stns_active_1980-2019_location.shp", append = F) # Write point shapefile
  

# Working -------------------
  stns_list_area_join <- hy_annual_stats(start_year = 1981, end_year = 2019) %>%
    select(STATION_NUMBER, Parameter, Year) %>%
    filter(Parameter == "Flow") %>%
    unique() %>%
    left_join(stns_1981_2019_no_geo) 
  
  
  yearly_drainage <- stns_list_area_join %>%
    select(STATION_NUMBER, Year, DRAINAGE_AREA_GROSS) %>%
    group_by(Year) %>%
    summarise(sum_area = sum(DRAINAGE_AREA_GROSS, na.rm = T))
  
  ggplot() +
    geom_bar(data = yearly_drainage, aes(x=Year, y=sum_area), stat="identity", fill="steelblue", width=0.7)
  