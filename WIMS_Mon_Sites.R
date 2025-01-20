library(jsonlite)
library(httr)
library(sf)
library(tidyverse)
library(magrittr)
library(leaflet)

#Find WIMS ref page here: https://environment.data.gov.uk/water-quality/view/doc/reference
# Just sampling sites
  
  base_url <- "http://environment.data.gov.uk/water-quality/"
  ending <- "id/sampling-point?_limit=9999999&area=6-28"   #6-28 is the area code for Wessex, find other areas here: https://environment.data.gov.uk/water-quality/id/ea-area.html
  url <- paste0(base_url,ending)

#Load in the api url using the httr and json packages. 
  A_stations <- GET(url) 
  A_stations$status_code  #if 200 all working :)
  
  api_char <- rawToChar(A_stations$content)
  api <- fromJSON(api_char, flatten=T)                             #change it from json into R readable data
  api <- api$items 
  str(api) #Check data type so we know what we're working with, our api is type dataframe

# Get site names form spatial area
  
  #Load spatial data
  
 # catch <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/Interim_WFD_2022.shp")# Catchment shapefiles
  #CAT_PHR <- catch[catch$OPCAT_NAME %in% c("Poole Harbour Rivers"),]
  
  catch_Trac <- read_sf("/dbfs/FileStore/WSX_HGray/WFD_Transitional_Water_Bodies_Cycle_3.shp") 
  CAT <- catch_Trac[catch_Trac$OPCAT_NAME == "Poole Harbour Rivers TraC",] 
  
#  CAT <- rbind(CAT_PHR,CAT_PH)
  
  CAT <- st_transform(CAT, 4326)
  
  # Transform to sf object
  
  apio <- api %>%  st_as_sf(coords= c("long","lat"),
                     crs=st_crs(4326))
  
  clip <- apio[CAT,]

  library(leaflet)

  leaflet(clip) %>% addProviderTiles(providers$Esri) %>% addCircles()

  source("WIMS_All_Deters.R")
# Load in area you wish to filter by
  BA <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/Interim_WFD_2022.shp")  %>% st_transform(4326)

#Filter 
  BA %<>% filter(MNCAT_NAME=="Avon Bristol and Somerset North Streams") %>% st_union()
  api_sf <-st_as_sf(api, coords=c("long","lat"), crs=4326)

#Spatial crop
  api_sf <- api_sf[BA,]

# Visual check
  leaflet (api_sf) %>% addProviderTiles(providers$Esri) %>% addCircleMarkers()


  
  api_sf$notation
  
  
