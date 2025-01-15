# Need to amend so doesn't matter if some cols are missing

WIMS_Batch_Dlwd <- function(Sites){
  
  library(jsonlite)
  library(httr)
  library(ggplot2)
  library(sf)
  library(tidyverse)
  library(lubridate)
  
  df <- data.frame()  # Initialize an empty dataframe
  
  
  for (x in 1:length(Sites)) {
    base_url <- "http://environment.data.gov.uk/water-quality/"
    ending <- "id/sampling-point/"
    sample_ID <- Sites[x]
    Deters_list <- "/measurements?_limit=9999999"
    url <- paste0(base_url, ending, sample_ID, Deters_list)
    
    # Load in the api url using the httr and json packages.
    # Load in the api url using the httr and json packages.
    A_stations <- GET(url)
    stop_for_status(A_stations)  # Check for a successful HTTP status code
    
    # Check if the content type is JSON
    if (tolower(headers(A_stations)$`content-type`) %in% c("application/json", "application/json;charset=utf-8")) {
      # Proceed with processing JSON data
      api_char <- rawToChar(A_stations$content)
      api2 <- fromJSON(api_char, flatten = TRUE)  # change it from json into R readable data
      api2 <- api2$items
      # Continue processing as usual...
    } else {
      stop("Unexpected content type. Expected JSON.")
    }
    
    A_stations$status_code  # if 200 all working
    api_char <- rawToChar(A_stations$content)
    api2 <- fromJSON(api_char, flatten = TRUE)  # change it from json into R readable data
    api2 <- api2$items
    # Change to right date format
    api2 <- api2 %>%
      mutate(
        date_time = ymd_hms(sample.sampleDateTime),
        Year = year(sample.sampleDateTime)
      ) %>% 
      group_by(Year) %>% 
      mutate(
        Year_Mean = mean(result)
      ) %>% 
      
      #Add lat and longs as they're useful
      st_as_sf(coords= c("sample.samplingPoint.easting","sample.samplingPoint.northing"),
               crs=st_crs(27700)) %>% 
      st_transform(crs= st_crs(4326)) %>% 
      
      mutate(
        long=unlist(map(geometry,1)),
        lat=unlist(map(geometry,2))
      )
    
    df <- rbind(df, api2)  # Append the data to df
    print(dim(df))  #to watch df appending
    
    #     if(x==length(Sites)){   # On last iteration write a csv.
    #   write.csv(df,Output_Path)
  }
  
  df %>% 
    filter(Year>="2023",
           determinand.definition=="Ammonia(N)"
    ) %>% 
    ggplot()+geom_point(data=df, aes(date_time, result))+labs(y=paste0(df$determinant.defintion, " !"))
  
}

source("WIMS_Mon_Sites.R")

WIMS_Batch_Dlwd(api_sf$notation)
  
