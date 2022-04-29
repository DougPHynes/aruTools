#Annotations, are the rough start and end times of sounds of interest, that can 
#be marked on acotuic recordings.
#In this project, annotations are made with Praat on acoustic files collected from BAR-LT sensors.
#Annotations are saved as TextGrid (.txt) files. 
#The approach is to read all TextGirds for a given directory (which correspond 
#to a field site), convert the TextGrids to a dataframe using the function textgrid_to_df,
#and then mutate, map, and tabularize the data as needed.


library(phonfieldwork) #https://github.com/ropensci/phonfieldwork
library(tidyverse)
library(lubridate) #deals with times and dates
library(auk)
library(sf)
library(leaflet)

directory <- "J://acousticData/sandPond/rawData" #reading from external hard drive

files <- list.files(directory, pattern = "*.TextGrid", recursive = TRUE, full.names = TRUE) #read all .TextGrids within folder

comList <- lapply(files, textgrid_to_df) 

df <- do.call(rbind, comList)

df2 <- df %>% 
  mutate(across(where(is.character), ~ na_if(.,""))) %>%
  mutate(start_sec = round(time_start),0) %>%
  mutate(date = as_datetime(substr(source, 6, 20)), tz = "ADT") %>% #extract datetime from source, i.e., filename
  mutate(dateTimeSound = start_sec + date) %>%
  #mutate(dateTimeSound = as.character(dateTimeSound)) %>%
  mutate(lat = substr(source, 43, 50)) %>% #extract latitude
  mutate(lat = str_replace(lat, "_", ".")) %>% 
  mutate(lon = substr(source, 53, 60)) %>% #extract longitude
  mutate(lon = paste0("-",(str_replace(lon, "_", ".")))) %>%
  mutate(site = group_indices(., lat, lon)) %>% #assign unique site numbers 
  mutate(across(content, toupper)) #capitalize species codes for merging with the official birds species codes list

spp_codes <- read_csv("C://Users/HynesD/aruTools/data/IBP-AOS-LIST21.csv") #birds species codes
spp_codes <- spp_codes %>% select(SPEC, COMMONNAME, SCINAME) 

spp<-get_ebird_taxonomy()

df3 <- df2 %>%
  left_join(spp_codes, by = c("content" = "SPEC")) %>%
  left_join(spp, by = c("COMMONNAME" = "common_name")) %>%
  arrange(taxon_order)
  


spNa <- df3 %>%
  group_by(site) %>%
  mutate(nSpp = n_distinct(COMMONNAME)) %>%
  distinct(COMMONNAME, .keep_all = TRUE) %>%
  drop_na(COMMONNAME) %>%
  ungroup() %>%
  select(site, COMMONNAME, dateTimeSound, lat, lon) %>%
  filter(!COMMONNAME == "Unidentified Thrush") %>%
  #pivot_wider(names_from = COMMONNAME, values_from = dateTimeSound) %>%
  select(-site) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  



# mapviewOptions(fgb = TRUE)


# leaflet(spNa) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(data = sand, fill = NA, weight = 3) %>%
#   addCircleMarkers(
#     fillOpacity = 0.8,
#     radius = 8,
#     stroke = FALSE,
#     color = "#ff7f00")
#     
#     
#     stroke = FALSE, fillColor = "red", fillOpacity = 0.2, size = )
# 
#     
#     , lat = as.numeric(spNa$lat), lng = as.numeric(spNa$lon))
#                    
#                    
#                    
#                    , popup = popupTable(spNa))
# 
# a <- mapview(spNa, cex = 15, alpha.regions = 0.8, layer.name = "Birds",  feature.id = FALSE) +
#   mapview(sand, alpha.regions = 0, color = "gray", lwd = 3, legend = FALSE, layer.name = "Sand Pond National Wildlife Area")
#   
# mapshot(a, "C://Users/HynesD/Documents/SandPond/aruDetections2021.html")
# 
# sand <- sand %>% st_transform(crs = 4326)
