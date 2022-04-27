
library(phonfieldwork)

directory <- "J://acousticData/sandPond/rawData"





#base_output_directory <- "C://Users//HynesD//Desktop//out"
#textgrid_to_df("J://acousticData/sandPond/rawData/5826/e2Sd128/8604_20210713_every1d6x4hrs [+43.81679-065.83275]/8604_20210713T160000-0300_every1d6x4hrs__+43_81679-065_83275_.TextGrid")

# Get a list of audio files inside the directory
# (Get-ChildItem is just like ls, or dir)
#files <- list.files(directory, pattern = "*.wav", recursive = TRUE, full.names = TRUE)
files <- list.files(directory, pattern = "*.TextGrid", recursive = TRUE, full.names = TRUE)

comList <- lapply(files, textgrid_to_df)

df <- do.call(rbind, comList)


df2 <- df %>% 
  mutate(date = as_datetime(substr(source, 6, 20)), tz = "ADT") %>%
  mutate(lat = substr(source, 43, 50)) %>%
  mutate(lat = str_replace(lat, "_", ".")) %>%
  mutate(lon = substr(source, 53, 60)) %>%
  mutate(lon = paste0("-",(str_replace(lon, "_", ".")))) %>%
  mutate(site = group_indices(., lat, lon)) %>%
  mutate(across(content, toupper)) %>%
  group_by(site) %>%
  
  mutate(sppRich =  )
  
spp_codes <- read_csv("C://Users/HynesD/aruTools/data/IBP-AOS-LIST21.csv")
spp_codes <- spp_codes %>% select(SPEC, COMMONNAME, SCINAME)


df3 <- df2 %>% left_join(spp_codes, by = c("content" = "SPEC"))
