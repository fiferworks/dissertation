#a list of packages used for this script
pkgs <-
  c(
    'tidyverse',
    'readxl',
    'sf',
    'spdep',
    'spDataLarge',
    'ggmap',
    'readxl',
    'writexl'
  )

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#####LOADING REQUIRED FILES####
df <- read_xlsx('ofv_clean_datasheet.xlsx')

#making sure R knows what variables are factors
df$symptoms <- as_factor(df$symptoms)
df$plant_cv <- as_factor(df$plant_cv)
df$family <- as_factor(df$family)
df$order <- as_factor(df$order)

####REMOVING NAS####
#removing columns of NA
df <- drop_na(df)

#making the data a spatial object
df <-
  st_as_sf(df,
           coords = c("lon", "lat"),
           crs = 4326)

#dropping duplicated GPS coordinates
df <- df %>% distinct(lon_lat, .keep_all = TRUE)

####Disease Progress at the Tallahassee Museum sites####
#selecting only Tallahassee Museum sites
#str_detect with filter after creating a single string from 'muse' collapsed by | (OR)
m <- c('muse')
tm <- df %>%
  filter(str_detect(id, str_c(m, collapse = "|")))

#making the data a spatial object
tm <-
  st_as_sf(tm,
           coords = c("lon", "lat"),
           crs = 4326)

####CALCULATING KNN####
city <-st_read('City_Limits_for_Tallahassee_Florida/City_Limits_for_Tallahassee_Florida.shp')
musm <-st_read('Parks_Boundaries__-_Florida_s_Capital_Region-shp/Parks_Boundaries__-_Florida_s_Capital_Region.shp')

#knn on coordinates
df_knn <- knearneigh(df$geometry, k = 3)
tm_knn <- knearneigh(tm$geometry, k = 5)

#reading shapefile geometry for plots
df_coords <- st_geometry(df)
tm_coords <- st_geometry(tm)

#plotting city knn
png("../images/map_knn_city_ofv.png", width = 1920, height = 1080)
plot(knn2nb(df_knn), df_coords)
plot(st_geometry(city), border = 'grey', add = T)
title(main = "Knn of OFV-infected plants in Tallahassee, k = 2")
dev.off()

#plotting museum knn
png("../images/map_knn_muse_ofv.png", width = 1920, height = 1080)
plot(knn2nb(tm_knn), tm_coords)
plot(st_geometry(musm), border = 'grey', add = T)
plot(st_geometry(city), border = 'grey', add = T)
title(main = "Knn of OFV-infected plants in Museum, k = 5")
dev.off()

####CLEANUP####
rm(list = ls())