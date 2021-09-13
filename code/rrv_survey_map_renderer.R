####SURVEY MAPS####
#a list of packages used for this script
pkgs <-
  c(
    'tidyverse',
    'sf',
    'ggplot2',
    'ggthemes',
    'tmap',
    'extrafont',
    'showtext',
    'leaflet',
    'leafsync',
    'dplyr',
    'mapview',
    'naniar',
    'htmlwidgets',
    'viridisLite',
    'viridis'
  )

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)


####GETTING REQUIRED FONTS####
#telling R the path where the fonts are located
font_paths('./fonts')

#imports the font Gill Sans MT as the font family 'gill_sans'
font_add(
  family = 'gill_sans',
  regular = "GIL_____.TTF",
  bold = "GILB____.TTF",
  italic = "GILBI___.TTF",
  bolditalic = "GILI____.TTF"
)

#imports the font Garamond MT as the font family 'garamond'
font_add(
  family = 'garamond',
  regular = "AGaramondPro-Regular.otf",
  bold = "AGaramondPro-Bold.otf",
  italic = "AGaramondPro-BoldItalic.otf",
  bolditalic = "AGaramondPro-Italic.otf"
)

# #uncommment and run to make sure it worked
# #it should list "gill_sans" and "garamond"
# font_families()

showtext_auto()


####READING IN THE REQUIRED FILES####
df <- read_csv('data/rrv_survey_clean_datasheet.csv')

#making sure certain columns are considered as factors
df$p_fructiphilus <- as_factor(df$p_fructiphilus)
df$p_fructiphilus <- fct_rev(df$p_fructiphilus)
df$month <- lubridate::month(df$date, abbr = FALSE, label = TRUE)
df$year <- lubridate::year(df$date)

#replacing NAs for collector
df$collector <- replace_na(df$collector, 'Unknown')
df$collector <- as_factor(df$collector)

#filters data to only show Florida sites
df_fl <- df %>% filter(df$state == 'FL')

####GRAPHING ALL SAMPLING LOCATIONS####
#makes the dataframe into a spatial dataframe
df <-
  st_as_sf(df,
           coords = c("lon", "lat"),
           crs = 4326) # data was collected by phone GPS, should be using WGS84

# fgb isn't supported right now
mapviewOptions(fgb = FALSE)

map_pf_mites <-
  mapview(
    df,
    col.regions = c("yellow", "gray50"),
    homebutton = FALSE,
    cex = 'eriophyoids',
    na.color = c("gray50"),
    alpha.regions = 0.8,
    zcol = 'p_fructiphilus',
    layer.name = 'P. fructiphilus Present',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(map_pf_mites, file = 'figure/rrv_survey_map_all_pf.png')

####GRAPHING P. FRUCTIPHILUS LOCATIONS IN FLORIDA####
#makes the dataframe into a spatial dataframe
df_fl <-
  st_as_sf(df_fl,
           coords = c("lon", "lat"),
           crs = 4326)

map_mites_fl <-
  mapview(
    df_fl,
    col.regions = c("yellow", "gray50"),
    homebutton = FALSE,
    cex = 'eriophyoids',
    na.color = c("gray50"),
    alpha.regions = 0.8,
    zcol = 'p_fructiphilus',
    layer.name = 'P. fructiphilus Present',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(map_mites_fl, file = 'figure/rrv_survey_map_fl_pf.png')

map_other_mites_fl <-
  mapview(
    df_fl,
    col.regions = viridis(
      51,
      option = 'E',
      begin = 0.5,
      end = 1
    ),
    homebutton = FALSE,
    cex = 'other_mites',
    na.color = c("gray50"),
    alpha.regions = 0.8,
    zcol = 'other_mites',
    layer.name = 'Other mites found on roses',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(map_other_mites_fl, file = 'figure/rrv_survey_map_fl_other.png')

####TMAP METHODS####
#reads in the shapefile for Florida
fl <- st_read('data/Florida_Counties.shp')

#read in data for small multiples maps:
fl_locs <- read_csv("data/rrv_survey_fl_table.csv")
fl_locs %>% filter(fl_locs$erios > 0)
fl_locs <- st_as_sf(fl_locs,
                    coords = c("lon", "lat"),
                    crs = 4326)
#making vector into factors and reordering so 'Yes' appears first in the legend:
fl_locs$'Eriophyoids' <- fl_locs$pfruct
fl_locs$Eriophyoids <-
  ifelse(fl_locs$Eriophyoids == TRUE, "Yes", "No")
fl_locs$Eriophyoids <- as_factor(fl_locs$Eriophyoids)

#plotting the places where we found erios
fl_map_erios <-
  tm_shape(fl) + tm_fill() + tm_borders() +
  tm_shape(fl_locs %>% filter(year != 2017)) +
  tm_dots("Eriophyoids",
          palette = viridis(
            2,
            alpha = 1,
            begin = 1,
            end = 0.5,
            direction = 1,
            option = "E"
          )) +
  tm_facets(by = 'year',
            ncol = 2,
            free.coords = FALSE) + tm_scale_bar(position = 'left') + tm_layout(
              main.title.size = 3.5,
              fontfamily = "gill_sans",
              main.title = 'Eriophyoid mites found on roses in northern Florida',
              main.title.position = "left"
            )

tmap_save(fl_map_erios, scale = 3, filename = "figure/rrv_survey_map_years_pf.png")

#plotting the places where we found other mites
fl_map_other <-
  tm_shape(fl) + tm_fill() + tm_borders() +
  tm_shape(fl_locs %>% filter(year != 2017)) +
  tm_dots('other',
          palette = 'viridis') +
  tm_facets(by = 'year',
            ncol = 2,
            free.coords = FALSE) + tm_scale_bar(position = 'left') + tm_layout(
              main.title.size = 3.5,
              fontfamily = 'gill_sans',
              main.title = 'Other mites found on roses in northern Florida',
              main.title.position = "left"
            )

tmap_save(fl_map_other, scale = 3, filename = "figure/rrv_survey_map_years_others.png")


####CLEANUP####
rm(list = ls())