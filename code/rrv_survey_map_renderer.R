####SURVEY MAPS####
#a list of packages used for this script
pkgs <-
  c(
    'tidyverse',
    'sf',
    'ggplot2',
    'ggthemes',
    'ggmap',
    'extrafont',
    'showtext',
    'leaflet',
    'leafsync',
    'dplyr',
    'mapview',
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
           crs = 4326)

mapviewOptions(fgb = FALSE)

map_pf_mites <-
  mapview(
    df,
    col.regions = c("#00326FFF", "gray50"),
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
    col.regions = c("#00326FFF", "gray50"),
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

# ####COMPARING MITE HOTSPOTS OVER TIME#####
# #subsetting the data for different months to compare trends
# fl_jan <- filter(df_fl, month == 'January')
# fl_feb <- filter(df_fl, month == 'February')
# fl_mar <- filter(df_fl, month == 'March')
# fl_apr <- filter(df_fl, month == 'April')
# fl_may <- filter(df_fl, month == 'May')
# fl_jun <- filter(df_fl, month == 'June')
# fl_jul <- filter(df_fl, month == 'July')
# fl_aug <- filter(df_fl, month == 'August')
# fl_sep <- filter(df_fl, month == 'September')
# fl_oct <- filter(df_fl, month == 'October')
# fl_nov <- filter(df_fl, month == 'November')
# fl_dec <- filter(df_fl, month == 'December')
#
# #map for february
# m_feb <-
#   mapview(
#     fl_feb,
#     col.regions = c("#00326FFF", "gray50"),
#     alpha.regions = 0.8,
#     zcol = 'p_fructiphilus',
#     layer.name = 'P. fructiphilus present: February',
#     map.types = 'CartoDB.Positron'
#   )
#
# mapshot(m_feb, file = 'figure/map_pf_feb_fl.png')
#
# #map for july
# m_jul <- mapview(
#   fl_jul,
#   col.regions = c("#00326FFF", "gray50"),
#   alpha.regions = 0.8,
#   zcol = 'p_fructiphilus',
#   layer.name = 'P. fructiphilus present: July',
#   map.types = 'CartoDB.Positron'
# )
#
# mapshot(m_jul, file = 'figure/map_pf_jul_fl.png')
#
# #a way to look at both maps at the same time
# leafsync::sync(m_feb, m_jul, ncol = 2, no.initial.sync = F)

###MAPPING WITH GGPLOT####
# #list of styles: http://leaflet-extras.github.io/leaflet-providers/preview/
# #maps the sf object with the Stamen Toner Lite style
# mites_map_toner <- mapview(df_fl, map.types = 'Stamen.TonerLite')
# mapshot(mites_map_toner, file = 'p_fruct_map_fl_toner.png')

# #colors the points by number of mites found
# map_den_pf_fl <-
#   mapview(df_fl,
#           cex = 'eriophyoids',
#           zcol = 'eriophyoids',
#           map.types = 'Stamen.TonerLite')
# #saves the map
# mapshot(map_den_pf_fl, file = '../images/map_den_pf_fl.png')


####GGPLOT2 METHODS####
#reads in the shapefile for Florida
fl <- st_read('data/Florida_Counties.shp')
# leon <- filter(fl, COUNTYNAME == 'LEON')
city <-
  st_read('data/City_Limits_for_Tallahassee_Florida.shp')

#plots points on the map
map_gg_pf_fl <-
  ggplot() +
  geom_sf(data = fl) +
  geom_sf(
    data = df_fl,
    aes(color = p_fructiphilus, cex = eriophyoids),
    show.legend = T
  ) +
  scale_color_viridis_d(
    na.value = 'gray50',
    option = 'E',
    begin = 0.1,
    end = 0.5
  ) +
  theme_tufte(base_size = 30, base_family = "gill_sans") +
  ggtitle(expression(
    'Sites Surveyed for ' ~ italic(P.fructiphilus) ~ ' in Florida 2017-2021'
  )) +
  theme(axis.title = element_blank(),  axis.text.x = element_blank()) +
  theme(
    plot.title = element_text(
      size = 100,
      face = "bold",
      family = "garamond"
    ),
    axis.text.x = element_text(
      color = "grey20",
      size = 50,
      angle = 0,
      hjust = .5,
      vjust = .5,
      face = "plain"
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 50,
      angle = 0,
      hjust = 1,
      vjust = 0,
      face = "bold"
    )
  )
ggsave(
  'figure/rrv_survey_map_gg_pf_fl.png',
  plot = last_plot(),
  type = "cairo",
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

# ####GGMAP VERSION####
# chr <-
#   get_map('312 Stadium Dr, Tallahassee, FL 32304',
#           maptype = "satellite",
#           zoom = 16)
#
# png(filename = 'figure/ggmap_pf_city_FSU.png',
#     width = 1280,
#     height = 1280)
# ggmap(chr) +
#   geom_sf(data = df_fl,
#           aes(cex = eriophyoids, color = eriophyoids),
#           inherit.aes = F) +
#   scale_color_viridis(option = 'C') +
#   coord_sf(crs = st_crs(4326)) +
#   labs(x = "Longitude",
#        y = "Latitude",
#        title = "Population of P. fructiphilus in Tallahassee") +
#   theme(
#     plot.title = element_text(
#       size = 25,
#       face = "bold",
#       hjust = 0.1,
#       color = "grey20",
#       family = "garamond"
#     ),
#     axis.text.x = element_text(
#       color = "grey20",
#       size = 20,
#       angle = 0,
#       hjust = .5,
#       vjust = .5,
#       face = "plain"
#     ),
#     axis.text.y = element_text(
#       color = "grey20",
#       size = 20,
#       angle = 0,
#       hjust = 1,
#       vjust = 0,
#       face = "bold"
#     ),
#     axis.title.x = element_text(
#       color = "grey20",
#       size = 20,
#       angle = 0,
#       hjust = 0.5,
#       vjust = 0,
#       face = "bold"
#     ),
#     axis.title.y = element_text(
#       color = "grey20",
#       size = 20,
#       angle = 0,
#       hjust = 0,
#       vjust = 0.5,
#       face = "bold"
#     ),
#     legend.title = element_text(color = "grey20", size = 18),
#     legend.text = element_text(color = "grey20", size = 17)
#   )
# dev.off()

####CLEANUP####
rm(list = ls())