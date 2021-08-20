####SURVEY MAP####
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
    'readxl',
    'mapview',
    'htmlwidgets',
    'viridisLite',
    'viridis'
  )

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)
# #you need to install phantomjs for the interactive maps
# webshot::install_phantomjs()

#loading required packages
lapply(pkgs, library, character.only = TRUE)

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


#####LOADING REQUIRED FILES####
df <- read_xlsx('data/all_mites_survey.xlsx')

####SETTING p_fructiphilus NAs TO 'NO' FOR MAPPING PURPOSES####
df$p_fructiphilus <- replace_na(df$p_fructiphilus, 'No')

####DROPS A SPECIFIC SITE WHICH MIGHT BE AN OUTLIER####
####DOUBLE CHECK SITE BEFORE USING FOR ANALYSIS####
df <- filter(df, df$id != 'James 114')

#second dataset, only Florida sites
df_fl <- filter(df, df$state == 'FL')

# #filters data to only show Tallahassee
# df_fl <- filter(df_fl, df_fl$county == 'Leon')

####GRAPHING ALL SAMPLING LOCATIONS####
#makes the dataframe into a spatial dataframe
df <-
  st_as_sf(df,
           coords = c("lon", "lat"),
           crs = 4326)

map_pf_mites <-
  mapview(
    df,
    col.regions = c("#B1B3B6", "#CB5382"),
    na.color = c("#B1B3B6"),
    alpha.regions = 1,
    zcol = 'p_fructiphilus',
    layer.name = 'P. fructiphilus Present?',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(map_pf_mites, file = './figure/map_pf_all.png')

####GRAPHING P. FRUCTIPHILUS LOCATIONS IN FLORIDA####
#makes the dataframe into a spatial dataframe
df_fl <-
  st_as_sf(df_fl,
           coords = c("lon", "lat"),
           crs = 4326)

map_mites_fl <-
  mapview(
    df_fl,
    col.regions = c("#B1B3B6", "#CB5382"),
    na.color = c("#B1B3B6"),
    alpha.regions = 1,
    zcol = 'p_fructiphilus',
    layer.name = 'P. fructiphilus Present?',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(map_mites_fl, file = '../images/map_p_fruct_fl.png')

####COMPARING MITE HOTSPOTS OVER TIME#####
#subsetting the data for different months to compare trends
fl_jan <- filter(df_fl, month == 'Jan')
fl_feb <- filter(df_fl, month == 'Feb')
fl_mar <- filter(df_fl, month == 'Mar')
fl_apr <- filter(df_fl, month == 'Apr')
fl_may <- filter(df_fl, month == 'May')
fl_jun <- filter(df_fl, month == 'Jun')
fl_jul <- filter(df_fl, month == 'Jul')
fl_aug <- filter(df_fl, month == 'Aug')
fl_sep <- filter(df_fl, month == 'Sep')
fl_oct <- filter(df_fl, month == 'Oct')
fl_nov <- filter(df_fl, month == 'Nov')
fl_dec <- filter(df_fl, month == 'Dec')

#map for february
m_feb <-
  mapview(
    fl_feb,
    col.regions = c("#B1B3B6", "#CB5382"),
    alpha.regions = 1,
    zcol = 'p_fructiphilus',
    layer.name = 'P. fructiphilus present: February',
    map.types = 'CartoDB.Positron'
  )

mapshot(m_feb, file = '../images/map_pf_feb_fl.png')

#map for july
m_jul <- mapview(
  fl_jul,
  col.regions = c("#B1B3B6", "#CB5382"),
  alpha.regions = 1,
  zcol = 'p_fructiphilus',
  layer.name = 'P. fructiphilus present: July',
  map.types = 'CartoDB.Positron'
)

mapshot(m_jul, file = '../images/map_pf_jul_fl.png')

#a way to look at both maps at the same time
leafsync::sync(m_feb, m_jul, ncol = 2, no.initial.sync = F)

###MAPPING WITH GGPLOT####
# #list of styles: http://leaflet-extras.github.io/leaflet-providers/preview/
# #maps the sf object with the Stamen Toner Lite style
# mites_map_toner <- mapview(df_fl, map.types = 'Stamen.TonerLite')
# mapshot(mites_map_toner, file = 'p_fruct_map_fl_toner.png')

#colors the points by number of mites found
map_den_pf_fl <-
  mapview(df_fl,
          cex = 'eriophyoids',
          zcol = 'eriophyoids',
          map.types = 'Stamen.TonerLite')
#saves the map
mapshot(map_den_pf_fl, file = '../images/map_den_pf_fl.png')


####GGPLOT2 METHODS####
#reads in the shapefile for Florida
fl <- st_read('Florida_Counties/Florida_Counties.shp')
# leon <- filter(fl, COUNTYNAME == 'LEON')
city <-
  st_read('City_Limits_for_Tallahassee_Florida/City_Limits_for_Tallahassee_Florida.shp')

#plots points on the map
map_gg_pf_fl <-
  ggplot() +
  geom_sf(data = fl) +
  geom_sf(
    data = df_fl,
    aes(color = p_fructiphilus, cex = eriophyoids),
    show.legend = T
  ) +
  scale_color_colorblind(2) +
  theme_tufte(base_size = 30, base_family = "gill_sans") +
  ggtitle(expression('Sites Surveyed for ' ~ italic(P.fructiphilus) ~ ' in Florida')) +
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
  ) +
  ggsave(
    '../images/map_gg_pf_fl.png',
    plot = last_plot(),
    type = "cairo",
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )

####GGMAP VERSION####
chr <-
  get_map('312 Stadium Dr, Tallahassee, FL 32304',
          maptype = "satellite",
          zoom = 16)

png(filename = '../images/ggmap_pf_city_FSU.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = df_fl,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population of P. fructiphilus in Tallahassee") +
  theme(
    plot.title = element_text(
      size = 25,
      face = "bold",
      hjust = 0.1,
      color = "grey20",
      family = "garamond"
    ),
    axis.text.x = element_text(
      color = "grey20",
      size = 20,
      angle = 0,
      hjust = .5,
      vjust = .5,
      face = "plain"
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 20,
      angle = 0,
      hjust = 1,
      vjust = 0,
      face = "bold"
    ),
    axis.title.x = element_text(
      color = "grey20",
      size = 20,
      angle = 0,
      hjust = 0.5,
      vjust = 0,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "grey20",
      size = 20,
      angle = 0,
      hjust = 0,
      vjust = 0.5,
      face = "bold"
    ),
    legend.title = element_text(color = "grey20", size = 18),
    legend.text = element_text(color = "grey20", size = 17)
  )
dev.off()

####CLEANUP####
rm(list = ls())