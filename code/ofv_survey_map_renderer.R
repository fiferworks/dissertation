####SURVEY MAP####
#a list of packages used for this script
pkgs <-
  c(
    'tidyverse',
    'sf',
    'ggmap',
    'ggplot2',
    'ggthemes',
    'extrafont',
    'showtext',
    'leaflet',
    'dplyr',
    'mapview',
    'htmlwidgets',
    'viridis',
    'RColorBrewer'
  )

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

# #you need to install phantomjs for the interactive maps
# webshot::install_phantomjs()


####GETTING REQUIRED FONTS####
#telling R the path where the fonts are located
font_paths("fonts")

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
df <- read_csv('data/ofv_master_datasheet.csv')

#making sure R knows what variables are factors
df$symptoms <- as_factor(df$symptoms)
df$plant_cv <- as_factor(df$plant_cv)
df$family <- as_factor(df$family)
df$order <- as_factor(df$order)

####REMOVING NAS####
#removing rows of NA
df <- drop_na(df)

#makes the dataframe into a spatial dataframe
df <-
  st_as_sf(df,
           coords = c("lon", "lat"),
           crs = 4326)

####GRAPHING SAMPLING LOCATIONS####
#creating a subset of the data containing only asparagales
asp <- df %>%
  filter(order == 'asparagales')

#making color pallet for the map
#the order goes (ringspot, stippling, none, chlorosis)
pal <- c('#C59434', '#A3B7F9')

#avoids a pandoc error 63
mapviewOptions(fgb = FALSE)

#mapping locations
map_asp_ofv <-
  mapview(
    asp,
    zcol = 'symptoms',
    col.regions = pal,
    legend = TRUE,
    alpha.regions = 1,
    layer.name = 'Ringspot Symptoms on Asparagales',
    map.types = 'Stamen.TonerLite'
  )
#list of styles: http://leaflet-extras.github.io/leaflet-providers/preview/

#saves the map
mapshot(map_asp_ofv, file = 'figure/map_asp_ofv.png')

#### Risk: Proximity of OFV-infected plants to Citrus
#pallet for map
#order: (ruscaceae, rutaceae, unknown)
pal_2 <- c('#A3B7F9', '#C59434', '#EDE6DE')

map_cit_prox <- mapview(
  df,
  zcol = 'family',
  col.regions = pal_2,
  legend = TRUE,
  alpha.regions = 1,
  layer.name = 'Proximity of Ruscaceae spp. to Citrus',
  map.types = 'Stamen.TonerLite'
)

#saves the map
mapshot(map_cit_prox, file = 'figure/map_cit_prox.png')

####LOADING SHAPEFILES####
fl <- st_read('data/Florida_Counties.shp')
# leon <- filter(fl, COUNTYNAME == 'LEON')
# city <-
#   st_read('data/City_Limits_for_Tallahassee_Florida.shp')
# musm <-
#   st_read(
#     'data/Parks_Boundaries__-_Florida_s_Capital_Region.shp'
#   )

# #using the package 'maps' instead for a map of Florida:
# counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
# counties <- subset(counties, grepl("florida", counties$ID))
# counties$area <- as.numeric(st_area(counties))

#plots points on the map
gg_ofv_map <-
  ggplot() +
  geom_sf(data = fl) +
  geom_sf(data = asp,
          aes(color = symptoms),
          show.legend = T) +
  scale_color_viridis(discrete = T,
                      direction = 1,
                      alpha = 0.5) +
  theme_tufte(base_size = 40, base_family = "gill_sans") +
  ggtitle(expression('Asparagales with Orchid Fleck Virus in Northern Florida')) +
  theme(axis.title = element_blank(),  axis.text.x = element_blank()) +
  theme(legend.position = "top") +
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
  'figure/gg_asp_map_ofv.png',
  plot = last_plot(),
  type = "cairo",
  width = 16,
  height = 9,
  scale = 1,
  dpi = 300
)

####CLEANUP####
rm(list = ls())