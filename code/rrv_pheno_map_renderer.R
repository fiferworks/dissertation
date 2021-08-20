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
    'leafsync',
    'dplyr',
    'readxl',
    'mapview',
    'htmlwidgets',
    'viridis'
  )

#installs the packages if you don't have them already installed
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# #you need to install phantomjs for the interactive maps
# webshot::install_phantomjs()

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####GETTING REQUIRED FONTS####
#telling R the path where the fonts are located
font_paths('../fonts')

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
df <- read_xlsx('clean_pheno_datasheet.xlsx')

#dropping unnecessary columns
df <-
  select(
    df,
    'sample_no',
    'id',
    'date',
    'month',
    'other_mites',
    'eriophyoids',
    'grams_dry_weight',
    'mites_per_gram',
    'pf_per_gram',
    'lon',
    'lat',
    'lon_lat',
    'city',
    'state',
    'county',
    'shade',
    'collector'
  )

####GRAPHING ALL SAMPLING LOCATIONS####
#makes the dataframe into a spatial dataframe
df <-
  st_as_sf(df,
           coords = c("lon", "lat"),
           crs = 4326)

#map of mite locations
#list of styles: http://leaflet-extras.github.io/leaflet-providers/preview/
map_pf_pheno <-
  mapview(
    df,
    na.color = c("#B1B3B6"),
    alpha.regions = 1,
    cex = 'pf_per_gram',
    zcol = 'pf_per_gram',
    layer.name = 'P. fructiphilus Per Gram of Rose Tissue',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(map_pf_pheno, file = '../images/map_pf_pheno.png')

####GRAPHING P. FRUCTIPHILUS LOCATIONS IN FLORIDA####
map_otmites_pheno <-
  mapview(
    df,
    alpha.regions = 1,
    cex = 'mites_per_gram',
    zcol = 'mites_per_gram',
    layer.name = 'Other Mites in the Landscape',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(map_otmites_pheno, file = '../images/map_otmites_pheno.png')


####COMPARING POPULATIONS OVER TIME#####
#subsetting the data for different months to compare trends
pf_jan <- filter(df, month == "1")
pf_feb <- filter(df, month == "2")
pf_mar <- filter(df, month == "3") #
pf_apr <- filter(df, month == "4") #
pf_may <- filter(df, month == "5") #Pruned!
pf_jun <- filter(df, month == "6") #
pf_jul <- filter(df, month == "7")
pf_aug <- filter(df, month == "8") #
pf_sep <- filter(df, month == "9")
pf_oct <- filter(df, month == "10") #
pf_nov <- filter(df, month == "11") #
pf_dec <- filter(df, month == "12")

#map for April
m_apr <- mapview(
  pf_apr,
  alpha.regions = 1,
  cex = 'pf_per_gram',
  zcol = 'pf_per_gram',
  layer.name = 'P. fructiphilus Phenology: April',
  map.types = 'CartoDB.Positron'
)

mapshot(m_apr, file = '../images/map_pf_pheno_apr.png')

#map for Jun
m_jun <-
  mapview(
    pf_jun,
    alpha.regions = 1,
    cex = 'pf_per_gram',
    zcol = 'pf_per_gram',
    layer.name = 'P. fructiphilus Phenology: June',
    map.types = 'CartoDB.Positron'
  )

mapshot(m_jun, file = '../images/map_pf_pheno_jun.png')

#a way to look at both maps at the same time
leafsync::sync(m_apr, m_jun, ncol = 2, no.initial.sync = F)

####GGMAP MAPS####
chr <-
  get_map('312 Stadium Dr, Tallahassee, FL 32304',
          maptype = "satellite",
          zoom = 20)

#map of pheno site in March
png(filename = '../images/ggmap_pf_pheno_mar.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = pf_mar,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population Phenology of P. fructiphilus in Tallahassee: March") +
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

#map of pheno site in April
png(filename = '../images/ggmap_pf_pheno_apr.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = pf_apr,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population Phenology of P. fructiphilus in Tallahassee: April") +
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

#map of pheno site in May
png(filename = '../images/ggmap_pf_pheno_may.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = pf_may,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population Phenology of P. fructiphilus in Tallahassee: May") +
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

png(filename = '../images/ggmap_pf_pheno_jun.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = pf_jun,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population Phenology of P. fructiphilus in Tallahassee: June") +
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

png(filename = '../images/ggmap_pf_pheno_aug.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = pf_aug,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population Phenology of P. fructiphilus in Tallahassee: August") +
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

png(filename = '../images/ggmap_pf_pheno_oct.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = pf_oct,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population Phenology of P. fructiphilus in Tallahassee: October") +
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

png(filename = '../images/ggmap_pf_pheno_nov.png',
    width = 1280,
    height = 1280)
ggmap(chr) +
  geom_sf(data = pf_nov,
          aes(cex = eriophyoids, color = eriophyoids),
          inherit.aes = F) +
  scale_color_viridis(option = 'C') +
  coord_sf(crs = st_crs(4326)) +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Population Phenology of P. fructiphilus in Tallahassee: November") +
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