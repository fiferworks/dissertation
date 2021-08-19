#a list of packages used for this script
pkgs <-
  c('tidyverse',
    'igraph',
    'geosphere',
    'sp',
    'sf')

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

#####LOADING REQUIRED FILES####
df <- read_csv('ofv_clean_datasheet.csv')

#selecting only Tallahassee Museum sites
#str_detect with filter after creating a single string from 'muse' collapsed by | (OR)
m <- c('muse')
df <- df %>%
  filter(str_detect(id, str_c(m, collapse = "|")))

#splitting the dataset
#ruscaceae
aspg <- df %>%
  filter(order == 'asparagales') %>%
  select(id, lon, lat, plant_cv, symptoms, source)

#rutaceae
cit <- df %>% filter(family == 'rutaceae') %>%
  select(id, lon, lat, plant_cv, symptoms, source)
cit <- drop_na(cit)

#splits by the two largest sampling efforts
df1 <- aspg %>% filter(id != 'muse6')
df2 <- aspg  %>% filter(id == 'muse6')

cit1 <- cit %>% filter(id != 'muse6')
cit2 <- cit %>% filter(id == 'muse6')

#gives the distances of df1 to each of the observations in df2
D = distm(df1[, 2:3], df2[, 2:3])
D2 = distm(cit1[, 2:3], cit2[, 2:3])

#which.min picks out the smallest entry in each row & uses this to index df1
tmp_df <- bind_cols(df1, df2[apply(D, 1, which.min), ])
tmp_cit <- bind_cols(cit1, cit2[apply(D2, 1, which.min), ])

#splitting dataframe by time
df1 <- tmp_df %>%
  select(id...1,
         lon...2,
         lat...3,
         plant_cv...4,
         symptoms...5,
         source...6)

df2 <- tmp_df %>%
  select(id...7,
         lon...8,
         lat...9,
         plant_cv...10,
         symptoms...11,
         source...12)

cit1 <- tmp_cit %>%
  select(id...1,
         lon...2,
         lat...3,
         plant_cv...4,
         symptoms...5,
         source...6)
cit2 <- tmp_cit %>%
  select(id...7,
         lon...8,
         lat...9,
         plant_cv...10,
         symptoms...11,
         source...12)

#data which matched cultivar and GPS, no citrus matched correctly
good_cv <- tmp_df %>%
  filter(tmp_df$plant_cv...4 == tmp_df$plant_cv...10) %>%
  rename(
    'id' = 'id...1',
    'lon' = 'lon...2',
    'lat' = 'lat...3',
    'plant_cv' = 'plant_cv...4',
    'symptoms' = 'symptoms...5',
    'source' = 'source...6',
    'id2' = 'id...7',
    'lon2' = 'lon...8',
    'lat2' = 'lat...9',
    'plant_cv2' = 'plant_cv...10',
    'symptoms2' = 'symptoms...11',
    'source2' = 'source...12',
  )


#averaging GPS coordinates for the same plants
good_cv$lon <- (good_cv$lon + good_cv$lon2) / 2
good_cv$lat <- (good_cv$lat + good_cv$lat2) / 2
good_cv$lon2 <- good_cv$lon
good_cv$lat2 <- good_cv$lat

gcv_1 <- good_cv %>%
  select(id, lon, lat, plant_cv, symptoms, source)
gcv_2 <- good_cv %>%
  select(id2, lon2, lat2, plant_cv2, symptoms2, source2) %>%
  rename(
    id = id2,
    lon = lon2,
    lat = lat2,
    plant_cv = plant_cv2,
    symptoms = symptoms2,
    source = source2
  )

#recombining the dataset
# good_cv <- bind_rows(gcv_1, gcv_2)

#now this tibble is ready to be split once the citrus and other plants are added again

#these are mismatched, so we will have to split them out and use them as they are without averaging coordinates
bad_cv <- tmp_df %>%
  filter(tmp_df$plant_cv...4 != tmp_df$plant_cv...10) %>%
  rename(
    'id' = 'id...1',
    'lon' = 'lon...2',
    'lat' = 'lat...3',
    'plant_cv' = 'plant_cv...4',
    'symptoms' = 'symptoms...5',
    'source' = 'source...6',
    'id2' = 'id...7',
    'lon2' = 'lon...8',
    'lat2' = 'lat...9',
    'plant_cv2' = 'plant_cv...10',
    'symptoms2' = 'symptoms...11',
    'source2' = 'source...12',
  )

bcv_1 <- bad_cv %>%
  select(id, lon, lat, plant_cv, symptoms, source)
bcv_2 <- bad_cv %>%
  select(id2, lon2, lat2, plant_cv2, symptoms2, source2) %>%
  rename(
    id = id2,
    lon = lon2,
    lat = lat2,
    plant_cv = plant_cv2,
    symptoms = symptoms2,
    source = source2
  )

bd_cit <- tmp_cit %>%
  filter(tmp_cit$plant_cv...4 != tmp_cit$plant_cv...10) %>%
  rename(
    'id' = 'id...1',
    'lon' = 'lon...2',
    'lat' = 'lat...3',
    'plant_cv' = 'plant_cv...4',
    'symptoms' = 'symptoms...5',
    'source' = 'source...6',
    'id2' = 'id...7',
    'lon2' = 'lon...8',
    'lat2' = 'lat...9',
    'plant_cv2' = 'plant_cv...10',
    'symptoms2' = 'symptoms...11',
    'source2' = 'source...12',
  )

#we will just keep this first one, the GPS failed to collect all the GPS data from the second visit.
bcit_1 <- bd_cit %>%
  select(id, lon, lat, plant_cv, symptoms, source)
#this method doesn't really work, trees aren't large patches in a landscape, I can't generalize
bcit_2 <- bd_cit %>%
  select(id2, lon2, lat2, plant_cv2, symptoms2, source2) %>%
  rename(
    id = id2,
    lon = lon2,
    lat = lat2,
    plant_cv = plant_cv2,
    symptoms = symptoms2,
    source = source2
  )


#recombining the dataset
# bad_cv <- bind_rows(bcv_1, bcv_2)

cit <- bcit_1
cit$plant_cv <- 'citrus_cv' #this will save me some pain later

#combining rows
t1 <- bind_rows(gcv_1, bcv_1)
t2 <- bind_rows(gcv_2, bcv_2)


#assigning colors to symptoms, assigning factors
df$disease_status <- df$symptoms
df$disease_status <- gsub('stippling', '#A3B7F9', df$disease_status)
df$disease_status <- gsub('ringspot', '#092C48', df$disease_status)
df$disease_status <- gsub('chlorosis', '#C59434', df$disease_status)
df$disease_status <- gsub('none', '#EDE6DE', df$disease_status)
df$labs <- df$plant_cv
df$labs <- gsub('liriope_gigantea', 'Lg', df$labs)
df$labs <- gsub('ophiopogon_japonicus', 'Oj', df$labs)
df$labs <- gsub('aspidistra_elatior', 'Ae', df$labs)
df$labs <- gsub('citrus_x_paradisi', 'C', df$labs)
df$labs <- gsub('citrus_x_sinensis', 'C', df$labs)
df$labs <- gsub('citrus_cv', 'C', df$labs)
df$symptoms <- as_factor(df$symptoms)
df$plant_cv <- as_factor(df$plant_cv)
df$labs <- as_factor(df$labs)
df <- drop_na(df) #this will save me some pain here

#same for t1
t1$disease_status <- t1$symptoms
t1$disease_status <- gsub('stippling', '#A3B7F9', t1$disease_status)
t1$disease_status <- gsub('ringspot', '#092C48', t1$disease_status)
t1$disease_status <- gsub('chlorosis', '#C59434', t1$disease_status)
t1$disease_status <- gsub('none', '#EDE6DE', t1$disease_status)
t1$labs <- t1$plant_cv
t1$labs <- gsub('liriope_gigantea', 'Lg', t1$labs)
t1$labs <- gsub('ophiopogon_japonicus', 'Oj', t1$labs)
t1$labs <- gsub('aspidistra_elatior', 'Ae', t1$labs)
t1$labs <- gsub('citrus_x_paradisi', 'C', t1$labs)
t1$labs <- gsub('citrus_x_sinensis', 'C', t1$labs)
t1$labs <- gsub('citrus_cv', 'C', t1$labs)
t1$symptoms <- as_factor(t1$symptoms)
t1$plant_cv <- as_factor(t1$plant_cv)
t1$labs <- as_factor(t1$labs)

#ditto for t2
t2$disease_status <- t2$symptoms
t2$disease_status <- gsub('stippling', '#A3B7F9', t2$disease_status)
t2$disease_status <- gsub('ringspot', '#092C48', t2$disease_status)
t2$disease_status <- gsub('chlorosis', '#C59434', t2$disease_status)
t2$disease_status <- gsub('none', '#EDE6DE', t2$disease_status)
t2$labs <- t2$plant_cv
t2$labs <- gsub('liriope_gigantea', 'Lg', t2$labs)
t2$labs <- gsub('ophiopogon_japonicus', 'Oj', t2$labs)
t2$labs <- gsub('aspidistra_elatior', 'Ae', t2$labs)
t2$labs <- gsub('citrus_x_paradisi', 'C', t2$labs)
t2$labs <- gsub('citrus_x_sinensis', 'C', t2$labs)
t2$labs <- gsub('citrus_cv', 'C', t2$labs)
t2$symptoms <- as_factor(t2$symptoms)
t2$plant_cv <- as_factor(t2$plant_cv)
t2$labs <- as_factor(t2$labs)

#citrus tags along
cit$disease_status <- cit$symptoms
cit$disease_status <-
  gsub('stippling', '#A3B7F9', cit$disease_status)
cit$disease_status <-
  gsub('ringspot', '#092C48', cit$disease_status)
cit$disease_status <-
  gsub('chlorosis', '#C59434', cit$disease_status)
cit$disease_status <- gsub('none', '#EDE6DE', cit$disease_status)
cit$labs <- cit$plant_cv
cit$labs <- gsub('citrus_cv', 'C', cit$labs)
cit$symptoms <- as_factor(cit$symptoms)
cit$plant_cv <- as_factor(cit$plant_cv)

#subsetting data for different plant types
lir_t1 <- t1 %>% filter(plant_cv != 'aspidistra_elatior')
lir_t2 <- t2 %>% filter(plant_cv != 'aspidistra_elatior')


aspd_t1 <- t1 %>% filter(plant_cv == 'aspidistra_elatior')
aspd_t1 <- bind_rows(aspd_t1, cit) #add citrus back in

aspd_t2 <- t2 %>% filter(plant_cv == 'aspidistra_elatior')
aspd_t2 <- bind_rows(aspd_t2, cit) #add citrus back in

#making a spatial point object to calculate distances
lon_lat <-
  SpatialPoints(coords = cbind(df$lon, df$lat),
                proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs')))
lon_lat_lir_t1 <-
  SpatialPoints(coords = cbind(lir_t1$lon, lir_t1$lat),
                proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs')))
lon_lat_lir_t2 <-
  SpatialPoints(coords = cbind(lir_t2$lon, lir_t2$lat),
                proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs')))
lon_lat_aspd_t1 <-
  SpatialPoints(
    coords = cbind(aspd_t1$lon, aspd_t1$lat),
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )
lon_lat_aspd_t2 <-
  SpatialPoints(
    coords = cbind(aspd_t2$lon, aspd_t2$lat),
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )
lon_lat_cit <-
  SpatialPoints(coords = cbind(cit$lon, cit$lat),
                proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs')))

tmp <-
  data_frame(
    select(
      df,
      'sample_no',
      'date',
      'symptoms',
      'disease_status',
      'plant_cv',
      'labs',
      'family',
      'order'
    )
  )
tmp_lir_t1 <-
  data_frame(select(
    lir_t1,
    'id',
    'plant_cv',
    'labs',
    'symptoms',
    'disease_status',
    'source'
  ))
tmp_lir_t2 <-
  data_frame(select(
    lir_t2,
    'id',
    'plant_cv',
    'labs',
    'symptoms',
    'disease_status',
    'source'
  ))
tmp_aspd_t1 <-
  data_frame(select(
    aspd_t1,
    'id',
    'plant_cv',
    'labs',
    'symptoms',
    'disease_status',
    'source'
  ))
tmp_aspd_t2 <-
  data_frame(select(
    aspd_t2,
    'id',
    'plant_cv',
    'labs',
    'symptoms',
    'disease_status',
    'source'
  ))
tmp_cit <-
  data_frame(select(
    cit,
    'id',
    'plant_cv',
    'labs',
    'symptoms',
    'disease_status',
    'source'
  ))

tot <-
  SpatialPointsDataFrame(
    coords = cbind(df$lon, df$lat),
    data = tmp,
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )

lir_1 <-
  SpatialPointsDataFrame(
    coords = cbind(lir_t1$lon, lir_t1$lat),
    data = tmp_lir_t1,
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )

lir_2 <-
  SpatialPointsDataFrame(
    coords = cbind(lir_t2$lon, lir_t2$lat),
    data = tmp_lir_t2,
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )

aspd_1 <-
  SpatialPointsDataFrame(
    coords = cbind(aspd_t1$lon, aspd_t1$lat),
    data = tmp_aspd_t1,
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )
aspd_2 <-
  SpatialPointsDataFrame(
    coords = cbind(aspd_t2$lon, aspd_t2$lat),
    data = tmp_aspd_t2,
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )
cit <-
  SpatialPointsDataFrame(
    coords = cbind(cit$lon, cit$lat),
    data = tmp_cit,
    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs'))
  )

#This makes a distance matrix?
distmtx_tot <- distm(tot, fun = distGeo)
distmtx_lir_1 <- distm(lir_1, fun = distGeo)
distmtx_lir_2 <- distm(lir_2, fun = distGeo)
distmtx_aspd_1 <- distm(aspd_1, fun = distGeo)
distmtx_aspd_2 <- distm(aspd_2, fun = distGeo)
distmtx_cit <- distm(cit, fun = distGeo)

# inverse power-law function
beta <- 2 # positive number
dmexp_tot <- distmtx_tot ^ (-beta)
dmexp_lir_1 <- distmtx_lir_1 ^ (-beta)
dmexp_lir_2 <- distmtx_lir_2 ^ (-beta)
dmexp_aspd_1 <- distmtx_aspd_1 ^ (-beta)
dmexp_aspd_2 <- distmtx_aspd_2 ^ (-beta)
dmexp_cit <- distmtx_cit ^ (-beta)

#cropdistancematr <- dmexp * cropmatrix # adjacency matrix
cropdmtx_tot <-
  graph_from_adjacency_matrix(
    dmexp_tot,
    mode = c("directed"),
    diag = TRUE,
    weighted = TRUE
  )
cropdmtx_lir_1 <-
  graph_from_adjacency_matrix(
    dmexp_lir_1,
    mode = c("directed"),
    diag = TRUE,
    weighted = TRUE
  )
cropdmtx_lir_2 <-
  graph_from_adjacency_matrix(
    dmexp_lir_2,
    mode = c("directed"),
    diag = TRUE,
    weighted = TRUE
  )
cropdmtx_aspd_1 <-
  graph_from_adjacency_matrix(
    dmexp_aspd_1,
    mode = c("directed"),
    diag = TRUE,
    weighted = TRUE
  )
cropdmtx_aspd_2 <-
  graph_from_adjacency_matrix(
    dmexp_aspd_2,
    mode = c("directed"),
    diag = TRUE,
    weighted = TRUE
  )

cropdmtx_cit <-
  graph_from_adjacency_matrix(
    dmexp_cit,
    mode = c("directed"),
    diag = TRUE,
    weighted = TRUE
  )

#combining igraphs
cropdmtx_lir <- union(cropdmtx_lir_1, cropdmtx_lir_2)

cropdmtx_aspd <- union(cropdmtx_aspd_1, cropdmtx_aspd_2)

#giving weight to links
lnk_wt_tot <- E(cropdmtx_tot)$weight
lnk_wt_lir_1 <- E(cropdmtx_lir_1)$weight
lnk_wt_lir_2 <- E(cropdmtx_lir_2)$weight
lnk_wt_aspd_1 <- E(cropdmtx_aspd_1)$weight
lnk_wt_aspd_2 <- E(cropdmtx_aspd_2)$weight
lnk_wt_cit <- E(cropdmtx_cit)$weight

# lnk_wt_tot <- lnk_wt_tot * 5

#plotting
plot(
  cropdmtx_tot,
  vertex.label = tot$labs,
  vertex.color = tot$disease_status
)

plot(
  cropdmtx_lir_1,
  vertex.label = lir_1$labs,
  vertex.color = lir_1$disease_status
)

plot(
  cropdmtx_lir_2,
  vertex.label = lir_2$labs,
  vertex.color = lir_2$disease_status
)

plot(
  cropdmtx_lir,
  vertex.label = lir_2$labs,
  vertex.color = lir_2$disease_status
)

modulos <- cluster_spinglass(cropdmtx_lir)
str(modulos)

plot(
  cropdmtx_aspd_1,
  vertex.label = aspd_1$labs,
  vertex.color = aspd_1$disease_status
)

plot(
  cropdmtx_aspd_2,
  vertex.label = aspd_2$labs,
  vertex.color = aspd_2$disease_status
)

plot(
  cropdmtx_aspd,
  vertex.label = aspd_2$labs,
  vertex.color = aspd_2$disease_status
)

plot(
  cropdmtx_cit,
  vertex.label = cit$labs,
  vertex.color = cit$disease_status
)

V(cropdmtx_cit)
edge_attr(cropdmtx_cit)
deg <- degree(cropdmtx_cit)
clo <- closeness(cropdmtx_cit)
bet <- betweenness(cropdmtx_cit)
eig <- evcent(cropdmtx_cit)$vector
name <- get.vertex.attribute(cropdmtx_cit, "name")
itable <- cbind(name, deg, clo, bet, eig)
itable[1, ]
hist(degree.distribution(cropdmtx_cit))

object.name <- cbind(V(cropdmtx_cit)$id, deg, clo, bet, eig)
