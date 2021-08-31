#loading required packages
pkgs <-
  c("tidyverse",
    "lubridate",
    "readxl",
    "writexl",
    "stringr",
    "mlr",
    "mice")

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####Importing GPS data####
gps <- read_csv("data/rrv_pheno_gps_from_photos.csv")

#dropping unnecessary exif data
gps <- dplyr::select(gps, -"GPSAltitude")

#sorting the columns by date
gps$date <- lubridate::as_date(gps$DateTimeOriginal)
gps <- dplyr::arrange(gps, date)

#making the id column based off of the filename
gps$id <- gsub("[^0-9.-]", "", gps$SourceFile)

#loop to remove the extra numbers in the dates
for (i in 1:6) {
  gps$id <- gsub("(.*)-.*", "\\1", gps$id)
}

#removes the last dot
gps$id <- sub(".", "", gps$id)

#adds in the 'Pheno' label
gps$id <- paste('Pheno', gps$id, sep = ' ')

#renaming columns
gps <-
  rename(gps,
         lat = "GPSLongitude",
         lon = "GPSLatitude",
         lon_lat = 'GPSPosition')

gps <- gps %>% distinct(date, id, .keep_all = TRUE)

avgs <-
  gps %>% group_by(id) %>% summarize(lat = mean(lat, na.rm = TRUE),
                                     lon = mean(lon, na.rm = TRUE))
avgs$lon_lat <- paste(avgs$lon, avgs$lat)

gps <- gps %>% dplyr::select(-lat, -lon, -lon_lat)

gps <- left_join(gps, avgs)

####Reading in the RRV Survey Master Datasheet####
df <- read_excel('data/rrv_pheno_jessie_datasheet.xlsx')

#making id a factor for sorting
df$id <- as_factor(df$id)
df <- df %>% arrange(date, id)
gps$id <- as_factor(gps$id)
gps <- gps %>% arrange(id)

# reading in the mite ipm trials
ipm <- read_excel('data/rrv_ipm_trial_2020-2021.xlsx')

ipm$id <- as_factor(ipm$id)
ipm <- ipm %>% arrange(date, id)

df <- df %>% add_column(Study = 'Phenology')
ipm <- ipm %>% add_column(Study = 'IPM')

# combining phenology dataset with the ipm treatments (they are at the same sites)
df <- bind_rows(df, ipm)

# arranging by date
df <- df %>% arrange(date, id)


# filling in missing information for some rows
df$rose_spp <- 'KO'
df$city <- 'Tallahassee'
df$state <- 'FL'
df$county <- 'Leon'
df$collector <- "Austin Fife"
df$shade <- "open sun"
df$p_fructiphilus <- "verified"
df$symptoms <- "No"

# reset the sample_column
df$sample_no <- 1:length(df$sample_no)

gps <-
  gps %>% dplyr::select(id, lat, lon , lon_lat) %>% distinct(id, .keep_all = TRUE)

df <- left_join(df, gps)

# figuring out how much data is missing from dry weights
# percentMissed <- function(x){sum(is.na(x))/length(x)*100}
# apply(df, 2, percentMissed)
# apply(df, 1, percentMissed)


#### only one week of the dry weights are lost, imputing missing weights ####
missing_weights <-
  df %>% dplyr::select(id, grams_dry_weight, other_mites, eriophyoids)

imputeMethod <- imputeLearner("regr.rpart")
gramImp <-
  impute(as.data.frame(missing_weights),
         classes = list(numeric = imputeMethod))

tempData <-
  mice(
    missing_weights,
    m = 5,
    maxit = 50,
    meth = 'pmm',
    seed = 500,
    data.init = gramImp$data
  )

tempData$imp$grams_dry_weight

missing_weights <- as_tibble(complete(tempData, 1))

#### adding a column for months (easier for nice graphs later) ####
df$grams_dry_weight[1:24] <- missing_weights$grams_dry_weight[1:24]
df$grams_dry_weight[124:172] <-
  missing_weights$grams_dry_weight[124:172]

df <-
  df %>% add_column(month = month(df$date, label = TRUE, abbr = FALSE),
                    .before = "other_mites")

# adding a column for mites/gram
df <-
  df %>% mutate(
    'mites/g' = (eriophyoids + other_mites) / grams_dry_weight,
    'erios/g' = eriophyoids / grams_dry_weight,
    .before = p_fructiphilus
  )

####Saving Master Datasheet as Excel Spreadsheet####
write_csv(df, "data/rrv_pheno_clean_datasheet.csv")

#cleanup
rm(list = ls())
