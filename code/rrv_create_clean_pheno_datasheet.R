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
df <- read_csv("data/rrv_pheno_gps_from_photos.csv")

#dropping unnecessary exif data
df <- select(df,-"GPSAltitude")

#sorting the columns by date
df$date <- lubridate::as_date(df$DateTimeOriginal)
df <- dplyr::arrange(df, date)

#making the id column based off of the filename
df$id <- gsub("[^0-9.-]", "", df$SourceFile)

#loop to remove the extra numbers in the dates
for (i in 1:6) {
  df$id <- gsub("(.*)-.*", "\\1", df$id)
}

#removes the last dot
df$id <- sub(".", "", df$id)

#adds in the 'Pheno' label
df$id <- paste('Pheno', df$id, sep = ' ')

#renaming columns
df <-
  rename(df,
         lat = "GPSLongitude",
         lon = "GPSLatitude",
         lon_lat = 'GPSPosition')

df <- df %>% distinct(date, id, .keep_all = TRUE)

####Reading in the Master Datasheet####
pf <- read_excel('data/rrv_pheno_master_datasheet.xlsx')

#removing the GPS data from the master dataset, which is simply copy-pasted from a single reading
pf <- select(pf,-c('lon', 'lat', 'lon_lat'))

#combining GPS data with phenology data
df <- left_join(pf, df)

#making id a factor for sorting
df$id <- as_factor(df$id)
df %>% arrange(date, id)

# figuring out how much data is missing from dry weights
# percentMissed <- function(x){sum(is.na(x))/length(x)*100}
# apply(df, 2, percentMissed)
# apply(df, 1, percentMissed)


# only 12.6% of the dry weights are lost, imputing missing weights
w <-
  df %>% select(id, grams_dry_weight, other_mites, eriophyoids)

imputeMethod <- imputeLearner("regr.rpart")
gramImp <-
  impute(as.data.frame(w), classes = list(numeric = imputeMethod))

tempData <-
  mice(
    w,
    m = 5,
    maxit = 500,
    meth = 'pmm',
    seed = 500,
    data.init = gramImp$data
  )

tempData$imp$grams_dry_weight

w <- as_tibble(complete(tempData, 1))

df$grams_dry_weight[1:24] <- w$grams_dry_weight[1:24]

####Saving Master Datasheet as Excel Spreadsheet####
write_csv(df, "data/rrv_pheno_clean_datasheet.csv")

#cleanup
rm(list = ls())
