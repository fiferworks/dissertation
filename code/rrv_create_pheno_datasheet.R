#loading required packages
pkgs <-
  c("tidyverse",
    "lubridate",
    "readxl",
    "writexl",
    "stringr",
    "biogeo")
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
df <- select(df, -"GPSAltitude")

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
         lon_lat = 'GPSPosition'
         )

####Reading in the Master Datasheet####
pf <- read_excel('data/rrv_mite_survey_master.xlsx')

#fixing date for later join
pf$date <- lubridate::as_date(pf$date)

#removing the GPS data from the master dataset, which is simply copy-pasted from a single reading
pf <- select(pf, -c('lon', 'lat', 'lon_lat'))

#selecting the phenology trials
#str_detect with filter after creating a single string from 'pheno' collapsed by | (OR)
pheno <- c('Pheno')
pf <- pf %>%
  filter(str_detect(id, str_c(pheno, collapse = "|")))

#removing old FSU sites which we couldn't monitor due to COVID-19 pandemic:
pf <- pf[-c(1:9),]

#renaming 'RRV Pheno #' to 'Pheno #'
pf$id <- gsub('RRV Pheno', 'Pheno', pf$id)

#combining GPS data with phenology data
w <- inner_join(pf, df) #THIS IS CAUSING AN ISSUE WITH DUPLICATING OBSERVATIONS, I MIGHT HAVE DUPLICATE IMAGES?

#making id a factor for sorting
w$id <-as_factor(w$id)

View(w %>% arrange(date, id))


####Saving Master Datasheet as Excel Spreadsheet####
write_xlsx(df, "rrv_pheno_datasheet.xlsx")

#cleanup
rm(list = ls())