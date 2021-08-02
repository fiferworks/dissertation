# checks for and installs missing packages used for this script
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(exifr))
  install.packages("exifr")

#reads inside of subfolders, and only .jpgs, be careful!
files <-
  list.files(
    pattern = "*.jpg",
    ignore.case = T,
    recursive = T,
    full.names = T
  )

df <-
  read_exif(
    files,
    tags = c(
      "DateTimeOriginal",
      "GPSLongitude",
      "GPSLatitude",
      "GPSAltitude",
      "GPSPosition"
    )
  )

#saving results
write_csv(df, "ofv_gps_from_photos.csv")

#cleanup
rm(list = ls())
