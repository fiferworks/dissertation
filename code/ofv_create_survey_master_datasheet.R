#loading required packages
pkgs <-
  c("tidyverse",
    "lubridate",
    "tm")

#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

df <- read_csv("./data/ofv_gps_from_photos.csv")

#dropping unnecessary exif data
df <-
  select(df, -"GPSAltitude")

#sorting the columns by date
df$date <- lubridate::as_datetime(df$DateTimeOriginal)
df <- dplyr::arrange(df, date)

#adding necessary columns
df <- df %>% add_column(sample_no = c(1:length(df$SourceFile)),
                        .before = T)

#more columns
df <- df %>% add_column(collector = c("Austin N. Fife"),
                        .after = T)

#renaming columns
df <-
  df %>% rename(
    source = SourceFile,
    lon = GPSLongitude,
    lat = GPSLatitude,
    position = GPSPosition
  )


#rearranging said columns
df <- select(df,
             "sample_no",
             "date",
             "lon",
             "lat",
             "position",
             "collector",
             "source")

####RECORDING CULTIVAR & SYMPTOMS####
#duplicating the file source, files are already sorted according to species group
df$plant_cv <- df$source

#removes the special symbols in the beginning
df$plant_cv <- str_sub(df$plant_cv, 3)

#duplicating for adding symptoms later
df$symptoms <- df$plant_cv

#removing everything after the first slash for cultivar
df$plant_cv <- sub("/.*", "", df$plant_cv)

#list of words to remove for cleaning
stopwords <-
  c("liriopogon", "aspidistra_elatior", "citrus", "other")
x  = df$symptoms
x  =  removeWords(x, stopwords)
df$symptoms <- x

df$symptoms <- str_sub(df$symptoms, 2)

df$symptoms <- sub("/.*", "", df$symptoms)

#adding columns for plant taxonomy, by duplicating the cultivar column
#and replacing names with the family name for the plant
df <- df %>%
  add_column(family = df$plant_cv) %>%
  mutate(family = ifelse(
    as.character(family == 'citrus'),
    'rutaceae',
    as.character(family)
  )) %>%
  mutate(family = ifelse(
    as.character(family == 'liriopogon' | family == 'aspidistra_elatior'),
    'nolinoideae',
    as.character(family)
  )) %>%
  mutate(family = ifelse(
    as.character(family == 'other'),
    'unknown',
    as.character(family)
  ))

#doing the same thing to create a column for order
df <- df %>%
  add_column(order = df$family) %>%
  mutate(order = ifelse(
    as.character(order == 'rutaceae'),
    'sapindales',
    as.character(order)
  )) %>%
  mutate(order = ifelse(
    as.character(order == 'nolinoideae'),
    'asparagales',
    as.character(order)
  ))

#saving master datasheet as excel spreadsheet
write_csv(df, "data/ofv_master_datasheet.csv")

#cleanup
rm(list = ls())