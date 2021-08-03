# loading required packages
pkgs <-
  c("tidyverse",
    "lubridate",
    "tm",
    "stringr",
    "readxl",
    "writexl")

# installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

# loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

# reading in the GC/MS data channel "TIC"
ticfiles <-
  list.files(
    path = "./data/rrd_spme/TIC",
    pattern = "*_Integration.txt",
    ignore.case = T,
    recursive = T,
    full.names = T
  )

# reading in the GC/MS data channel mass calls "MS_CALL"
msfiles <-
  list.files(
    path = "./data/rrd_spme/MS_CALL",
    pattern = "*_Integration.txt",
    ignore.case = T,
    recursive = T,
    full.names = T
  )

#combine the lists
gcfiles <- c(ticfiles, msfiles)

#Using the list we created, we create temporary files, tidy them up a little, and combine them into a single dataframe (tibble)

#R will do the following actions for all files listed in "ticfiles"
#R checks if we have the temp file "dummy", if not, R creates one
for (file in gcfiles) {
  if (!exists("gc_data")) {
    tbl_colnames <-
      c(
        'Sample',
        'No.',
        'Peak Name',
        'Retention Time',
        'Area',
        'Height',
        'Relative Area',
        'Relative Height'
      )
    gc_data <- read_csv("\n", col_names = tbl_colnames)
  }
  # if R finds the file "gc_data", it reads each file in the directory, and saves it as a temporary file "gc_data_temp"
  if (exists("gc_data")) {
    gc_data_temp <- read_tsv(file, skip = 39, na = c('n.a.', "NA"))
    
    # reads off the name of the file
    print(file)
    
    # filters out undetected peaks
    gc_data_temp <- gc_data_temp %>% filter(gc_data_temp$No. > 0)
    
    # removes the totals on the last column
    gc_data_temp <- gc_data_temp %>% filter(row_number() <= n() - 1)
    
    # removes two rows which were incorrectly parsed and duplicated
    gc_data_temp <- gc_data_temp %>% slice(-(1:1))
    
    # dropping unnecessary columns
    gc_data_temp <- select(gc_data_temp,-'X9',-'Amount')
    
    # grabs the file name of the file being processed
    file_name <- file
    
    #removes the special symbols in the beginning
    file_name <- str_sub(file_name, start = 17)
    file_name <- str_sub(file_name, end = -17)
    
    # adding column for the source file
    gc_data_temp <- gc_data_temp %>% add_column(Sample = file_name)
    # appends gc_data_temp to our gcdata file
    gc_data <- bind_rows(gc_data, gc_data_temp)
  }
}

#renaming columns
gc_data <-
  gc_data %>% rename(
    'Peak No.' = 'No.',
    'Retention Time (min)' = 'Retention Time',
    'Area (counts*min)' = Area,
    'Height (counts)' = Height,
    'Relative Area (%)' = 'Relative Area',
    'Relative Height (%)' = 'Relative Height'
  )

gc_data <-
  gc_data %>% select(
    Sample,
    'Peak No.',
    'Peak Name',
    'Retention Time (min)',
    'Area (counts*min)',
    'Height (counts)',
    'Relative Area (%)',
    'Relative Height (%)'
  )

# Duplicating columns which will describe the type of sample each trial is
gc_data$`Injection Type` <- gc_data$Sample
gc_data$Treatment <- gc_data$`Injection Type`

# replacing underscores and slashes with spaces
gc_data$`Injection Type` <- gsub("_", " ", gc_data$`Injection Type`)
gc_data$`Injection Type` <-
  gsub("\\/", " ", gc_data$`Injection Type`)

# creating a column for Channel type and Location, cutting off the extra words and cleaning whitespace
gc_data$Channel <- gc_data$`Injection Type`
gc_data$Location <- gc_data$`Injection Type`
gc_data$Channel <- str_sub(gc_data$Channel, end = 3L)
gc_data$Channel <- str_trim(gc_data$Channel)

# list of words to remove for cleaning Injection Type column
stopwords <-
  c(
    "site",
    "1ul",
    "nonyl",
    "acetate",
    "fiber",
    "no",
    "bag",
    "MS",
    "CALL",
    "TIC",
    "clean",
    "actigard",
    "rrv",
    "field"
  )
x  = gc_data$`Injection Type`
x  =  tm::removeWords(x, stopwords)
gc_data$`Injection Type` <- x

# removing punctuation, digits and whitespace
gc_data$`Injection Type` <-
  tm::removePunctuation(gc_data$`Injection Type`)
gc_data$`Injection Type` <-
  tm::removeNumbers(gc_data$`Injection Type`)
gc_data$`Injection Type` <- str_trim(gc_data$`Injection Type`)


# list of words to remove for cleaning Location column
stopwords <-
  c(
    "site",
    "1ul",
    "nonyl",
    "acetate",
    "fiber",
    "no",
    "bag",
    "MS",
    "CALL",
    "TIC",
    "clean",
    "actigard",
    "rrv"
  )
x  = gc_data$Location
x  =  tm::removeWords(x, stopwords)
gc_data$Location <- x

# cleaning strings for Location column
gc_data$Location <- tm::removePunctuation(gc_data$Location)
gc_data$Location <- tm::removeNumbers(gc_data$Location)
gc_data$Location <-
  gsub(" blank     field", "field", gc_data$Location)
gc_data$Location <- str_trim(gc_data$Location)
gc_data$Location <- gsub("rose", "field", gc_data$Location)


# Assigns blank treatments to NA
gc_data$Treatment[gc_data$`Injection Type` == "blank"] <- NA

#removing folder names
gc_data$Treatment <- sub("TIC\\/rose_", "", gc_data$Treatment)
gc_data$Treatment <- sub("MS_CALL\\/rose_", "", gc_data$Treatment)

# cleaning
gc_data$Treatment <-
  sub("1ul_nonyl_acetate", "untreated", gc_data$Treatment)
gc_data$Treatment <-
  sub("clean_site*", "untreated", gc_data$Treatment)
gc_data$Treatment <-
  sub("clean_actigard*", "actigard", gc_data$Treatment)
gc_data$Treatment <- sub("rrv_site*", "rrv", gc_data$Treatment)

# removing underscores
gc_data$Treatment <- gsub("_", " ", gc_data$Treatment)

# removing other parts
gc_data$Treatment <- tm::removePunctuation(gc_data$Treatment)
gc_data$Treatment <- tm::removeNumbers(gc_data$Treatment)

# list of words to remove for cleaning
stopwords <-
  c("site")
x  = gc_data$Treatment
x  =  tm::removeWords(x, stopwords)
gc_data$Treatment <- x

# cleaning up whitespace
gc_data$Treatment <- str_trim(gc_data$Treatment)

gc_data$Treatment <- sub("rose", "untreated", gc_data$Treatment)

# changing column types to numeric
cols.num <-
  c(
    "Peak No.",
    "Retention Time (min)",
    "Area (counts*min)",
    "Height (counts)",
    "Relative Area (%)",
    "Relative Height (%)"
  )
gc_data[cols.num] <- sapply(gc_data[cols.num], as.numeric)

# make all NA chems into true NAs
gc_data$`Peak Name` <- sub("NA .*$", NA, gc_data$`Peak Name`)

# helper function to turn empty cells into NAs: https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

## transform all columns
gc_data <- gc_data %>% mutate_each(funs(empty_as_na)) 

####MAKE COLUMN THAT DIVIDES AREA OF EACH CHEMICAL BY THE INTERNAL STANDARD!####
unique(gc_data$`Peak Name`)
head(gc_data)

# saving master datasheet as excel spreadsheet
write_xlsx(gc_data, "data/rrd_spme_master_datasheet.xlsx")

# cleanup
rm(list = ls())