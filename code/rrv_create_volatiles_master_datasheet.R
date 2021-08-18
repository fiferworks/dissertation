####LOADING PACKAGES AND READING IN FILE LISTS####
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

# reading in the SPME GC/MS data channel "TIC"
ticfiles <-
  list.files(
    path = "./data/rrd_spme/TIC",
    pattern = "*_Integration.txt",
    ignore.case = T,
    recursive = T,
    full.names = T
  )

# reading in the SPME GC/MS data channel mass calls "MS_CALL"
msfiles <-
  list.files(
    path = "./data/rrd_spme/MS_CALL",
    pattern = "*_Integration.txt",
    ignore.case = T,
    recursive = T,
    full.names = T
  )

# reading in the QSEP GC/MS data channel mass calls "MS_CALL"
qsfiles <-
  list.files(
    path = "./data/rrd_qsep/TIC",
    pattern = "*_Integration.txt",
    ignore.case = T,
    recursive = T,
    full.names = T
  )


#combine the lists
gcfiles <- c(ticfiles, msfiles, qsfiles)

####IMPORTING DATASHEETS AND CREATING COMBINED TABLE####
# Using the list we created, we create temporary files, tidy them up a little, and combine them into a single dataframe (tibble)
# The warnings are about an unnamed column, R automatically gives it a placeholder name0
# R will do the following actions for all files listed in "gcfiles"
# R checks if we have the temp file "dummy", if not, R creates one
for (file in gcfiles) {
  if (!exists("gc_data")) {
    tbl_colnames <-
      c(
        'Path',
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
    
    # removes the totals on the last column
    gc_data_temp <- gc_data_temp %>% filter(row_number() <= n() - 1)
    
    # removes two rows which were incorrectly parsed and duplicated
    gc_data_temp <- gc_data_temp %>% slice(-(1:2))
    
    # dropping unnecessary columns
    gc_data_temp <- select(gc_data_temp,-'X9',-'Amount')
    
    # grabs the file name of the file being processed
    path_name <- file
    
    # adding column for the source file
    gc_data_temp <-
      gc_data_temp %>% add_column(Path = path_name)
    
    # appends gc_data_temp to our gcdata file
    gc_data <- bind_rows(gc_data, gc_data_temp)
  }
}


####CREATING MORE DATA COLUMNS AND CLEANING DATA####
# renaming columns
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
    Path,
    'Peak No.',
    'Peak Name',
    'Retention Time (min)',
    'Area (counts*min)',
    'Height (counts)',
    'Relative Area (%)',
    'Relative Height (%)'
  )


# Duplicating columns which will describe the type of sample each trial is
gc_data$`Injection Method` <- gc_data$Path
gc_data$`Injection Method` <-
  str_sub(gc_data$`Injection Method`, start = 8)
gc_data$`Injection Method` <-
  str_sub(gc_data$`Injection Method`, end = 8)

gc_data$`Injection Type` <- gc_data$Path

# removes the special symbols from the beginning of the path before appending
gc_data$`Injection Type` <-
  str_sub(gc_data$`Injection Type`, start = 17)
gc_data$`Injection Type` <-
  str_sub(gc_data$`Injection Type`, end = -17)

# making more columns from the cleaned path names
gc_data$Sample <- gc_data$`Injection Type`
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
    "field",
    "greenhouse",
    "test",
    "is"
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
    "rrv",
    "test",
    "is"
  )
x  = gc_data$Location
x  =  tm::removeWords(x, stopwords)
gc_data$Location <- x

# cleaning strings for Location column
gc_data$Location <- tm::removePunctuation(gc_data$Location)
gc_data$Location <- tm::removeNumbers(gc_data$Location)
gc_data$Location <-
  gsub(" blank     field", "field", gc_data$Location)
gc_data$Location <-
  gsub(" rose   greenhouse ", "greenhouse ", gc_data$Location)
gc_data$Location <-
  gsub(" rose  greenhouse ", "greenhouse ", gc_data$Location)
gc_data$Location <- str_trim(gc_data$Location)
gc_data$Location <- gsub("rose", "field", gc_data$Location)

# list of words to remove for cleaning Sample column
stopwords <-
  c("MS_CALL/",
    "TIC/")
x  = gc_data$Sample
x  =  tm::removeWords(x, stopwords)
gc_data$Sample <- x

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
gc_data$Treatment <-
  sub("clean_greenhouse*", "untreated", gc_data$Treatment)
gc_data$Treatment <-
  sub("clean_test*", "untreated", gc_data$Treatment)
gc_data$Treatment <-
  sub("clean_no*", "untreated", gc_data$Treatment)

# removing underscores
gc_data$Treatment <- gsub("_", " ", gc_data$Treatment)

# removing other parts
gc_data$Treatment <- tm::removePunctuation(gc_data$Treatment)
gc_data$Treatment <- tm::removeNumbers(gc_data$Treatment)

# list of words to remove for cleaning
stopwords <-
  c("site", "no", "is", "greenhouse")
x  = gc_data$Treatment
x  =  tm::removeWords(x, stopwords)
gc_data$Treatment <- x

# cleaning up whitespace
gc_data$Treatment <- str_trim(gc_data$Treatment)

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

# turns empty cells into NAs
gc_data$`Peak Name` <-
  ifelse(as.character(gc_data$`Peak Name`) != "", gc_data$`Peak Name`, NA)


####MAKE COLUMN THAT DIVIDES AREA OF EACH CHEMICAL BY THE INTERNAL STANDARD ####
# tibble of internal standards
IntStds <- gc_data %>%
  group_by(Sample) %>%
  filter(`Peak Name` == 'Nonyl Acetate')

# works to remove samples without internal standards
IntStds <- IntStds %>%  filter(`Peak No.` > 0)

# tibble without internal standards
Peaks <- gc_data %>%
  group_by(Sample)

#list of unique samples
sample_list <- unique(IntStds$Sample)

# making a dataframe for the following loop
tbl_colnames <-
  c(
    'Sample',
    'Treatment',
    'Peak No.',
    'Peak Name',
    'Retention Time (min)',
    'IS Relative Area (%)',
    'Area (counts*min)',
    'Height (counts)',
    'Relative Area (%)',
    'Relative Height (%)',
    'Injection Method',
    'Injection Type',
    'Channel',
    'Location',
    'Path'
  )
df <-
  read_csv("\n",
           col_names = tbl_colnames,
           col_types = "ccdcddddddccccc")

# loop to calculate area relative to the internal standard of each sample
for (i in 1:length(sample_list)) {
  if (exists("df")) {
    filtered_peaks <- Peaks[Peaks$Sample == sample_list[i], ]
    filtered_is <- IntStds[IntStds$Sample == sample_list[i], ]
    tmp <-
      mutate(
        filtered_peaks,
        `IS Relative Area (%)` =  filtered_peaks$`Area (counts*min)` / filtered_is$`Area (counts*min)`
      )
    df <- bind_rows(df, tmp)
  }
}

# saving master datasheet as excel spreadsheet
write_xlsx(df, "data/rrv_volatiles_master_datasheet.xlsx")

# cleanup
rm(list = ls())