####PACKAGES####
pkgs <- c('tidyverse', 'readxl', 'writexl')
lapply(pkgs, library, character.only = T)
rm(list = ls())

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)

####GRIFFIN DATA####
#reading in file, data from IPM 2019 Georgia Trials
q1 <-
  read_excel(
    'jessie_ipm_datasheet.xlsx',
    sheet = 1,
    col_types = c('guess',
                  'guess',
                  'guess',
                  'date',
                  'guess',
                  'guess')
  )


#splitting plant and block into different columns
q1 <-
  separate(q1, `Block, Plant, Treat`, c('Block', 'Plant'), sep = 1)

#splitting out treatments as well
q1 <- separate(q1, Plant, c('Plant', 'Treatment'), sep = -1)

#renames treatments to more readable form
q1$Treatment[q1$Treatment == 'M'] <- "Mites"
q1$Treatment[q1$Treatment == 'W'] <- "Water"
q1$Treatment[q1$Treatment == 'N'] <- "Ninja"
q1$Treatment[q1$Treatment == 'K'] <- "Kontos"
q1$Treatment[q1$Treatment == 'A'] <- "Actigard"
q1$Treatment[q1$Treatment == '+'] <- "Mites + Ninja"

#renaming a column to 'Field'
q1 <- q1 %>% rename(Field = `Athens/Griffin`)

#adding combined id for plants
q1$ID <- paste(q1$Plant, q1$Block, sep = '')

#rearranging columns
q1 <-
  dplyr::select(q1,
                'ID',
                'Plant',
                'Block',
                'Treatment',
                'Eriophyoids',
                `Other Mites`,
                'Date',
                'Field')

#getting summary stats for each treatment group
q1 <-
  q1 %>% group_by(Treatment) %>% mutate(N = n()) %>% ungroup()

#saving the master file
write_xlsx(q1, 'ipm_master.xlsx')

#dropping unused columns
q1 <-
  dplyr::select(
    q1,
    'Plant',
    'Block',
    'Treatment',
    'Eriophyoids',
    'Other Mites',
    'Date',
    'Field',
    'ID',
    'N'
  )

#making things lowercase for ease of use when coding
q1 <- q1 %>% rename(
  'plant' = 'Plant',
  'block' = 'Block',
  'treat' = 'Treatment',
  'date' = 'Date',
  'erios' = 'Eriophyoids',
  'other' = `Other Mites`,
  'field' = 'Field',
  'id' = 'ID',
  'n' = 'N'
)

#forcing date to be read as a character to avoid errors
q1$date <- as.character(q1$date)

#making sure all variables are lowercase
q1$id <- tolower(q1$id)
q1$block <- tolower(q1$block)
q1$treat <- tolower(q1$treat)

#removing incomplete records
q1 <- q1 %>% drop_na

df <- q1

#rearranging columns
df <- dplyr::select(df,
                    'id',
                    'plant',
                    'block',
                    'treat',
                    'erios',
                    'other',
                    'field',
                    'n',
                    'date')

#getting summary stats for each treatment group for other mites
df <-
  df %>% group_by(treat) %>% mutate(
    per_plant = mean(other),
    totals = sum(other),
    sd = sd(other),
    se = sd(other) / sqrt(n()),
    n_samples = n()
  ) %>%
  ungroup()


#saving output
write_csv(df, 'ipm.csv')

#cleanup
rm(list = ls())