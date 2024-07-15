# Data formating and cleaning for MA thesis MEG
# Dani Gargya
# June 24

# loading libraries ----
library(tidyverse)
library(ggplot2)
library(dplyr) 
library(haven) # to transform data from sav to csv
library(readr) # to transform data from sav to csv

# workflow ----
# data conversion Lisa
# import new data and formating (from webside)
# allocate groups (0,1,2)
# data cleaning
### exclude imcomplete data?
### exclude data with more than 25% missing (6 answers)
### exclude data that took less than 2 minutes
# data transformation
### scaling according to Lisa (0-3, instead of 1-4)
### inverse scales to reflect meaning



# data conversion Lisas data from sav to csv ----
### import and convert lisas data from sav to csv
#pre angell
lisa_data_angell_pre <- read_sav("data/data_collection/data_pauli/ANGELL_PRE_anonym.SAV")
write_csv(x=lisa_data_angell_pre, path="data/data_collection/data_pauli/angell_pre.csv")

#post angell
lisa_data_angell_post <- read_sav("data/data_collection/data_pauli/ANGELL_POST_anonym.SAV")
write_csv(x=lisa_data_angell_post, path="data/data_collection/data_pauli/angell_post.csv")

## not needed
# pre goethe
#lisa_data_goethe_pre <- read_sav("data/data_collection/data_pauli/GOETHE_PRE_anonym.SAV")
#write_csv(x=lisa_data_goethe_pre, path="data/data_collection/data_pauli/goethe_pre.csv")

# goethe post
#lisa_data_goethe_post <- read_sav("data/data_collection/data_pauli/GOETHE_POST_anonym.SAV")
#write_csv(x=lisa_data_goethe_post, path="data/data_collection/data_pauli/goethe_post.csv")





# import new data and formating ----
## copied from scoscie website: GNU R-SCript für Daten-Import
ds_file = file.choose()
# setwd("./")
#ds_file = "rdata_KRSumfrage_2024-06-07_10-21.csv"
options(encoding = "UTF-8")
ds = read.delim(
  file=ds_file, encoding="UTF-8", fileEncoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = NULL,
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","AT01_01","AT01_02","AT01_03",
    "AT01_04","B001_01","B001_02","B001_03","B001_04","B001_05","B001_06","B001_07",
    "B001_08","B001_09","B001_10","CS01_01","CS01_02","CS01_03","IN01_01","IN01_02",
    "IN01_03","PB01_01","PB01_02","SN01_01","SN01_02","SN01_03","SW01_01","SW01_02",
    "SW01_03","SW01_04","SW01_05","SW01_06","SW01_07","SW01_08","WD01","WD02",
    "WD02_01","WD02_02","WD02_03","TIME001","TIME002","TIME003","TIME004","TIME005",
    "TIME006","TIME007","TIME008","TIME009","TIME_SUM","MAILSENT","LASTDATA",
    "FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
    MODE="factor", STARTED="character", AT01_01="numeric", AT01_02="numeric",
    AT01_03="numeric", AT01_04="numeric", B001_01="numeric", B001_02="numeric",
    B001_03="numeric", B001_04="numeric", B001_05="numeric", B001_06="numeric",
    B001_07="numeric", B001_08="numeric", B001_09="numeric", B001_10="numeric",
    CS01_01="numeric", CS01_02="numeric", CS01_03="numeric", IN01_01="numeric",
    IN01_02="numeric", IN01_03="numeric", PB01_01="numeric", PB01_02="numeric",
    SN01_01="numeric", SN01_02="numeric", SN01_03="numeric", SW01_01="numeric",
    SW01_02="numeric", SW01_03="numeric", SW01_04="numeric", SW01_05="numeric",
    SW01_06="numeric", SW01_07="numeric", SW01_08="numeric", WD01="numeric",
    WD02="numeric", WD02_01="logical", WD02_02="logical", WD02_03="logical",
    TIME001="integer", TIME002="integer", TIME003="integer", TIME004="integer",
    TIME005="integer", TIME006="integer", TIME007="integer", TIME008="integer",
    TIME009="integer", TIME_SUM="integer", MAILSENT="character",
    LASTDATA="character", FINISHED="logical", Q_VIEWER="logical",
    LASTPAGE="numeric", MAXPAGE="numeric", MISSING="numeric", MISSREL="numeric",
    TIME_RSI="numeric"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

row.names(ds) = ds$CASE

rm(ds_file)

#ds = ds_tmp
#rm(ds_tmp)

### assign groups ----
ds <- ds %>%
  mutate(Group = ifelse(WD02_01, "group2", ifelse(WD02_03, "group0", "group1")))

# count occurences
value_counts <- ds %>%
  count(Group)  # Count occurrences of each value in col1


# exclude incomplete data ----
# Remove unnecessary columns with NA
ds$SERIAL <- NULL
ds$REF <- NULL
ds$MAILSENT <- NULL

# Function to count the number of NAs in a row
count_nas <- function(row) {
  sum(is.na(row))
}

# Apply the function to each row and create a new column 'count_nas'
ds$count_nas <- apply(ds, 1, count_nas)

# Filter the data frame to exclude rows with NAs
ds_filtered <- ds[ds$count_nas <= 1, ]


# exclude data with more than 25% missing (8 answers) ----
###### more than 8 times I don't know or does it change because more questions??!
# Function to count the number of -1s in a row
count_minus_ones <- function(row) {
  sum(row == -1, na.rm = TRUE)
}

ds_filtered$count_minus_ones <- apply(ds_filtered, 1, count_minus_ones)

# Filter the data frame to exclude rows with more than eight -1s
ds_filtered <- ds_filtered[ds_filtered$count_minus_ones <= 8, ]

# exclude data with less than  2.5 min/ 150sec! processing time (instead of 4)----
#ds_filtered <- ds_filtered[ds_filtered$TIME_SUM >= 240, ]
ds_filtered <- ds_filtered[ds_filtered$TIME_SUM >= 150, ]


# data transformation # scaling according to Lisa (0-3, instead of 1-4) ----
# Define a function to convert values
convert_values <- function(x) {
  x <- ifelse(x == 4, 3,
              ifelse(x == 3, 2,
                     ifelse(x == 2, 1,
                            ifelse(x == 1, 0,
                                   ifelse(x == -1, -100, 
                                          x)))))
  return(x)
}


# Apply the function to all columns of the data frame
ds_scaled <- lapply(ds_filtered, convert_values)
ds_scaled <- as.data.frame(ds_scaled)

# invert certain scales to reflect meaning (AT2, B3, B4, B8, B9, SW3, SW5) ----
# Define a function to inverse scales
inverse_scale <- function(x) {
  x <- ifelse(x == 3, 0,
              ifelse(x == 2, 1,
                     ifelse(x == 1, 2,
                            ifelse(x == 0, 3, x))))
  return(x)
}

# Define a function to inverse scales and add new columns
inverse_scale_and_add_columns <- function(df, columns) {
  for (col in columns) {
    new_col_name <- paste(col, "inverse", sep = "_")
    df[[new_col_name]] <- inverse_scale(df[[col]])
  }
  return(df)
}



# Apply the function to add extra columns with inverse scales
ds_scaled_in <- inverse_scale_and_add_columns(ds_scaled, c("AT01_02", "B001_03", "B001_04", "B001_08", "B001_09", "SW01_03", "SW01_05"))

# save formated dataset as csv (angell_mzp3) ----
write_csv(x=ds_scaled_in, path="data/data_collection/angell_mzp3.csv")
