# Data analysis for MA thesis MEG
# Dani Gargya
# June 24

# workflow ----
# loading all relevant datasets
# statistical analysis data
# making pretty graphs

# loading libraries ----
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(haven) # to transform data from sav to csv
library(readr) # to transform data from sav to csv


# importing relevant data ----
mzp1_clean <- read.csv("data/data_collection/data_pauli/angell_pre.csv")
mzp2_clean <- read.csv("data/data_collection/data_pauli/angell_post.csv")

#mzp3_clean <- read.csv("data/data_collection/rdata_KRSumfrage_2024-06-04_13-09.csv")


# checking data ----
str(ds)
head(ds)
tail(ds)
summary(ds)





# create colour palette ----
display.brewer.pal(n = 8, name = 'Dark2')
brewer.pal(n = 8, name = "Dark2")
display.brewer.pal(n=4, name = "Set1")
brewer.pal(n=4, name = "Set1")