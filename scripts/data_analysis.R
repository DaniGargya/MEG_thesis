library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# create colour palette ----
display.brewer.pal(n = 8, name = 'Dark2')
brewer.pal(n = 8, name = "Dark2")
display.brewer.pal(n=4, name = "Set1")
brewer.pal(n=4, name = "Set1")

## convert lisas data from sav to csv ----
library(haven)
library(readr)

#pre angell
lisa_data_angell_pre <- read_sav("data/data_collection/data_pauli/ANGELL_PRE_anonym.SAV")
write_csv(x=lisa_data_angell_pre, path="data/data_collection/data_pauli/angell_pre.csv")

#post angell
lisa_data_angell_post <- read_sav("data/data_collection/data_pauli/ANGELL_POST_anonym.SAV")
write_csv(x=lisa_data_angell_post, path="data/data_collection/data_pauli/angell_post.csv")

# pre goethe
lisa_data_goethe_pre <- read_sav("data/data_collection/data_pauli/GOETHE_PRE_anonym.SAV")
write_csv(x=lisa_data_goethe_pre, path="data/data_collection/data_pauli/goethe_pre.csv")

# goethe post
lisa_data_goethe_post <- read_sav("data/data_collection/data_pauli/GOETHE_POST_anonym.SAV")
write_csv(x=lisa_data_goethe_post, path="data/data_collection/data_pauli/goethe_post.csv")
