# names_load.R

library(tidyverse)

names=readr::read_csv("data-raw/names.csv")

usethis::use_data(names, overwrite = TRUE)

