# lpga_data.R

lpga_data=readr::read_csv(file = "data-raw/lpga_data.csv")

usethis::use_data(lpga_data, overwrite = TRUE)
