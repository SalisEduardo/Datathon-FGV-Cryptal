library(tidyverse)
library(readr)
library(lubridate)

x <- read_csv("df_p1_Litecoin.csv")


lubridate::ymd(x$date)

x$date <- as.POSIXct(x$date/1000, origin = "1970-01-01") 

x %>%  dplyr::arrange(date) %>%  tail()