### This function reads met data from each town, aggrigates it, then returns a list
###
### as written it reads "Daymet_max.xlsx" only, however adding a argument to the
### function call so that it reads a user specified file is easy. did a test with
### "Precip.xlsx" and it worked 
###
### also, currently the aggrigation is mean and max for each year and the summer
### for each year. summer is currently defined from the ticks perspective as april-august
###
### can add more complexity such as user defined aggrigation, tho this would most likely 
### have to be a predifined function (median)
###
### "Terra_LST.xlsx" and "aqua_LST.xlsx"  have has missing values added na.rm = TRUE 

library(xlsx, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(lubridate, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(tidyverse, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(tidyr, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")

aggrigate_data <- function(full_path, met.file, town){
 
  time_series <- "/data_raw/Final_Time_Series"
  time_series <- paste(full_path, time_series, sep = "")
  files <- list.files(file.path(time_series, town))
  if(stringr::str_detect(files[1], "xlsx")){
    file.name <- paste(file.path(time_series, town, met.file), ".xlsx", sep = "")
    data <- read.xlsx(file.name, sheetIndex = 1)
    data <- data[,1:2]
    colnames(data) <- c("date", "val")
  } else {
    file.name <- paste(file.path(time_series, town, met.file), ".csv", sep = "")
    data <- read.table(file.name, sep = ",", skip = 1, na.strings = "")
    colnames(data) <- c("date", "val")
    data$date <- seq.Date(as.Date("2008-01-01"), by = 1, length.out = nrow(data))
  }

  data <- separate(data, date, c("year", "month", "day"), sep = "-")
  data$val <- as.numeric(data$val)
  
  
  year.fit <- as.character(2008:2018)
  
  ## convert aqua- and terra-modis land surface temperature to deg. C
  if(met.file %in% c("Aqua_LST", "Terra_LST")){
    data$val <- data$val*0.02 - 273.15
  }
  
  data <- subset(data, year %in% year.fit)
  
  ## year mean
  year.mean <- data %>%
    group_by(year) %>% 
    dplyr::summarise(mean = mean(val, na.rm = TRUE))
  
  ## year max
  year.max <- data %>%
    group_by(year) %>% 
    dplyr::summarise(max = max(val, na.rm = TRUE))
  
  ## define "summer"
  summer.months <- c("04", "05", "06", "07", "08")
  
  ## summer mean
  summer.mean <- data %>%
    filter(month %in% summer.months) %>% 
    group_by(year) %>% 
    dplyr::summarise(mean = mean(val, na.rm = TRUE))
  
  ## summer max
  summer.max <- data %>%
    filter(month %in% summer.months) %>% 
    group_by(year) %>% 
    dplyr::summarise(max = max(val, na.rm = TRUE))
  
  data.aggrigated <- list(year.mean = scale(year.mean[,2], scale = FALSE),
                               year.max = scale(year.max[,2], scale = FALSE),
                               summer.mean = scale(summer.mean[,2], scale = FALSE),
                               summer.max = scale(summer.max[,2], scale = FALSE),
                               year = year.fit)

  
  
  return(data.aggrigated)
}

# test <- aggrigate_data(met.file)

