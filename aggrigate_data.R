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

library(xlsx)
library(lubridate)
library(tidyverse)


time_series <- "data_raw/Final_Time_Series" # work with real time series
towns <- list.files(time_series) # list of towns that have final time series

aggrigate_data <- function(){
  for(t in seq_along(towns)){
    data <- read.xlsx(file.path(time_series, towns[t], "VPD.xlsx"), sheetIndex = 1)
    data <- data[,1:2]
    colnames(data) <- c("date", "val")
    
    ## i think read.xlsx() does this step for us
    ## Make to lubridate Date format year-month-day
    # data$date <- lubridate::as_date(data$date ,tz = "EST",  format = "%B %d, %Y") %>% 
    
    ## separate year, month, day into their own columns
    data <- separate(data, date, c("year", "month", "day"), sep = "-")
    
    ## hold out years 2016-2019 for validation
    data <- data %>% filter(!(year %in% c("2016", "2017", "2018", "2019")))
    
    ## year mean
    year.mean <- data %>%
      group_by(year) %>% 
      dplyr::summarise(mean = mean(val), na.rm = TRUE)
    
    ## year max
    year.max <- data %>%
      group_by(year) %>% 
      dplyr::summarise(max = max(val), na.rm = TRUE)
    
    ## define "summer"
    summer.months <- c("04", "05", "06", "07", "08")
    
    ## summer mean
    summer.mean <- data %>%
      filter(month %in% summer.months) %>% 
      group_by(year) %>% 
      dplyr::summarise(mean = mean(val), na.rm = TRUE)
    
    ## summer max
    summer.max <- data %>%
      filter(month %in% summer.months) %>% 
      group_by(year) %>% 
      dplyr::summarise(max = max(val), na.rm = TRUE)
    
    data.aggrigated[[t]] <- list(year.mean = scale(year.mean, scale = FALSE),
                                 year.max = scale(year.max, scale = FALSE),
                                 summer.mean = scale(summer.mean, scale = FALSE),
                                 summer.max = scale(summer.max, scale = FALSE))
    print(t) # counter
  
  }
  names(data.aggrigated) <- towns
  return(data.aggrigated)
}

# test <- aggrigate_data()

