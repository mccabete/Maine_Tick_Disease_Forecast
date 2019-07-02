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
### don't think this will work with "Terra_LST.xlsx" as it has missing values (add na.rm = TRUE??)

library(xlsx)
library(lubridate)
library(tidyverse)


time_series <- "data_raw/Final_Time_Series" # work with real time series
towns <- list.files(time_series) # list of towns that have final time series

aggrigate_data <- function(){
  for(t in seq_along(towns)){
    data <- read.xlsx(file.path(time_series, towns[t], "Daymet_max.xlsx"), sheetIndex = 1)
    data <- data[,1:2]
    colnames(data) <- c("date", "val")
    
    ## i think read.xlsx() does this step for us
    ## Make to lubridate Date format year-month-day
    # data$date <- lubridate::as_date(data$date ,tz = "EST",  format = "%B %d, %Y") %>% 
    
    ## separate year, month, day into their own columns
    data <- separate(data, date, c("year", "month", "day"), sep = "-")
    
    ## year mean
    year.mean <- data %>%
      group_by(year) %>% 
      dplyr::summarise(mean = mean(val))
    
    ## year max
    year.max <- data %>%
      group_by(year) %>% 
      dplyr::summarise(max = max(val))
    
    ## define "summer"
    summer.months <- c("04", "05", "06", "07", "08")
    
    ## summer mean
    summer.mean <- data %>%
      filter(month %in% summer.months) %>% 
      group_by(year) %>% 
      dplyr::summarise(mean = mean(val))
    
    ## summer max
    summer.max <- data %>%
      filter(month %in% summer.months) %>% 
      group_by(year) %>% 
      dplyr::summarise(max = max(val))
    
    data.aggrigated[[t]] <- list(year.mean = year.mean,
                                 year.max = year.max,
                                 summer.mean = summer.mean,
                                 summer.max = summer.max)
    print(t) # counter
  
  }
  names(data.aggrigated) <- towns
  return(data.aggrigated)
}

test <- aggrigate_data()

