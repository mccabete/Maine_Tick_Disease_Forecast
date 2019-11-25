

### Reading in tick data
tick_disease_big <- read.csv("town_level_data_lyme.csv", header = TRUE, stringsAsFactors = FALSE)

tick_disease_big$Population <- gsub(pattern = ",", replacement = "", tick_disease_big$Population)

towns_in_cumberland <- c("Harpswell", "Freeport", "Brunswick", "Pownal",  "Yarmouth", 
                         "Cumberland", "Standish", "Chebeague_Island", "Portland", "Falmouth", "South_Portland", 
                         "Cape_Elizabeth", "Long_Island", "Scarborough", "Harrison", "Gray", "North_Yarmouth", 
                         "Baldwin", "Bridgton", "Casco", "Raymond", "Naples", "Sebago", "Windham", "Gorham",
                         "Westbrook", "New_Gloucester") # "Frye_Island",
unique(tick_disease_big$Number)

towns_for_validation <- c("Bridgton","Sebago",  "Westbrook")




## Correct names for spacing
tick_disease_big$Location <- gsub(" ", "_", tick_disease_big$Location)
tick_disease <- tick_disease_big[tick_disease_big$Location %in% towns_in_cumberland, ]

towns_suppresed <- dplyr::filter(tick_disease, tick_disease$Rate == "*"|tick_disease$Rate == "NR") %>%
  dplyr::distinct(Location)

towns_to_run <- towns_in_cumberland[!((towns_in_cumberland %in% towns_for_validation))]
#towns_to_run <- towns_in_cumberland[!((towns_in_cumberland %in% as.vector(towns_suppresed$Location)))]

unique(tick_disease$Number)



### Reading in test data

#Generate all the file paths
file_path <- matrix(NA, nrow = length(towns_in_cumberland), ncol = 1)
for (i in seq_along(towns_in_cumberland)){
  file_path[i] <- paste("data_raw/Test_Time_Series","/", towns_in_cumberland[i],"/", sep = "")
}

## Precip
precip <- matrix(data= NA, nrow = 1, ncol = 3)
precip <- as.data.frame(precip)
names(precip) <- c("date", "ppt", "town")
for( i in seq_along(file_path)){
  path  <- paste(file_path[i], "Precip.csv", sep = "")
  tmp_csv <- read.csv(file = path, header = FALSE, stringsAsFactors = FALSE, skip = 1)
  tmp_csv$town <- towns_in_cumberland[i]
  names(tmp_csv) <- c("date", "ppt", "town")
  precip <- rbind(precip, tmp_csv)
}

## VPD
vpd <- matrix(data= NA, nrow = 1, ncol = 3)
vpd <- as.data.frame(vpd)
names(vpd) <- c("date", "vpd", "town")
for( i in seq_along(file_path)){
  path  <- paste(file_path[i], "VPD.csv", sep = "")
  tmp_csv <- read.csv(file = path, header = FALSE, stringsAsFactors = FALSE, skip = 1)
  tmp_csv$town <- towns_in_cumberland[i]
  names(tmp_csv) <- c("date", "vpd", "town")
  vpd <- rbind(vpd, tmp_csv)
}

## NDVI
ndvi <- matrix(data= NA, nrow = 1, ncol = 3)
ndvi <- as.data.frame(ndvi)
names(ndvi) <- c("date", "ndvi", "town")
for( i in seq_along(file_path)){
  path  <- paste(file_path[i], "NDVI.csv", sep = "")
  tmp_csv <- read.csv(file = path, header = FALSE, stringsAsFactors = FALSE, skip = 1)
  tmp_csv$town <- towns_in_cumberland[i]
  names(tmp_csv) <- c("date", "ndvi", "town")
  ndvi <- rbind(ndvi, tmp_csv)
}

## LST
lst <- matrix(data= NA, nrow = 1, ncol = 3)
lst <- as.data.frame(lst)
names(lst) <- c("date", "lst", "town")
for( i in seq_along(file_path)){
  path  <- paste(file_path[i], "LST.csv", sep = "")
  tmp_csv <- read.csv(file = path, header = FALSE, stringsAsFactors = FALSE, skip = 1)
  tmp_csv$town <- towns_in_cumberland[i]
  names(tmp_csv) <- c("date", "lst", "town")
  lst <- rbind(lst, tmp_csv)
}
lst$lst[lst$lst == ""] <- NA
lst$lst <- gsub(pattern = ",", replacement = "", lst$lst)
lst$lst <- as.numeric(lst$lst)
lst$lst <- lst$lst * 0.02 # multiply by scaling factor to put into K


### Aggrigating to annual: growing degree days
upper_threshold <- 0.002
lower_threshold <- 0.9

GetVpdDays <- function(vpd, towns_in_cumberland, upper_threshold, lower_threshold){ ## Not official calcualtion

  ### Make the output datafrate
  vpd$date <- lubridate::as_date(vpd$date,tz = "EST",  format = "%B %d, %Y") ## Make to lubridate Date format
  ncol <- 3
  nrow <- length(towns_in_cumberland) * length(na.omit(unique(lubridate::year(vpd$date))))
  annual_vpd <- matrix( data = NA, nrow = nrow, ncol = ncol)
  annual_vpd <- as.data.frame(annual_vpd)
  names(annual_vpd) <- c("year", "town", "vpd_days")
  
  ### By town min and max
  index_in_matrix <- 1
  for (i in seq_along(towns_in_cumberland)){
    
    tmp <- vpd[vpd$town == towns_in_cumberland[i], ]
    years <- na.omit(unique(lubridate::year(vpd$date)))
    
    
    for(k in seq_along(years)){ ## yearly loop
      vpd_days <- 0
      tmp_year <- tmp[lubridate::year(tmp$date) == years[k], ]
      days <- na.omit(lubridate::day(tmp_year$date))
      
      for(j in seq_along(days)){ ## Check each day in year
        one_day <- tmp_year[lubridate::day(tmp_year$date) == days[j],] ### Stupid day
        min <- min(na.omit(one_day$vpd))
        max <- max(na.omit(one_day$vpd))
        
        within_threshold <- (min > lower_threshold) & (max < upper_threshold)
        
        if(within_threshold){
          vpd_days <- vpd_days + 1
        }
      }
      
      annual_vpd$town[index_in_matrix] <- towns_in_cumberland[i]
      annual_vpd$year[index_in_matrix] <- years[k]
      annual_vpd$vpd_days[index_in_matrix] <- vpd_days
      index_in_matrix <- index_in_matrix + 1
    }
    
    
  }
  
  
} ## Returns a matrix of towns by years with days within a humidity threshold 




