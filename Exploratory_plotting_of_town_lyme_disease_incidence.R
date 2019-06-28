

### Reading in tick data
tick_disease_big <- read.csv("/Users/tess/Documents/work/Maine_Tick_Disease_Forecast/town_level_data_lyme.csv", header = TRUE, stringsAsFactors = FALSE)
towns_in_cumberland <- c("Harpswell", "Freeport", "Brunswick", "Pownal", "Fyre_Island", "Yarmouth", 
                         "Cumberland", "Standish", "Chebeague_Island", "Portland", "Falmouth", "South_Portland", 
                         "Cape_Elizabeth", "Long_Island", "Scarborough", "Harrison", "Gray", "North_Yarmouth", 
                         "Baldwin", "Bridgton", "Casco", "Raymond", "Naples", "Sebago", "Windham", "Gorham",
                         "Westbrook", "New_Gloucester")
unique(tick_disease_big$Number)

tick_disease <- tick_disease_big[tick_disease_big$Location %in% towns_in_cumberland, ]

unique(tick_disease$Number)


### Reading in test data

#Generate all the file paths
file_path <- matrix(NA, nrow = length(towns_in_cumberland), ncol = 1)
for (i in seq_along(towns_in_cumberland)){
  file_path[i] <- paste("/Users/tess/Documents/work/Maine_Tick_Disease_Forecast/data_raw/Test_Time_Series","/", towns_in_cumberland[i],"/", sep = "")
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




