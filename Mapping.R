#___________________________________________________________________________________________________________________________________
# 1.) This is an example workflow that retrieves addresses from OpenAddresses.io, and plots housing density in Portland, Oregon. 
# 2.) This is an example workflow that retrieves addresses for all income restriced housing in Seattle, WA and plots them on a map.
#
# By: James Bowers 
# 
# The data used in this code was taken from: 
# https://openaddresses.io
# https://data.seattle.gov/Community/Rent-and-Income-Restricted-Housing/b6zn-zsin
# 
#___________________________________________________________________________________________________________________________________


## 1.)  MAPPING - HOUSING DENSITY IN PORTLAND, OR ####

library(dplyr)
library(readtext)
library(ggmap)
library(RColorBrewer)



options(scipen=999)

setwd("C:/Users/hj163e/Documents/My Folder/Personal/GitHub Projects/Geocoding and Mapping")
getwd()


### Get Data ####

#### download .zip file and store in working directory
dataUrl <- "https://s3.amazonaws.com/data.openaddresses.io/openaddr-collected-us_west.zip"
download.file(dataUrl, "uswest.zip")

#### unzip the contents of .zip folder and store in working directory
zipFilePath <- paste0(getwd(),"/uswest.zip")
unzip(zipFilePath)

#### get list of states (also the name of individual folders within "uswest.zip/us" )
stateList <- list.files(path=paste0(getwd(),"/us"))
stateList <- stateList[c(10,12)] # limit to just OR

#### initialize data frame for all address info
allData <- data.frame()

#### loop through all folders and retrieve the data for each city/state
for (i in 1:length(stateList)) {
  # get file path for the state
  filePath <- sprintf(paste0(getwd(),"/us/%s"), stateList[i])
  # get the names of all files for the state
  allStateFiles <- list.files(filePath)
  # get index for .csv files for the state 
  allStateFilesCSVIndex <- regexpr(".csv", allStateFiles, useBytes=FALSE)
  # get names of all .csv files for the state 
  allStateCSV <- allStateFiles[allStateFilesCSVIndex != -1]
    for (i2 in 1:length(allStateCSV)) {
      # only try to read .csv files that are > 100 bytes
      if(file.size(paste0(filePath,"/",allStateCSV[i2])) > 100) {
        # retrieve the data from .csv file  
        cityData <- read.csv(sprintf(paste0(getwd(), "/us/%s/", allStateCSV[i2]), stateList[i]), stringsAsFactors = FALSE)
        # add city and state abbreviation to data frame
        cityData$CITY <- allStateCSV[i2]
        cityData$ST <- stateList[i]
        # concatenate data for city into larger list of data for all cities
        allData <- rbind(allData,cityData)
      }
   }
}

#### check to ensure we got records from all states
table(allData$ST)

### Clean Data #### 
#### keep only: Long, Lat, Number, Street, Unit, State (Not City, comes from file folder name, not accurate)
allData_clean <- allData[ ,c(1,2,3,4,5,12)]
#### capitalize state abbreviation
allData_clean$ST <- toupper(allData_clean$ST)
#### concatenate address fields 
allData_clean <- mutate(allData_clean,Complete_Address = 
                          paste(NUMBER, STREET, UNIT, ST, sep=" "))

head(allData_clean)

#### keep only distinct long, lat and complete address combinations
allData_clean <- allData_clean[ ,c(1,2,7)]
allData_clean_unq <- distinct(allData_clean)

head(allData_clean_unq)

#### keep just the long and lat for mapping
or_lonlat_uniq <- allData_clean_unq[ ,1:2]

head(or_lonlat_uniq)

### Generate Map ####
#### get base map
PDX <- get_map(location = "Portland, OR", zoom = 11, maptype = "roadmap")
ggmap(tst) 

library(ggmap)
tst <- get_map()

#### filter long and lat data to those +/- .2 degrees of lat/long map limits
lower_limits <- attr(PDX, "bb")-.2
upper_limits <- attr(PDX, "bb")+.2

pdx_lonlat_uniq <- filter(or_lonlat_uniq, LON <= upper_limits$ur.lon & LON >= lower_limits$ll.lon
                                        & LAT <= upper_limits$ur.lat & LAT >= lower_limits$ll.lat)

####  plot density map (edges of map alter the density plot)
ggmap(PDX, extent = "panel", maprange = FALSE) + 
  geom_density2d(data = pdx_lonlat_uniq, mapping = aes(x = LON, y = LAT), size = 0.5, bins=30) +
  stat_density2d(data = pdx_lonlat_uniq, aes(x = LON, y = LAT, fill = ..level.., alpha = ..level..),
                 bins = 30, geom = "polygon") +
  scale_fill_gradient(low = "green3", high = "red2") +
  scale_alpha(range = c(0.1, 0.25)) +
  labs(title = "Housing Density\nPortland, OR", subtitle=("(distorted from map edges)")) +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        plot.title = element_text(size=13, face = "bold", hjust = 0.5))


#### plot density map (without edges altering the density plot)
ggmap(PDX, extent = "normal", maprange = FALSE) %+% 
  pdx_lonlat_uniq + aes(x = LON, y = LAT) + # define data here, not in density functions to allow plotting outside map edges
  geom_density2d(size = 0.5, bins=30) +
  stat_density2d(aes(fill = ..level.., alpha = ..level..), bins = 30, geom = "polygon") +
  scale_fill_gradient(low = "green3", high = "red2") +
  scale_alpha(range = c(0.1, 0.25)) +
  coord_map(projection="mercator", # cuts the edges off and only display the map portion
              xlim=c(attr(PDX, "bb")$ll.lon, attr(PDX, "bb")$ur.lon),
              ylim=c(attr(PDX, "bb")$ll.lat, attr(PDX, "bb")$ur.lat)) +
  labs(title = "Housing Density\nPortland, OR", subtitle=("(non-distorted)")) +
  theme(legend.position = "none", 
        axis.title = element_blank(),
        plot.title = element_text(size=13, face = "bold", hjust = 0.5))












### 2.) MAPPING - INCOME RESTRICTED HOUSING IN SEATTLE, WA ####

#### Define Functions #### 

##### Input an address and return the lat and long using the Google Geocode API
geocode_wait <- function(Address) {
  lat.long <- data.frame(NA,NA)
  counter <- 1
  while (is.na(lat.long[1, 1]) & counter <= 5) {
    lat.long <- geocode(Address)
    result <- cbind(lat.long, Address)
    counter <- counter+1
  }
  Sys.sleep(5)
  return(result)
}

#### Takes input from geocode_wait and returns a tidy dataframe of results
clean_geocode <- function(geocode_wait_result) {
  unlisted_data <- data.frame(matrix(unlist(geocode_wait_result), nrow=length(geocode_wait_result), byrow=T), stringsAsFactors=FALSE)[,1:2]
  colnames(unlisted_data) <- c("lon","lat")
  return(unlisted_data)
}

### Get Data ####
allData <- read.csv(url("https://data.seattle.gov/api/views/b6zn-zsin/rows.csv?accessType=DOWNLOAD"), stringsAsFactors = FALSE)


### Clean Data ####
str(allData)
#### rename columns
colnames(allData) <- c("Name","Address","Zip","UnitsWithLimits","HUD_Ind","SHA_Ind","State_Ind",
                       "WSHFC_Ind","SeattleCity_Ind","YearInService","CouncilDistrict")
#### create a complete address field
allData <- mutate(allData, CompleteAddress = paste0(Address, ", SEATTLE, WA ", Zip))


### Geocode Data ####
GeocodedAddressesFinal <- geocode(tst$CompleteAddress, output = "latlona")

#### If the above does return all values, use the defined functions
#### instead to throttle the requests to the Google API
GeocodedAddresses <- lapply(tst[,12], geocode_wait)
#### convert geocode list to tidy dataframe 
Geocodes <- clean_geocode(GeocodedAddresses)
#### concatenate geocodes back with all data
GeocodedAddressesFinal <- cbind(Geocodes, tst)

head(GeocodedAddressesFinal)


### Generate Map ####
#### get base map
SEA <- get_map(location = "Seattle, WA", zoom = 12, maptype = "roadmap")
ggmap(SEA) 

#### define colors for districts
myPalette <- colorRampPalette(brewer.pal(7, "Set1"))

#### plot scatterplot map
ggmap(SEA, extent = "panel", maprange = FALSE) + 
  geom_point(data = GeocodedAddressesFinal, mapping = aes(x = lon, y = lat, size = UnitsWithLimits, color = CouncilDistrict), alpha=0.75) +
  scale_colour_gradientn(colours = myPalette(100), limits=c(1, 7))

