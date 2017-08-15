# Main method to call the functions of Data_prep.R
# Created by: Pariya Pourmohammadi
# Date: June/09/17

options(scipen=999)
options(max.print=10000)
options(warn=1)

#inFilePath <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/"
#inFileName <- "listings_20170324.csv"
#outFilePath <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/output/"
#codePath <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/r_scripts/analysis/rental-listing-cleaner/"
#spatialSrcPath <- "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"

inFilePath <- Sys.getenv("IN_FILE_PATH")
inFileName <- Sys.getenv("IN_FILE_NAME")
inGeoName <- Sys.getenv("IN_GEO_NAME")
outFilePath <- Sys.getenv("OUT_FILE_PATH")

codePath <- Sys.getenv("CODE_PATH")
spatialSrcPath <- Sys.getenv("SPATIAL_SRC_PATH")


######function calls #############
getwd()
setwd(inFilePath)

####load Raw data####
raw_listings <- read.csv(inFileName, 
                         header=FALSE, col.names=c("id","ask","bedrooms","title","address","post_at","created_at",
                                                   "updated_at","source_id","survey_id","latitude","longitude"))

setwd(codePath)

#import functions from Data_prep.R code####
source("Data_prep.R")

####clean_raw_listing function call from Data_prep.R to clean the data
listings_unique <- clean_raw_listing(raw_listings)

####Dupllicate_finder function call from Data_prep.R to identify the  possible duplicates####
listings_unique <- remove_duplicates(listings_unique)

#########Room Validation adds new columns to the table for each category and ###########
#######Creates files for statistics and records inside and outside any of the groups####
###### room_validator function calls room_analysis() function and comb() function##########
######this function automatically does validation for studio and 1-10 bedrooms#########
listings_unique <- room_validator(listings_unique)

#merge the data of listings unique with geo-located data
#carlos_file <- read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/carlos/170726_padmapper_sample_2.csv")
setwd(inFilePath)
carlos_file <- read.csv(inGeoName)
y = carlos_file[ , c("id","fwd_geolocated", "rev_geolocated",'latitude_merge','longitude_merge','joint_addresses_merge','mapzen_geolocated','mapzen_confidence')] 
listings_unique <- merge(listings_unique, y, by ="id",all.x = TRUE)

###frequency table for the whole dataset/listings_unique and sample
generate_tables(listings_unique)
