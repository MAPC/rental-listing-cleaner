---
title: "rentalListings_data_cleaner"
author: "Pariya Pourmohammadi"
date: "August 10, 2017"
output:
word_document: default
pdf_document: default
html_document: default
---



## Notations on the code
Considerations about the data structure and the settings of the code:

- The warnings are ignored by the code 

```{}
options(warn=-1)
```

- There are four directories that should be set up for the file at the top of the code, and the name of the raw dataset. The directories are:
inFilePath: where the raw data is read from
outFilePath: where the summary statistics and cleaned data and results are saved
codePath: where the codes are located
spatialSrcPath: spatial data source path

- The name of raw data should be set in the **inFileName** variable

- The attributes of raw data Data frame must include following columns:
**"id","ask","bedrooms","title","address","post_at","created_at","updated_at","source_id","survey_id","latitude","longitude"**

- The reference shapefiles are required to follow the format of the attribute names and file names, if there is a change in the names (of either files or columns) the code will throw an error and stop.

shapefile names are: towns_MA.shp, comm_type.shp, census_tract.shp, 1partner_city_nhoods.shp, 2Bos_neighborhoods.shp, 3MARKET AREAS NEW_region.shp



## Code files 

This repository includes two R codes of  *Data_prep.R* and *main.R* codes. *Data_prep.R* is the script which encompasses all the functions and subfunctions(submodules) and *main.R* includes the function calls. These scripts basically construct one of the modules of a larger project named **Rental Listings Research** which is done by **Metropolitan Area Planning Council (MAPC)** of Boston. Main intention of this modeule is:

-Cleaning the data
-Spatial Joining of Various Boundaries to Points
-Validation of Number of Bedrooms through:
                                      Text Analysis
                                      Location
                                      Price



### *Data_prep.R*
Data_prep.R script includes different functions of:



#### clean_raw_listing <- function(listing)

- Removes the listings with very high or very low asking price ( < 300 and >50000)

- Cleans the title (removing punctuations, turning the title to uppercase, substitution of abreviations with original words, ) and creates a new column for the cleaned title, the old title will be kept in another column named original_title

- Removes listings with identical Unique_id (Unique_id  by appending listing$ask, listing$bedrooms, listing$title, listing$latitude, listing$longitude)

Unique_id Example: 2700 1 NO FEE Spacious One Bed w  Den HT HW Incld Close to Downtown -71.054788 42.341351

- Calls the spatial_locator function and adds new attributes for muni, community type, town, neighborhood based on latitude and longitude of each point

**input**: 
Data frame of Raw_listings which is obtained from scraping the website **This listing should be in the defined format of MAPC scraper and shopuld include these colomn names: "id","ask","bedrooms","title","address","post_at","created_at","updated_at","source_id","survey_id","latitude","longitude"**

**output**: 
The Data frame of cleaned listing which has new attributes for the location, the location attributes include: community type, town, neighborhood and Census tract



#### remove_duplicates <- function(listing, listingDup)
This function uses the results of Dupllicate_finder function. For records which are collected from craigslist, the records with lower distance (which have same group values) will be removed and the first record will be kept, and for padmapper data those records which have identical titles will be removed.

**input**: 
The cleaned list of records and list of possible duplicates **list of possible duplicates is optional, if it is not passed it will be created by a function call from inside the remove_duplicates function and recursively the remove_duplicates function will be called**

**output**: 
The Data frame of deduplicated listing



#### room_validator <- function(listing)
This function will automatically validate the attribute *bedrooms* for **studio** and **1-9** bedroom listings in the listing Data frame. This method will generate a new listing with new attributes. It will generate a new attribute called numBr which will take value of -1 for all the listings which are not validated and 0 for studio and values of 1-9 for 1-9 bedroom listings. This function will also create two dimensions per group.

For example for 1-bedroom listings it will create one col named **one_bedroom ** which is a boolean variable implying whether the point is one bedroom      or not and another column called **not_in_range_one_bedroom** which has values of -1, 0, +1. Since this function considers asking price as one        factor this dimension will clarify id the price is lower, withing or above the asking price threshold.


**input**: 
Data frame of Cleaned listings 

**output**: 
Data frame of Listings with new dimensions: 
numBR, studio, not_in_range_studio, one_bedroom, not_in_range_one_bedroom, etc.



#### spatial_locator <- function (listing)
This function Geo-locates the data and adds new dimensions for community_type, Town, Neighborhood and Census tract to the table. 

*Notation*: The reference shapefiles must follow the format of attribute names, if the attributes of reference shape does not match the column that the code reads the code will throw an Error!

**input**: 
Listings Data frame.

**output**: 
New Data frame of listings with new columns of community_type, Town, Neighborhood and Census tract 



#### Dupllicate_finder <- function(listing)
finds all the possible duplicates based on **Jaro-Winkler** *(https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance)* distance of each title with next 10 titles , exactly matching prices (this number can be changed by user, the reasons for selecting 10 as the window size is: running time, high possibility of occurence of duplicates in small proximity) and exactly matching number of bedrooms value

**input**: 
Data frame of Listings

**output**: 
Possible duplicates, with a new dimension of Group pointing to the group which each data belongs to, the points with the same group are the ones with       high similarity in title and exact matching of bedrooms and asking price values.



#### room_analysis <- function (keywrdlst, excl_list, listings, name, br, numBR, num )
This function will be called from room_validation function, but the computation that is set in room_validation is only for 0-10 bedroom listings, so user can do analysis for other values by calling this function independently. The algorithm for this method is based on data parsing. The base analysis is based on text mining and finding specific phrases (keywrdlst) in title of each record, indicating if a record can be considered as for example "studio", then another round of analysis will be done on the records based on price range information extracted from parsed data. The filteration will continue until the whole data set is analyzed.
This function will consider spatially specific statistics for defining the threshold, such that for records with neighborhood it will first consider the range price of neighborhood as the basis, if there are less records than the **num** value in a neighborhood the algorithm will investigate the statistics of the town, if there are less records than the **num** value again the algorithm will move one level upper to community type statistics. Again if there are less records than the **num** value in a neighborhood the algorithm will consider the total recs statistics for the next round of parsing.

**input**:
keywrdlst: the vector of keywords possibly indicate the target group

listing: the Data frame of listings which require to be processed

name: a string for the name of the field to be added to the table

br: expected number of bedrooms in the bedrooms field, for example for 1 bedroom records it is expected that the number of bedrooms is claimed as 1 in    the original data

numBR: the value for num_bedrooms that we want to set, for example for 1 bedroom records user prefers the number of bedrooms to be set as 1

num: the threshold of #of records per neighborhood

**output**: 
Listing with two new attributes for binary data, if a point is in the specific category or not. If the record's price is in the range or not. for example for 1-bedroom listings it will create one col named **one_bedroom ** which is a boolean variable implying whether the point is one bedroom or not and another column called **not_in_range_one_bedroom** which has values of -1, 0, +1



#### comb <- function(index)
create all the possible combination of words and generates records which point to specific group in room analysis. 

```{r}
require(ngram)

comb <- function(index){
property_type <- c("APARTMENT", "HOUSE", "TOWNHOUSE", "DUPLEX", "CONDO", "FLAT")
numbers_str <- c("ONE", "TWO" ,"THREE","FOUR","FIVE","SIX","SEVEN","EIGHT","NINE", "TEN")
ind <- index
index2 <- length(property_type)
n <- 1
x <- NULL

while (index > 0){
while(index2 > 0){
x[n] <- concatenate(numbers_str[index],"BEDROOM", property_type[index2], collapse = " ")
index2 <- index2-1
n <- n+1
}
index <- index-1
}

index <- ind
index2 <- length(property_type)
index3 <- ind
n <- 1
x1 <- NULL

while (index > 0){
while(index2 > 0){
while(index3 > 0){
x1[n] <- concatenate(numbers_str[index],"BEDROOM", numbers_str[index3] , "BATHROOM",property_type[index2], collapse = " ")
index3 <- index3-1
n <- n+1
}
index3 <- ind
index2 <- index2-1

}
index <- index-1
}
list <- c(x, x1)

return(list)
}

print(comb(2))

```

**input**: The number of bedrooms to create keyword list

**output**:The Keyword list



#### Threshold_finder <- function (listing, name)
Finds the median and standard deviation of asking price of any listing that is passed to the function and returnes the values of median, sd, lower and upper bounds. Lower bound is computed by subtraction of 1sd from median. If lower than 500$ will take 0.5 sd, if still lower than 500$ will consider 500$ as the lower bound. this function is a supporting function for room_analysis.

**input**: 
The df of listings and the name of boolean attribute based on whoch statistics are computed.

**output**:
Summary statistics of the points with value of 1 in the target attribute.



#### comm_type_rent_stat <- function(recs)
Finds the median and standard deviation of asking price of any listing that is passed to the function per community type, this function returnes the values of median, sd, lower and upper bounds for each Comm_type. Lower bound is computed by subtraction of 1sd from median. If lower than 500$ will take 0.5 sd, if still lower than 500$ will consider 500$ as the lower bound. this function is a supporting function for room_analysis.

**input**:
Data frame of Listings 

**output**:
Table of community types and asking price per comm type statistics.



#### town_rent_stat <- function(recs)
Finds the median and standard deviation of asking price of any listing that is passed to the function per Town, this function returnes the values of median, sd, lower and upper bounds for each town Lower bound is computed by subtraction of 1sd from median. If lower than 500$ will take 0.5 sd, if still lower than 500$ will consider 500$ as the lower bound. this function is a supporting function for room_analysis.

**input**:
Data frame of Listings

**output**:
Table of towns and asking price per town statistics.



#### neighborhood_stat <- function(recs)
Finds the median and standard deviation of asking price of any listing that is passed to the function per neighborhood, this function returnes the values of median, sd, lower and upper bounds for each neighborhood Lower bound is computed by subtraction of 1sd from median. If lower than 500$ will take 0.5 sd, if still lower than 500$ will consider 500$ as the lower bound. this function is a supporting function for room_analysis.

**input**:
Data frame of Listings with neighborhood attribute

**output**:
Table of neighborhoods and asking price per neighborhood statistics.



#### exclude_recs <- function (listing, excl_list)
This function takes a listing and removes all the records which have any of features in the excluding list (which is a vector).

**input**:
A data frame of listings and vector of excluding phrases/words.

**output**:
A data frame that has none of the elements of excluding list in the title attribute.



#### n_gram_builder <- function(listing,val)
Creates a large text of the titles in a listing and constructs table of n-grams from the text, this function is an auxhilary function which can be applied in more complicated computations and text mining methods. 

**input**:
The data frame of listings, the value of n in n-gram. 

**output**:
A dataframe of ngrams, frequency and probability of each n-gram.



### sample.DF <-function(x, percentile) 
Builds a random sample of each data frame.

**input**:
A Data frame (x)  and the percentage of samples.


**output**:
Returns a randomly selected data frame of input data frame (x)



#### generate_stat_tables <- function (listings)
This function will generate tables for statistics of data frame based on town and all attributes of neighborhood.

**input**:
A data frame of listings.

**output**:
Prints  the results of neighboprhood, census tract, town statistics on the file in outFilePath directory.



## *main.R*
This script includes function calls of:

*listings_unique <- clean_raw_listing(raw_listings)*

*listings_unique <- remove_duplicates(listings_unique)*

*listings_unique <- room_validator(listings_unique)*

*generate_tables(listings_unique)*


## *Future work*
To improving the script it is suggested that:

Probability functions be applied to each data point and added to the table as a new dimensions, these probabilities will imply the possibility that each point falls in one group. Through these weights another round of room validation is fissible.

Dynamic computation of asking price ranges, these ranges can get more accurate according to the location of points. for this script all the points follow a uniform rule for computation of ranges and making final decision about the category that the point belongs to.
