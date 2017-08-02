# Created by: Pariya Pourmohammadi
# Date: June/09/17
# This code does data preparation including n-grams, and B of W, br_Analysis, spatial locator
.libPaths("C:\\Program Files\\R\\R-3.4.0\\library")

options(scipen=999)
options(max.print=10000)

require(plyr)
require(ggplot2)
require(stringr)
require(stringdist)
require(ngram)
require(sp)
require(rgdal)
require(raster)
require(foreign)

clean_raw_listing <- function(listing){
  listing<-raw_listings
  #Convert date strings to numeric
  listing$post_date <- as.Date(listing$post_at)
  listing$created_date <- as.Date(listing$created_at)
  listing$updated_date <- as.Date(listing$updated_at)
  
  original_title <- listing$title
  
  listing <- cbind(listing[,1:4],original_title,listing[,5:15])
  
  listing$year <-substr(as.character(listing$updated_date),1,4)
  listing$month <- substr(as.character(listing$updated_date),6,7)
  
  
  gsub(" AVE ", " AVENUE ", listing$title)
  
  listing$title <- gsub(".5", "",listing$title)
  listing$title <- gsub(".0", "",listing$title)
  
  #remove characters
  ###round up the 1.56 records
  listing$title <- gsub("[[:punct:]]", " ", listing$title)
  
  #Remove duplicate titles
  listing <- listing[!(listing$title == 'None'), ]
  listing$uniqueid <- paste(listing$ask,listing$bedrooms,listing$title,listing$latitude,listing$longitude)
  listing <- subset(listing[!duplicated( listing$uniqueid), ])
  
  #drop outliers
  listing <- subset(listing, as.numeric(listing$ask) >= 301 & as.numeric(listing$ask) <= 50000)
  
  listing$title <- paste0(" ",listing$title)
  listing$title <- paste0(listing$title, " ")
  
  listing$title <- toupper(listing$title)
  listing$title <- iconv(listing$title, "UTF-8", "ASCII", sub = " " )
  
  listing$title <- gsub("[0-9]*+BR+[0-9]*+BA", "[0-9]*+ BR +[0-9]*+ BA", listing$title, fixed=TRUE)
  listing$title <- gsub(" [0-9]*+BR ", " [0-9]*+  BR ", listing$title, fixed=TRUE)
  listing$title <- gsub(" [0-9]*+BA ", " [0-9]*+  BA ", listing$title, fixed=TRUE)
  listing$title <- gsub('"'," ",listing$title)
  
  listing$title <- gsub("\n", " ", listing$title)
  listing$title <- gsub("BR ", " BEDROOM ", listing$title)
  listing$title <- gsub("BA ", " BATHROOM ", listing$title)
  listing$title <- gsub(" BATH ROOM ", " BATHROOM ", listing$title)
  listing$title <- gsub(" BATH  ", " BATHROOM ", listing$title)
  listing$title <- gsub(" AVAIL ", " AVAILABLE ", listing$title)
  listing$title <- gsub(" SQ ", " SQUARE ", listing$title)
  listing$title <- gsub(" HT ", " HEAT ", listing$title)
  listing$title <- gsub(" HW ", " HOT WATER ", listing$title)
  listing$title <- gsub(" W D ", " WASHER/DRYER ", listing$title)
  listing$title <- gsub(" ST ", " STREET ", listing$title)
  listing$title <- gsub(" BD ", " BEDROOM ", listing$title)
  listing$title <- gsub(" BED ", " BEDROOM ", listing$title)
  listing$title <- gsub(" BDRM", " BEDROOM ", listing$title)
  listing$title <- gsub(" BRS ", " BEDROOM ", listing$title)
  listing$title <- gsub(" BEDROOMS ", " BEDROOM ", listing$title)
  # listing$title <- gsub(" ROOM ", " BEDROOM ", listing$title)
  #  listing$title <- gsub(" ROOMS ", " BEDROOMS ", listing$title)
  listing$title <- gsub(" RED LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" BLUE LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" ORANGE LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" GREEN LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" SILVER LINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" REDLINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" BLUELINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" ORANGELINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" GREENLINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" SILVERLINE ", " TRANSIT ", listing$title)
  listing$title <- gsub(" BUS ROUTE ", " BUSROUTE ", listing$title)
  listing$title <- gsub(" BUSROUTE ", " BUSROUTE ", listing$title)
  listing$title <- gsub(" T ", " TRANSIT ", listing$title)
  listing$title <- gsub(" INC ", " INCLUDED ", listing$title)
  listing$title <- gsub(" INCLD ", " INCLUDED ", listing$title)
  listing$title <- gsub(" INCL ", " INCLUDED ", listing$title)
  listing$title <- gsub(" INCLUDING ", " INCLUDED ", listing$title)
  # listing$title <- gsub(" A ", " 1 ", listing$title)
  listing$title <- gsub(" MILES ", " MILE ", listing$title)
  listing$title <- gsub(" APT ", " APARTMENT ", listing$title)
  listing$title <- gsub(" WANTED ", " NEEDED ", listing$title)
  listing$title <- gsub(" SPACIOUS ", " LARGE ", listing$title)
  listing$title <- gsub(" NEAT ", " CLEAN ", listing$title)
  listing$title <- gsub(" GORGEOUS ", " BEAUTIFUL ", listing$title)
  listing$title <- gsub(" JP ", " JAMAICA PLAIN ", listing$title)
  listing$title <- gsub(" BLDG ", " BUILDING ", listing$title)
  listing$title <- gsub(" PRKNG ", " PARKING ", listing$title)
  listing$title <- gsub(" BDS ", " BEDROOM ", listing$title)
  listing$title <- gsub(" HWD ", " HARDWOOD ", listing$title)
  listing$title <- gsub(" RD ", " ROAD ", listing$title)  
  listing$title <- gsub(" AVE ", " AVENUE ", listing$title)
  listing$title <- gsub(" LOCA ", " LOCATION ", listing$title)
  listing$title <- gsub(" APLCS ", " APPLIANCES ", listing$title)
  
  numbers <- c(" 1 ", " 2 " ," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 ", " 10 ")
  numbers_str <- c(" ONE ", " TWO " ," THREE "," FOUR "," FIVE "," SIX "," SEVEN "," EIGHT "," NINE ", " TEN ")
  
  for (i in (1: length(numbers))){
    listing$title  <- gsub(numbers[i], numbers_str[i], listing$title )
  }
  
  while(any(str_detect(listing$title, "  "))){
    listing$title <- gsub("  ", " ", listing$title)
  }
  
  listing <- listing[which(listing$source_id == 2 | (listing$source_id==1 & (str_detect(listing$title, "APARTMENT")
                                                                             |str_detect(listing$title, "BED")|str_detect(listing$title, "STUDIO")
                                                                             |str_detect(listing$title, "LOFT")|str_detect(listing$title, "CONDO")
                                                                             |str_detect(listing$title, "HOUSE")|str_detect(listing$title, "BUILDING")
                                                                             |str_detect(listing$title, "UNIT")|str_detect(listing$title, "ROOM")
                                                                             |str_detect(listing$title, "BATH")))), ]
  
  wrd_lst <- c(" ON "," FOR "," A "," AT "," WHERE "," TO "," THE "," OF ",
               " WHEN ", " WITH ", " AND ")
  
  for (i in (1: length(wrd_lst))){
    listing$title   <- gsub(wrd_lst[i], " ", listing$title )
  } 
  
  # listing <- cbind(listing[,1:5],cl_title,listing[,6:19])
  listing <- listing[-which(listing$bedrooms <0),]
  
  listing <- spatial_locator(listing)
  
  return(listing)
}
#compute the Jaro–Winkler distance between each title and the 10 next
#if less than 0.15 check the price and # of bedrooms if they are identical 
#label the record as duplicate


Dupllicate_finder <- function(listing){
  
  dup_indices <- list()
  
  for (i in (1:(length(listing[,1])-10))){
    tmp2<- NULL      
    tmp1 <- NULL
    
    temp1_dist <- stringdist(listing$title[i],listing$title[(i+1):(i+10)], method = "jw", p=0.1)
    tmp1 <- i+ which(temp1_dist< 0.15)
    tmp1 <- tmp1[which(listing$ask[i] == listing$ask[tmp1] )]
    
    if(length(tmp1 > 0)) {
      tmp2 <- tmp1[which(listing$bedrooms[i] == listing$bedrooms[tmp1])]
    }
    
    if ((length(tmp2)>0))  {
      dup_indices[length(dup_indices)+1] <- list(c(i, tmp2))
      
    } 
  }
  
  duplicate_listing_indices <- reshape2:: melt(dup_indices)
  names(duplicate_listing_indices) <- c("index", "group")
  duplicate_listing_indices <- duplicate_listing_indices[-which(duplicated(duplicate_listing_indices$index)),]
  
  duplicate_listing <-listing[duplicate_listing_indices$index,]
  group <- duplicate_listing_indices$group
  duplicate_listing <- cbind(duplicate_listing, group)
  
  return(duplicate_listing)
}


#this function removes duplicates according to results of duplicate finder, 
#craigslist duplicates will be deleted, padmapper duplicates if have identical titles will be romoved
remove_duplicates <- function(listing, listingDup){
  craigs_list_dups <- listingDup[which(listingDup$source_id==1),]
  
  craigs_list_dedup <- subset(craigs_list_dups[!duplicated( craigs_list_dups$group), ])
  
  listing <- listing[-which(listing$id %in% craigs_list_dups$id),]
  listing <- rbind(listing, craigs_list_dedup[,1:27])
  
  pm_dups <- listingDup[which(listingDup$source_id ==2),]
  for (i in (1:length(unique(pm_dups$group)))){
    temp <- pm_dups[which(pm_dups$group == unique(pm_dups$group)[i]),]
    id <- temp$id[which(duplicated(temp$title))]
    if(!identical(id, integer(0))){
      listing <- listing[-which(listing$id %in% id),]
    }
  }
  
  return(listing)
}


##########transfor the text into bag of words (1,2,3,4,5 grams)#############
n_gram_builder <- function(listing,val){
  titles <-listing$cl_title
  titles <- paste(titles, " ")
  
  large_txt <- paste(titles, collapse=" :::: ")
  
  #uni_grams
  if (val==1) {
    col_names <- c("ngrams", "Freq")
    comb_one <- strsplit(titles,split = " ")
    wrd_gram <- data.frame(table(unlist(comb_one)))
    names(wrd_gram) <- col_names
    wrd_gram <- wrd_gram[str_which(wrd_gram$ngrams, "[^[:digit:]]"), ]
    wrd_gram$prop <- wrd_gram$Freq/sum(wrd_gram$Freq)
  }
  
  #ngram
  else if (val > 1) {
    comb_two <- ngram(large_txt, n = val, sep = " ")
    wrd_gram <- data.frame(get.phrasetable(comb_two))
    wrd_gram <- wrd_gram[-(which(str_detect(wrd_gram$ngrams, ":"))),]
    wrd_gram$prop <- wrd_gram$freq/sum(wrd_gram$freq)
  }
  
  else {print("The value is not acceptable")}
  
  return (wrd_gram)
}


################### to associate the town and census tract ID ########################
###################################################################################
spatial_locator <- function (listing) {
  #check is the files exist in the directory of dsn throw
  #error if not available with releavant message and exit the mothod
  
  list.files('K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial', pattern='\\.shp$')
  
  try(if (file.exists('K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/towns_MA.shp') == FALSE) 
    stop("Towns boundary file not available!"))
  
  try(if (file.exists('K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/comm_type.shp') == FALSE) 
    stop("Community type boundary file not available!"))
  
  try(if (file.exists('K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/census_tract.shp') == FALSE) 
    stop("Census tract boundary file not available!"))
  
  try(if (file.exists('K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/1partner_city_nhoods.shp') == FALSE) 
    stop("Level 1 neighborhood file not available"))
  
  try(if (file.exists('K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/2Bos_neighborhoods.shp') == FALSE) 
    stop("Level 2 neighborhood file not available!"))
  
  try(if (file.exists('K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/3MARKET AREAS NEW_region.shp') == FALSE) 
    stop("Level 3 neighborhood file not available!"))
  
  #read shape files of source boundaries including towns, census tracts, neighborhoods
  towns.shape <- readOGR(dsn=path.expand("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"), layer ="towns_MA")
  
  comm_type.shape <- readOGR(dsn=path.expand("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"), layer ="comm_type")
  
  tract.shape <- readOGR(dsn=path.expand("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"), layer ="census_tract")
  
  neighborhoods.shape <- readOGR(dsn=path.expand("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"), layer ="1partner_city_nhoods")
  
  bos_neighborhoods_2.shape <- readOGR(dsn=path.expand("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"), layer ="2Bos_neighborhoods")
  
  bos_neighborhoods_3.shape <- readOGR(dsn=path.expand("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial"), layer ="3MARKET AREAS NEW_region")
  
  #project the shapefiles to NAD83 
  CRS.new <- CRS( "+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000
                  +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  proj4string(towns.shape) <- CRS.new
  
  
  tract.shape <- spTransform(tract.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                              +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                              +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  comm_type.shape <-  spTransform(comm_type.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                                       +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                                       +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  neighborhoods.shape <- spTransform(neighborhoods.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                                              +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                                              +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  bos_neighborhoods_2.shape <- spTransform(bos_neighborhoods_2.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                                                          +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                                                          +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  bos_neighborhoods_3.shape <- spTransform(bos_neighborhoods_3.shape, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                                                          +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                                                          +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  
  #read lat long attributes of the listing records and create event points of WGS1984 geographic projection 
  event.Points <- SpatialPoints(data.frame(latitude = listing$latitude, longitude= listing$longitude),
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  
  #reproject the points to NAD83
  event.Points <- spTransform(event.Points, CRS("+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 
                                                +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +datum=NAD83 +units=m
                                                +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  #overlay any of the boundaries with the listing records
  pnt_towns.shape <- over(event.Points,towns.shape)
  pnt_comm_type.shape <- over(event.Points,comm_type.shape)
  pnt_tract.shape <-over(event.Points,tract.shape) 
  pnt_nei_1.shape <-over(event.Points,neighborhoods.shape) 
  pnt_nei_2.shape <-over(event.Points,bos_neighborhoods_2.shape) 
  pnt_nei_3.shape <-over(event.Points,bos_neighborhoods_3.shape) 
  
  #extract the names from each overlay
  muni <- pnt_towns.shape$town
  muni_ID <- pnt_towns.shape$town_id
  comm_type <- pnt_comm_type.shape$commtype
  ct10_id <- pnt_tract.shape$ct10_id
  tract10 <- pnt_tract.shape$tract10
  neighborhood_01 <- pnt_nei_1.shape$Nhood
  neighborhood_02 <- pnt_nei_2.shape$Name
  neighborhood_03 <- pnt_nei_3.shape$SUB_HOOD
  
  #add the boundaries to the listings
  listing <- cbind(listing, muni_ID, muni, comm_type, tract10, ct10_id, neighborhood_01, neighborhood_02,neighborhood_03)
  
  #the records with no vcommunity type are kept out of the dataset
  listing <- listing[-which(is.na(listing$comm_type)) , ]
  
  return(listing)
}


################ studio identification function/writes studio records to #####################
#################a file and returns a adds a di studio records###########################
##############################################################################################
#this function takes 5 arguments including:
#keywrdlst: the vector of keywords possibly indicate the target group
#listing: the listings which require to be processed
#name: the name of the field to be added to the table
#br: expected number of bedrooms in the bedrooms field
#numBR: the value for num_bedrooms that we want to set
#num: the threshold of #of records per neighborhood

room_analysis <- function (keywrdlst, excl_list, listings,name, br, numBR, num ){

  #tmp is the data frame which holds the whole records and chunchs of studio records will be removed from it gradually
  tmp <- listings
  name_2 <- paste(name,"_not_in_range")
  
  #create the new field with given name
  tmp[,name] <- 0
  tmp[,name_2] <- 0
  
  #look at titles and check the keywordlist
  for (i in (1:length(keywrdlst))){
    tmp[which(str_detect(tmp$title, keywrdlst[i])), name] <- 1
  }
  
  #det_recs includes the records with word studio in the title
  det_recs <- tmp[which((tmp[,name]==1)),]
  tmp <- tmp[-which((tmp[,name] == 1)),]
  
  if(empty(det_recs)==TRUE) stop("The base search failed!")
  
  rec_statistics <- Threshold_finder(det_recs, name)
  range_neighborhood <- neighborhood_stat(det_recs)
  range_comm_type <- comm_type_rent_stat(det_recs)
  range_town <- town_rent_stat(det_recs)
  
  #adjust ranges for neighborhoods
  range_neighborhood$lower_bound[which(range_neighborhood$lower_bound < rec_statistics$lower_bound)] <- rec_statistics$lower_bound
  range_neighborhood$upper_bound[which(range_neighborhood$upper_bound > rec_statistics$upper_bound)] <- rec_statistics$upper_bound
  
  neighborhood_rent <- range_neighborhood
  
  diff_neightborhoods <- as.vector(unique(neighborhood_rent$neighborhood))
  filtered <- NULL
  
  for (iter in (1:length(diff_neightborhoods))){
    #   print(iter)
    flag <- 0
    indices <- which(det_recs$neighborhood_01 == diff_neightborhoods[iter])
    target_neighborhood <- neighborhood_rent[which(neighborhood_rent$neighborhood == diff_neightborhoods[iter]),]
    recs_neighborhood <- det_recs[indices,]
    
    if(range_neighborhood$count[which(neighborhood_rent$neighborhood == diff_neightborhoods[iter])] < num){
      target_town <- toupper(as.character(neighborhood_rent$muni[which(neighborhood_rent$neighborhood == diff_neightborhoods[iter])]))
      target_neighborhood <- range_town[which(range_town$muni == target_town),]
    }
    
    if(target_neighborhood$count < num){
      target_community <- as.character(neighborhood_rent$community_type[which(neighborhood_rent$neighborhood == diff_neightborhoods[iter])])
      target_neighborhood <- range_comm_type[which(range_comm_type$community_type == target_community),]
    }
    
    if(target_neighborhood$count<num){
      target_neighborhood <- rec_statistics
    } 
    
    #detect the records with asking price which is in range
    neighborhood_rec_in_range <- recs_neighborhood[which(recs_neighborhood$ask >= target_neighborhood$lower_bound & 
                                                           recs_neighborhood$ask <= target_neighborhood$upper_bound),]
    
    #detect the records with asking price which is not in range
    neighborhood_rec_not_in_range <- recs_neighborhood[-(which(recs_neighborhood$ask >= target_neighborhood$lower_bound & 
                                                                 recs_neighborhood$ask <= target_neighborhood$upper_bound)), ]
    
    if(!empty(neighborhood_rec_not_in_range)){
      neighborhood_rec_not_in_range[which(neighborhood_rec_not_in_range$ask < target_neighborhood$lower_bound),name_2 ] <- -1
      neighborhood_rec_not_in_range[which(neighborhood_rec_not_in_range$ask > target_neighborhood$upper_bound),name_2 ] <- +1
    }
    
    #look for the rest of possible studios in tmp
    indices2 <- which(tmp$neighborhood_01 == diff_neightborhoods[iter] & tmp$bedrooms == br)
    filtered_recs <- tmp[indices2,]
    
    
    filtered_recs <- filtered_recs[which(filtered_recs$ask > target_neighborhood$lower_bound & 
                                           filtered_recs$ask < target_neighborhood$upper_bound),]
    
    if (!empty(filtered_recs)) filtered_recs <- exclude_recs(filtered_recs, excl_list)
    
    ind_lists <- c(0,0,0)
    
    if (!empty(filtered_recs)) ind_lists[1] <- 1
    if (!empty(neighborhood_rec_in_range))ind_lists[2] <-1
    if (!empty(neighborhood_rec_not_in_range))ind_lists[3] <- 1
    
    if (any(ind_lists [1:3] != 0)){
      if (ind_lists [1] == 1)
        filtered <- rbind(filtered, filtered_recs)
      if (ind_lists [2] == 1)
        filtered <- rbind(filtered, neighborhood_rec_in_range)
      if (ind_lists [3] == 1)
        filtered <- rbind(filtered, neighborhood_rec_not_in_range)
    } else {
      print(paste("No ", name, " records observed in ", diff_neightborhoods[iter]))
    }
  }
  
  #remove the recs of target neighborhoods from the studio recs
  det_recs <- det_recs[-(which(det_recs$id %in% filtered$id)),]
  
  if (length(which(tmp$id %in% filtered$id))> 0) tmp <- tmp[-(which(tmp$id %in% filtered$id)),]
  
   if (!empty(det_recs)) {
    rec_statistics <- Threshold_finder(det_recs, name)
    rec_comm_statistics <- comm_type_rent_stat(det_recs)
    rec_town_statistics <- town_rent_stat(det_recs)
    }
  
  #filter the remaining records based on updated total recs statistics
  temp <- tmp[which(tmp$bedrooms==br & tmp[,name]==0), ] 
  temp <- exclude_recs(temp, excl_list)

  if (!empty(temp)) temp[,name] <- 1
  
  #combines all the records and save it in X
  X <- NULL
  
 # if (!empty(recs_cln_in_range))  X <- rbind(X,recs_cln_in_range)
  if (!empty(det_recs)) X <- rbind(X,det_recs)
  if (!empty(filtered))  X <- rbind(X,filtered)
  if (!empty(temp)) X <- rbind(X,temp)
  
  if(any(X[,name_2] == 0)){
    inner <- X[which(X$comm_type == "Inner Core" & X[,name_2]== 0),]
    not_inner <- X[which(X$comm_type != "Inner Core" & X[,name_2]== 0),]
    inner_core_stat <- range_comm_type[which(range_comm_type$community_type == "Inner Core"),]
    
    inner[which(inner$ask < inner_core_stat$lower_bound ), name_2] <- -1
    inner[which(inner$ask > inner_core_stat$upper_bound ), name_2] <- +1
    
    not_inner[which(not_inner$ask < rec_statistics$lower_bound ), name_2] <- -1
    not_inner[which(not_inner$ask > rec_statistics$upper_bound ), name_2] <- +1
    
    X <- X[-(which(X[,name_2] == 0)),]
    X <- rbind(X,not_inner,inner)
  }

  if(!empty(X)) X[,name] = 1
  
  if (!empty(tmp)) tmp <- tmp[-which(tmp$id %in% X$id),]
  
  #create the statistics for all the detected records
  rec_statistics <- Threshold_finder(X, name)
  
  #create the statistics for all the detected records
  range_town <- town_rent_stat(X)
  
  #create the statistics for all the detected records
  range_comm_type <- comm_type_rent_stat(X)
  
  #compare the range neighborhood statistics with the total
  range_neighborhood$total_range_lower <- 0
  range_neighborhood$total_range_upper <- 0
  
  range_neighborhood$total_range_lower[which(range_neighborhood$lower_bound < rec_statistics$lower_bound)] <- -1
  range_neighborhood$total_range_upper[which(range_neighborhood$upper_bound>rec_statistics$upper_bound)] <- +1
  
  setwd("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/output")
  
  #write summary statistics of municipals and neighborhoods to file
  line=paste("THIS IS A SUMMARY STATISTICS OF ", name ," BASED ON MUNICIPALS")
  text <- paste("The lower and upper bound of the asking price for the", name," are $" , 
                round(rec_statistics$lower_bound, digits = 0) ," and $", round(rec_statistics$upper_bound, digits = 0), 
                " and the median value of ",name ,"rent for the whole MA is $",rec_statistics$median , sep = "")
  sink(paste(name,"_stat_muni.txt"), append = TRUE)
  Head <- line
  cat("**************************************************************************************\n")
  cat("\t")
  cat(Head, "\n\t\t", text)
  cat("\n**************************************************************************************\n")
  print(range_town, row.names = FALSE)
  sink()
  
  line2=paste("THIS IS A SUMMARY STATISTICS OF ",name,"S BASED ON NEIGHBORHOODS")
  Head <- line2
  sink(paste(name,"_stat_neighborhoods.txt"), append = TRUE)
  cat("**************************************************************************************\n")
  cat("\t",Head,"\n")
  cat("**************************************************************************************\n")
  print(range_neighborhood, row.names = FALSE)
  sink()
  
  
  line3=paste("THIS IS A SUMMARY STATISTICS OF ",name,"S BASED ON COMMUNITY TYPE")
  Head <-line3
  sink(paste(name,"_stat_commtype.txt"), append = TRUE)
  cat("**************************************************************************************\n")
  cat("\t", Head, "\n")
  cat("**************************************************************************************\n")
  print(range_comm_type, row.names = FALSE)
  sink()
  
  #write the results including and excluding the target to file
  tmp1 <- cbind(tmp[,1:27], tmp[name], tmp[name_2])
  X1 <- cbind(X[,1:27], X[name], X[name_2])
  
  write.csv(tmp1, paste("no_",name,"_records.csv"))
  write.csv(X1, paste(name,"s.csv"))
  write.csv(range_comm_type, paste(name,"_range_comm_type.csv"))
  write.csv(range_neighborhood, paste(name,"_range_neighborhood.csv"))
  write.csv(range_town, paste(name,"_range_muni.csv"))
  setwd("K:/DataServices/Projects/Current_Projects/rental_listings_research/r_scripts/analysis/Data_preparation/")
  
  if(!empty(tmp) & !empty(X)) {
    X$numRooms <- numBR
    tmp[,name] <- 0
    tmp <- rbind(tmp, X)
  }
  
  return (tmp)
}

#returns a vector of median/standard deviation/lower/upper bound based 
#the arguments of this function include the listing and the name of the summary stats. field
Threshold_finder <- function (listing, name){
  results <- NULL
 # listing <- det_recs
  if (!empty(listing)){
    #Set threshold to 2std dev higher and lower than median
    threshold_median <- median(listing[which(listing[,name]==1), ]$ask)
    threshold_sd <- sd(listing[which(listing[,name]==1), ]$ask)
    lower <- threshold_median - 1*threshold_sd
    upper <- threshold_median + 2*threshold_sd
    
    if(lower<=500 & !is.na(lower)) lower<- threshold_median - (0.5)*threshold_sd
    if(lower<=500 & !is.na(lower))lower<- 500
    
    result <- data.frame(median = threshold_median, sdev =threshold_sd,
                         lower_bound = lower, upper_bound = upper)
  }
  
  return(result)
}

#returns statistics of prices of a given list based on the neighborhood list
neighborhood_stat <- function(recs){
  range_neighborhood <- NULL
  
#recs<- det_recs
  if(!empty(recs)) {
    median_neightborhood <- tapply(recs$ask, recs$neighborhood_01, FUN = median)
    sd_neighborhood <- tapply(recs$ask, recs$neighborhood_01, FUN = sd)
    upper_bound_neighborhood <- median_neightborhood + 2*sd_neighborhood
    lower_bound_neighborhood <- median_neightborhood - 1*sd_neighborhood
    num_neighborhoods <- as.data.frame(table(unlist(recs$neighborhood_01)))
    
    median_neightborhood <- reshape2 :: melt(median_neightborhood)
    median_neightborhood <- as.data.frame(median_neightborhood)
    
    sd_neighborhood <- reshape2 :: melt(sd_neighborhood)
    sd_neighborhood <- as.data.frame(sd_neighborhood)
    
    upper_bound_neighborhood <- reshape2 :: melt(upper_bound_neighborhood)
    upper_bound_neighborhood <- as.data.frame(upper_bound_neighborhood)
    
    lower_bound_neighborhood <- reshape2 :: melt(lower_bound_neighborhood)
    lower_bound_neighborhood <- as.data.frame(lower_bound_neighborhood)
    
    neighborhoods_data <- read.dbf(file = "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/1partner_city_nhoods.dbf")
    neighborhoods_data <- as.data.frame(neighborhoods_data)
    
    munis <- NULL
    
    for (i in (1:length(lower_bound_neighborhood[,1]))){
      muni <- as.character(neighborhoods_data$muni[which(neighborhoods_data$Nhood == as.character(lower_bound_neighborhood$Var1[i]))])
      munis <- rbind(munis, muni[1])
    }
    
    community_type <- NULL
    comm_types <-  read.dbf(file = "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/comm_type.dbf")
    for (i in (1:length(lower_bound_neighborhood[,1]))){
      comm_type <- as.character(comm_types$commtype[which(toupper(comm_types$municipal) == toupper(munis[i]))])
      community_type <- rbind(community_type, comm_type[1])
    }
    #if lower bound is smaller than 500 compute the lower bouind using 0.5 sd
    if(length(which(lower_bound_neighborhood$value <= 500)) > 0) lower_bound_neighborhood$value[which(lower_bound_neighborhood$value <= 500)] <- median_neightborhood$value[which(lower_bound_neighborhood$value <= 500)] - (0.5) * sd_neighborhood$value[which(lower_bound_neighborhood$value <= 500)]
    #if lower bound is smaller than 500 set the lower bound to 500
    if(length(which(lower_bound_neighborhood$value <= 500)) > 0) lower_bound_neighborhood$value[which(lower_bound_neighborhood$value <= 500)] <- 500
    
    range_neighborhood <- data.frame(community_type = community_type, muni = munis , neighborhood = upper_bound_neighborhood$Var1,
                                     count = num_neighborhoods$Freq,median = median_neightborhood$value,sd = sd_neighborhood$value,
                                     lower_bound = lower_bound_neighborhood$value,upper_bound =upper_bound_neighborhood$value)
  }
  return(range_neighborhood)
}

#returns range of prices of a given list based on the community type
comm_type_rent_stat <- function(recs){
  range_comm_type <- NULL
  if(!empty(recs)){
    #create the statistics for neighborhoods including median/sd/upper and lower bound
    median_CT <- tapply(recs$ask, recs$comm_type, FUN = median)
    sd_CT <- tapply(recs$ask, recs$comm_type, FUN = sd)
    upper_bound_CT <- median_CT + 2*sd_CT
    lower_bound_CT <- median_CT - 1*sd_CT
    num_recs <- as.data.frame(table(unlist(recs$comm_type)))
    
    median_CT <- reshape2 :: melt(median_CT)
    median_CT <- as.data.frame(median_CT)
    
    sd_CT <- reshape2 :: melt(sd_CT)
    sd_CT <- as.data.frame(sd_CT)
    
    upper_bound_CT <- reshape2 :: melt(upper_bound_CT)
    upper_bound_CT <- as.data.frame(upper_bound_CT)
    
    lower_bound_CT <- reshape2 :: melt(lower_bound_CT)
    lower_bound_CT <- as.data.frame(lower_bound_CT)
    
    if(length(which(lower_bound_CT$value <= 500)) > 0) lower_bound_CT$value[which(lower_bound_CT$value <= 500)] <- median_CT$value[which(lower_bound_CT$value <= 500)] - 0.5*sd_CT$value[which(lower_bound_CT$value <= 500)]
    if(length(which(lower_bound_CT$value <= 500)) > 0) lower_bound_CT$value[which(lower_bound_CT$value <= 500)]<- 500
   
    range_comm_type <- data.frame(community_type = upper_bound_CT$Var1, count = num_recs$Freq,
                                  median = median_CT$value,sd = sd_CT$value,
                                  lower_bound = lower_bound_CT$value, upper_bound =upper_bound_CT$value)
  }
  
  return(range_comm_type)
}

#create the statistics for munis including median/sd/upper and lower bound
town_rent_stat <- function(recs){ 
  range_town <- NULL
  if(!empty(recs)){
    median_towns <- tapply(recs$ask, recs$muni, FUN = median)
    sd_towns <- tapply(recs$ask, recs$muni, FUN = sd)
    lower_bound_town <- median_towns - 1*sd_towns
    upper_bound_town <- median_towns + 2*sd_towns
    num_towns <- as.data.frame(table(unlist(recs$muni)))
    
    median_towns <- reshape2:: melt(median_towns)
    median_towns <- as.data.frame(median_towns)
    
    sd_towns <- reshape2:: melt(sd_towns)
    sd_towns <- as.data.frame(sd_towns)
    
    lower_bound_town <- reshape2:: melt(lower_bound_town)
    lower_bound_town <- as.data.frame(lower_bound_town)
    
    upper_bound_town <- reshape2:: melt(upper_bound_town)
    upper_bound_town <- as.data.frame(upper_bound_town)
    
    if(length(which(lower_bound_town$value < 500))>0) lower_bound_town$value[which(lower_bound_town$value < 500)] <- median_towns$value[which(lower_bound_town$value < 500)]- 0.5*sd_towns$value[which(lower_bound_town$value < 500)]
    if(length(which(lower_bound_town$value < 500))>0) lower_bound_town$value[which(lower_bound_town$value < 500)] <- 500
    
    community_type <- NULL
    comm_types <-  read.dbf(file = "K:/DataServices/Projects/Current_Projects/rental_listings_research/data/spatial/comm_type.dbf")
    for (i in (1:length(lower_bound_town[,1]))){
      comm_type <- as.character(comm_types$commtype[which(toupper(comm_types$municipal) == toupper(upper_bound_town$Var1[i]))])
      community_type <- rbind(community_type, comm_type[1])
    }
    
    range_town <- data.frame(community_type = community_type, muni = upper_bound_town$Var1 , count = num_towns$Freq,
                             median_muni = median_towns$value, sd_muni = sd_towns$value, lower_bound = lower_bound_town$value, 
                             upper_bound =upper_bound_town$value)
  }
  return(range_town)
}

#excludes the words/phrase in excl_list from titles of listing
exclude_recs <- function (listing, excl_list) {
  
  excl_filter <- NULL
  if(!empty(listing)){
    for (i in (1:length(excl_list))){
      
      if (any(str_detect(listing$title,excl_list[i])))
        listing <- listing[-which(str_detect(listing$title,excl_list[i])),]
      if(length(listing[,1]) == 0)
        break()
    }
  }
  
  return(listing)
}

#this functioon creats vectors of all combination of words which
#indicate that a record has "index" number of bedrooms
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

sample.DF <-function(x, percentile) {
  
  listings <- listings_unique
  size_rand <- length(listings[,1])
  random_numbers <- floor(runif(size_rand, min=0, max=101))
  listings <- cbind(listings, random_numbers)
  random_listings <- listings[which(random_numbers<= percentile),]
  random_listings$random_numbers <- NULL
  return(random_listings)
}
