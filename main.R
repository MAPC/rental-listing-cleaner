# Created by: Pariya Pourmohammadi
# Date: June/09/17
# main method to call the functions

######function calls #############
getwd()
setwd("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/")

raw_listings <- read.csv("listings_20170324.csv", 
                         header=FALSE, col.names=c("id","ask","bedrooms","title","address","post_at","created_at",
                                                   "updated_at","source_id","survey_id","latitude","longitude"))

setwd("K:/DataServices/Projects/Current_Projects/rental_listings_research/r_scripts/analysis/Data_preparation/")

source("Data_prep.R")

listings_unique <- clean_raw_listing(raw_listings)
listingDup <- Dupllicate_finder(listings_unique)
listings_unique <- remove_duplicates(listings_unique,listingDup)
listing_unique_asli <- listings_unique

listings_unique <- listing_unique_asli

#flag all the records with #of bedrooms -1 to decide about each record
listings_unique$numRooms <- -1

#construction of incuding and excluding lists
keywrdlst_studio <- c(" STUDIO ")
keywrdlst_one_br <- comb(1)
keywrdlst_two_br <- comb(2)
keywrdlst_three_br <- comb(3)
keywrdlst_four_br <- comb(4)
keywrdlst_five_br <- comb(5)
keywrdlst_six_br <- comb(6)
keywrdlst_seven_br <- comb(7)
keywrdlst_eight_br <- comb(8) 
keywrdlst_nine_br<- comb(9)
keywrdlst_ten_br<- comb(10)

excllst_studio <- c(keywrdlst_one_br,
                    keywrdlst_two_br,
                    keywrdlst_three_br,
                    keywrdlst_four_br,
                    keywrdlst_five_br ,
                    keywrdlst_six_br,
                    keywrdlst_seven_br,
                    keywrdlst_eight_br,
                    keywrdlst_nine_br,
                    keywrdlst_ten_br)

excllst_one_br <- c(keywrdlst_studio,
                    keywrdlst_two_br,
                    keywrdlst_three_br,
                    keywrdlst_four_br,
                    keywrdlst_five_br ,
                    keywrdlst_six_br,
                    keywrdlst_seven_br,
                    keywrdlst_eight_br,
                    keywrdlst_nine_br,
                    keywrdlst_ten_br)


excllst_two_br <- c(keywrdlst_studio,
                    keywrdlst_one_br,
                    keywrdlst_three_br,
                    keywrdlst_four_br,
                    keywrdlst_five_br ,
                    keywrdlst_six_br,
                    keywrdlst_seven_br,
                    keywrdlst_eight_br,
                    keywrdlst_nine_br,
                    keywrdlst_ten_br)

excllst_three_br <- c(keywrdlst_studio,
                      keywrdlst_one_br,
                      keywrdlst_two_br,
                      keywrdlst_four_br,
                      keywrdlst_five_br ,
                      keywrdlst_six_br,
                      keywrdlst_seven_br,
                      keywrdlst_eight_br,
                      keywrdlst_nine_br,
                      keywrdlst_ten_br)

excllst_four_br  <- c(keywrdlst_studio,
                      keywrdlst_one_br,
                      keywrdlst_two_br,
                      keywrdlst_three_br,
                      keywrdlst_five_br ,
                      keywrdlst_six_br,
                      keywrdlst_seven_br,
                      keywrdlst_eight_br,
                      keywrdlst_nine_br,
                      keywrdlst_ten_br)

excllst_five_br  <- c(keywrdlst_studio,
                      keywrdlst_one_br,
                      keywrdlst_two_br,
                      keywrdlst_three_br,
                      keywrdlst_four_br ,
                      keywrdlst_six_br,
                      keywrdlst_seven_br ,
                      keywrdlst_eight_br,
                      keywrdlst_nine_br,
                      keywrdlst_ten_br)

excllst_six_br  <- c(keywrdlst_studio,
                     keywrdlst_one_br,
                     keywrdlst_two_br,
                     keywrdlst_three_br,
                     keywrdlst_four_br ,
                     keywrdlst_five_br,
                     keywrdlst_seven_br,
                     keywrdlst_eight_br,
                     keywrdlst_nine_br,
                     keywrdlst_ten_br)

excllst_seven_br <- c(keywrdlst_studio,
                      keywrdlst_one_br,
                      keywrdlst_two_br,
                      keywrdlst_three_br,
                      keywrdlst_four_br ,
                      keywrdlst_five_br,
                      keywrdlst_six_br,
                      keywrdlst_eight_br,
                      keywrdlst_nine_br,
                      keywrdlst_ten_br)

excllst_eight_br <- c(keywrdlst_studio,
                      keywrdlst_one_br,
                      keywrdlst_two_br,
                      keywrdlst_three_br,
                      keywrdlst_four_br ,
                      keywrdlst_five_br,
                      keywrdlst_six_br ,
                      keywrdlst_seven_br,
                      keywrdlst_nine_br ,
                      keywrdlst_ten_br )

excllst_nine_br <- c(keywrdlst_studio,
                     keywrdlst_one_br,
                     keywrdlst_two_br,
                     keywrdlst_three_br,
                     keywrdlst_four_br ,
                     keywrdlst_five_br,
                     keywrdlst_six_br ,
                     keywrdlst_seven_br,
                     keywrdlst_eight_br ,
                     keywrdlst_ten_br )

excllst_ten_br <- c(keywrdlst_studio,
                    keywrdlst_one_br,
                    keywrdlst_two_br,
                    keywrdlst_three_br,
                    keywrdlst_four_br ,
                    keywrdlst_five_br,
                    keywrdlst_six_br ,
                    keywrdlst_seven_br,
                    keywrdlst_eight_br ,
                    keywrdlst_nine_br )

#studio Identification function call
name_studio <-'studio'
br_studio <- 0
numBR_studio <- 0
num <- 20
listings_unique <- room_analysis(keywrdlst_studio, excllst_studio,listings_unique,name_studio, br_studio, numBR_studio, num )

#one br Identification function call
name_one <- 'one_bedroom'
br_one <- 1
numBR_one <- 1
listings_unique <- room_analysis(keywrdlst_one_br, excllst_one_br,listings_unique, name_one, br_one, numBR_one, num )

#two br Identification function call
name_two <- 'two_bedroom'
br_two <- 2
numBR_two <- 2
listings_unique <- room_analysis(keywrdlst_two_br, excllst_two_br,listings_unique, name_two, br_two, numBR_two, num )

#three br Identification function call
name_three <- 'three_bedroom'
br_three <- 3
numBR_three  <- 3
listings_unique <- room_analysis(keywrdlst_three_br, excllst_three_br,listings_unique, name_three, br_three, numBR_three, num )

#four br Identification function call
name_four <- 'four_bedroom'
br_four <- 4
numBR_four <- 4
listings_unique <- room_analysis(keywrdlst_four_br, excllst_four_br,listings_unique, name_four, br_four, numBR_four, num )

#five br Identification function call
name_five <-'five_bedroom'
br_five <- 5
numBR_five <- 5
listings_unique <- room_analysis(keywrdlst_five_br, excllst_five_br,listings_unique, name_five, br_five, numBR_five, num )

#six br Identification function call
name_six <- 'six_bedroom'
br_six  <- 6
numBR_six  <- 6
listings_unique <- room_analysis(keywrdlst_six_br, excllst_six_br,listings_unique, name_six , br_six , numBR_six , num )

#seven br Identification function call
name_seven <- 'seven_bedroom'
br_seven <- 7
numBR_seven <- 7
listings_unique <- room_analysis(keywrdlst_seven_br, excllst_seven_br,listings_unique, name_seven, br_seven, numBR_seven, num )

#eight br Identification function call
name_eight <- 'eight_bedroom'
br_eight  <- 8
numBR_eight  <- 8
listings_unique <- room_analysis(keywrdlst_eight_br, excllst_eight_br,listings_unique, name_eight, br_eight, numBR_eight, num )

#nine br Identification function call
name_nine <- 'nine_bedroom'
br_nine <- 9
numBR_nine <- 9
listings_unique <- room_analysis(keywrdlst_nine_br, excllst_nine_br,listings_unique, name_nine, br_nine, numBR_nine, num )

#ten br Identification function call
name_ten <- 'ten_bedroom'
br_ten <- 10
numBR_ten <- 10
listings_unique <- room_analysis(keywrdlst_ten_br, excllst_ten_br,listings_unique, name_ten, br_ten, numBR_ten, num )

########################frequency table of records##################################
table_towns <- data.frame(table(unlist(listings_unique$muni)))
asking_towns <-reshape2:: melt(unlist(tapply(listings_unique$ask, listings_unique$muni, FUN = median)))
table_towns <- merge(table_towns,asking_towns,all = TRUE, by ="Var1")
names(table_towns) <- c("muni", "frequency", "median asking price")

table_census_tracts <- data.frame(table(unlist(listings_unique$ct10_id)))
asking_census_tracts<-reshape2:: melt(unlist(tapply(listings_unique$ask, listings_unique$ct10_id, FUN = median)))
table_census_tracts <- merge(table_census_tracts,asking_census_tracts,all = TRUE, by ="Var1")
names(table_census_tracts) <- c("CT10", "frequency", "median asking price")

table_neighborhoods1 <- data.frame(table(unlist(listings_unique$neighborhood_01)))
asking_neighborhoods1<-reshape2:: melt(unlist(tapply(listings_unique$ask, listings_unique$neighborhood_01, FUN = median)))
table_neighborhoods1 <- merge(table_neighborhoods1,asking_neighborhoods1,all = TRUE, by ="Var1")
names(table_neighborhoods1) <- c("neighborhoods1", "frequency", "median asking price")

table_neighborhoods2 <- data.frame(table(unlist(listings_unique$neighborhood_02)))
asking_neighborhoods2<-reshape2:: melt(unlist(tapply(listings_unique$ask, listings_unique$neighborhood_02, FUN = median)))
table_neighborhoods2 <- merge(table_neighborhoods2,asking_neighborhoods2,all = TRUE, by ="Var1")
names(table_neighborhoods2) <- c("neighborhoods2", "frequency", "median asking price")

table_neighborhoods3 <- data.frame(table(unlist(listings_unique$neighborhood_03)))
asking_neighborhoods3<-reshape2:: melt(unlist(tapply(listings_unique$ask, listings_unique$neighborhood_03, FUN = median)))
table_neighborhoods3 <- merge(table_neighborhoods3,asking_neighborhoods3,all = TRUE, by ="Var1")
names(table_neighborhoods3) <- c("neighborhoods3", "frequency", "median asking price")



carlos_file <- read.csv("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/carlos/170726_padmapper_sample_2.csv")
y = carlos_file[ , c("id","fwd_geolocated", "rev_geolocated",'latitude_merge','longitude_merge','joint_addresses_merge','mapzen_geolocated','mapzen_confidence')] 

listings_unique <- merge(listings_unique, y, by="id",all.x = TRUE)
listings_sample <- sample.DF(x = listings_unique, percentile = 10)

##########################   write results to file   #################################

setwd("K:/DataServices/Projects/Current_Projects/rental_listings_research/data/output/")

write.csv(listingDup, "similar_records.csv")

listings_unique$cl_title <- NULL
write.csv(listings_unique, "listings_unique.csv")

write.csv(table_towns, "table_towns.csv")
write.csv(table_census_tracts, "table_census_tracts.csv")
write.csv(table_neighborhoods1, "table_neighborhoods1.csv")
write.csv(table_neighborhoods2, "table_neighborhoods2.csv")
write.csv(table_neighborhoods3, "table_neighborhoods3.csv")

write.csv(listings_sample, "listings_sample.csv")

                 