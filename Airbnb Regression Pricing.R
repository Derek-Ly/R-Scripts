#install.packages("dplyr","tidyverse","ggplot2","mice","stringr","sqldf","Rserve","ggmap","lubridate","estimatr","car","caret","dummies","arsenal")
#install.packages("xlsx")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(mice)
library(stringr)
library(sqldf)
library(tidyr)
library(ggmap)
library(lubridate)
library(estimatr)
library(car)
library(caret)
library(dummies)
library(arsenal)

#Package Installation for Tableau Integration with R
library(Rserve);
Rserve()

#Set working directory to read input files
setwd("C:/R/Data")

#Verify directory path
getwd()

#Import data required for solution development
city_data <- read.csv("neighbourhoods.csv", sep=",", stringsAsFactors = FALSE, strip.white = TRUE)
reviews_data <- read.csv("reviews_dec18.csv", sep=",", stringsAsFactors = FALSE, strip.white = TRUE)
lisiting_data <- read.csv("listings_dec18.csv", sep=",", stringsAsFactors = FALSE, strip.white = TRUE)

#City file analysis
names(city_data)
str(city_data)
head(city_data,5)
tail(city_data,5)
nrow(city_data)
ncol(city_data)
summary(city_data)
sum(is.na(city_data))
city_data[!complete.cases(city_data),]

# check variables with missing data 
md.pattern(city_data)

#file contain inner melbourn cities and noth neighbourhoods
#Rename neighbourhood column to City
names(city_data)[2] <- "city"

#neighbourhood_group is 100% null. Exclude this column from dataset
city_data <- select(city_data, -c(1))


#Steps to register Google API and call to get LAT and LONG values for City
#Register Google API
try(register_google(key = "AIzaSyDBWWp6auZGU0vwoJTt142GXyWBZVr4iPI", write = TRUE), silent = TRUE)

#call to get City Long and Lattitude data.
geodata <- NULL
try(geodata <- geocode(c(city_data$city)), silent = TRUE)

#Add variables in CIty dataset
city_data$latitude <- NULL
city_data$longitude <- NULL
try(city_data$latitude <- geodata$lat, silent = TRUE)
try(city_data$longitude <- geodata$lon, silent = TRUE)


#Review file Analysis
names(reviews_data)
str(reviews_data)
head(reviews_data,5)
tail(reviews_data,5)
nrow(reviews_data)
ncol(reviews_data)
summary(reviews_data)
sum(is.na(reviews_data))
reviews_data[!complete.cases(reviews_data),]

# check variables with missing data 
md.pattern(reviews_data)

# only 7 observations missing comments value. Keeping these rows because very low percentage of full dataset. Also this will impact stats for review counts
#Changing format for date variable
reviews_data$date <- as.Date(reviews_data$date)

#Lisiting file Analysis
names(lisiting_data)
str(lisiting_data)
head(lisiting_data,5)
tail(lisiting_data,5)
nrow(lisiting_data)
ncol(lisiting_data)
summary(lisiting_data)
sum(is.na(lisiting_data))

# check variables with missing data 
md.pattern(lisiting_data)

#Remove $ from price columns
lisiting_data$price <- suppressWarnings(as.numeric(gsub("[$]","",lisiting_data$price)))
lisiting_data$weekly_price <- suppressWarnings(as.numeric(gsub("[$]","",lisiting_data$weekly_price)))
lisiting_data$monthly_price <- suppressWarnings(as.numeric(gsub("[$]","",lisiting_data$monthly_price)))
lisiting_data$security_deposit <- suppressWarnings(as.numeric(gsub("[$]","",lisiting_data$security_deposit)))
lisiting_data$cleaning_fee <- suppressWarnings(as.numeric(gsub("[$]","",lisiting_data$cleaning_fee)))
lisiting_data$extra_people <- suppressWarnings(as.numeric(gsub("[$]","",lisiting_data$extra_people)))

# Remove % from host_reponse_rate column
lisiting_data$host_response_rate <- suppressWarnings(as.numeric(gsub("[%]","",lisiting_data$host_response_rate)))

#Remove N/A test from host_acceptance_rate
lisiting_data$host_acceptance_rate <- suppressWarnings(as.numeric(gsub("[N/A]","",lisiting_data$host_acceptance_rate)))

#Remove none text from experiences_offered
lisiting_data$experiences_offered <- suppressWarnings(as.numeric(gsub("[none]","",lisiting_data$experiences_offered)))

#Remove unwanted characters from amenities column
lisiting_data$amenities <- gsub("[\\{]","",lisiting_data$amenities)
lisiting_data$amenities <- gsub("[\\}]","",lisiting_data$amenities)
lisiting_data$amenities <- gsub("[\\.]","",lisiting_data$amenities)
lisiting_data$amenities <- gsub('[\\"]',"",lisiting_data$amenities)

#Rename city column ->suburb and neighbourhood_cleansed -> city. Column names were not correct
names(lisiting_data)[names(lisiting_data)=="city"] <- "suburb"
names(lisiting_data)[names(lisiting_data) =="neighbourhood_cleansed"] <- "city"

#Converting column types because wasn't able to check NULL values without conversion
lisiting_data$license <- suppressWarnings(as.numeric(lisiting_data$license))
lisiting_data$price <- suppressWarnings(as.numeric(lisiting_data$price))
lisiting_data$weekly_price <- suppressWarnings(as.numeric(lisiting_data$weekly_price))
lisiting_data$monthly_price <- suppressWarnings(as.numeric(lisiting_data$monthly_price))
lisiting_data$security_deposit <- suppressWarnings(as.numeric(lisiting_data$security_deposit))
lisiting_data$cleaning_fee <- suppressWarnings(as.numeric(lisiting_data$cleaning_fee))
lisiting_data$extra_people <- suppressWarnings(as.numeric(lisiting_data$extra_people))
lisiting_data$host_since <- suppressWarnings(as.Date(lisiting_data$host_since))
lisiting_data$last_scraped <- suppressWarnings(as.Date(lisiting_data$last_scraped))
lisiting_data$calendar_last_scraped <- suppressWarnings(as.Date(lisiting_data$calendar_last_scraped))
lisiting_data$first_review <- suppressWarnings(as.Date(lisiting_data$first_review))
lisiting_data$last_review <- suppressWarnings(as.Date(lisiting_data$last_review))
lisiting_data$host_is_superhost <- suppressWarnings(as.factor(lisiting_data$host_is_superhost))
lisiting_data$host_has_profile_pic <- suppressWarnings(as.factor(lisiting_data$host_has_profile_pic))
lisiting_data$host_identity_verified <- suppressWarnings(as.factor(lisiting_data$host_identity_verified))
lisiting_data$is_location_exact <- suppressWarnings(as.factor(lisiting_data$is_location_exact))
lisiting_data$property_type <- suppressWarnings(as.factor(lisiting_data$property_type))
lisiting_data$room_type <- suppressWarnings(as.factor(lisiting_data$room_type))
lisiting_data$bed_type <- suppressWarnings(as.factor(lisiting_data$bed_type))
lisiting_data$instant_bookable <- suppressWarnings(as.factor(lisiting_data$instant_bookable))
lisiting_data$require_guest_profile_picture <- suppressWarnings(as.factor(lisiting_data$require_guest_profile_picture))
lisiting_data$is_business_travel_ready <- suppressWarnings(as.factor(lisiting_data$is_business_travel_ready))
lisiting_data$require_guest_phone_verification <- suppressWarnings(as.factor(lisiting_data$require_guest_phone_verification))
lisiting_data$zipcode <- as.factor(lisiting_data$zipcode)
lisiting_data$suburb <- as.factor(lisiting_data$suburb)

#Function to check observation count with NULL values for each variable
result_set <- NULL
check_null <- function(x){
  for (i in 1:ncol(x)){
    count_null <- sum(is.na(x[,i])) 
    count_null <- as.data.frame(count_null)
    count_null$var <- colnames(x)[i]
    
    result_set <- filter(rbind(result_set,count_null), count_null !=0)
  }
  result_set$pct_missing <- (round(result_set$count_null / nrow(x),4) * 100)
  return(result_set)
}

#Call function to checl null values in each variable
missing_count_new<- check_null(lisiting_data)

# create vecotr with variables that has missing values % >=92 
drop_col <- filter(missing_count_new, missing_count_new$pct_missing >=92)

#Compare data between host_listings_count, host_total_listings_count and calculated_host_listings_count variables
compare(lisiting_data$host_listings_count, lisiting_data$host_total_listings_count)
compare(lisiting_data$host_listings_count, lisiting_data$calculated_host_listings_count)

#based on comparison both host_listings_count and host_total_listings_count has same values. calculated_host_listings_count has same value but with minor difference.
#calculated_host_listings_count assume more accurate bcz calculated and ugnore scrapped listings. 
#based on compare results add columns to drop_col venctor. this will be later used to drop variables from listing dataset
drop_col_new <- data.frame(count_null= c(0,0), var=c("host_listings_count","host_total_listings_count"), pct_missing=c(0,0))

#add other columns using rbind into drop_col vector
drop_col <- rbind(drop_col, drop_col_new)

#Steps to remove variables from listing dataset
#Remove variables from listing dataset using  drop_col vector
lisiting_data_mutate <- lisiting_data[,!(names(lisiting_data) %in% drop_col$var)]


#SQL to merge different datasets into one. This dataset will be used for Tableau insight
#Use sqldf to merge all 3 datasets
tableau_dataset <- sqldf("select l.id as lisiting_id,listing_url,scrape_id,last_scraped,name,notes,transit,access,interaction,
                                 house_rules, picture_url,host_id,host_url,host_name,host_since,host_location,host_about,
                                 host_response_time,host_response_rate,host_is_superhost,host_thumbnail_url,host_picture_url,
                                 host_neighbourhood,host_verifications,host_has_profile_pic,host_identity_verified,street,
                                 neighbourhood,l.city,suburb,state,zipcode,market,smart_location,country_code,country,l.latitude,
                                 l.longitude,is_location_exact,property_type,room_type,accommodates,bathrooms,bedrooms,beds,bed_type,
                                 amenities,price,security_deposit,cleaning_fee,guests_included,extra_people,minimum_nights,
                                 maximum_nights,calendar_updated,has_availability,availability_30,availability_60,availability_90,
                                 availability_365,calendar_last_scraped,number_of_reviews,first_review,last_review,
                                 review_scores_rating,review_scores_accuracy,review_scores_cleanliness,review_scores_checkin,
                                 review_scores_communication,review_scores_location,review_scores_value,requires_license,
                                 instant_bookable,is_business_travel_ready,cancellation_policy,require_guest_profile_picture,
                                 require_guest_phone_verification,calculated_host_listings_count,
                                 c.latitude as city_latitude,c.longitude as city_longitude,
                                 r.id as review_id, r.date, r.reviewer_id, r.reviewer_name, r.comments
                            from lisiting_data_mutate l
                                 LEFT OUTER JOIN reviews_data r ON  r.listing_id = l.id
                                 LEFT OUTER JOIN city_data c ON c.city = l.city
                           where l.country = 'Australia' "
                        )

#Write cleansed and merged data in CSV file format. this will be used as source file for Tableau Project
write.csv(tableau_dataset, file="airbnb_data.csv", quote = TRUE, row.names = FALSE)


#Check record count for each property type. select top 10-15 types to create dummy variables
sqldf("select property_type, count(distinct lisiting_id) as count from tableau_dataset group by property_type order by 2 desc")

#Check record count for each room type
sqldf("select room_type, count(distinct lisiting_id) as count from tableau_dataset group by room_type order by 2 desc")

#Check amenities count against different property types
#ammenities count for house dataset
house_ammenities <- sqldf('select distinct amenities from tableau_dataset where property_type = "House" ')
house_ammenities <- strsplit(as.character(house_ammenities$amenities), ",")
house_ammenities.freq <- table(unlist(house_ammenities))
house_words_count <- cbind.data.frame(names(house_ammenities.freq),as.integer(house_ammenities.freq))
colnames(house_words_count) <- c("amenity","count")
house_words_count <- house_words_count[order(-house_words_count$count),]

#ammenities count for apartment dataset
apartment_ammenities <- sqldf('select distinct amenities from tableau_dataset where property_type = "Apartment" ')
apartment_ammenities <- strsplit(as.character(apartment_ammenities$amenities), ",")
apartment_ammenities.freq <- table(unlist(apartment_ammenities))
apartment_words_count <- cbind.data.frame(names(apartment_ammenities.freq),as.integer(apartment_ammenities.freq))
colnames(apartment_words_count) <- c("amenity","count")
apartment_words_count <- apartment_words_count[order(-apartment_words_count$count),]


#ammenities count for townhouse dataset
townhouse_ammenities <- sqldf('select distinct amenities from tableau_dataset where property_type = "Townhouse" ')
townhouse_ammenities <- strsplit(as.character(townhouse_ammenities$amenities), ",")
townhouse_ammenities.freq <- table(unlist(townhouse_ammenities))
townhouse_words_count <- cbind.data.frame(names(townhouse_ammenities.freq),as.integer(townhouse_ammenities.freq))
colnames(townhouse_words_count) <- c("amenity","count")
townhouse_words_count <- townhouse_words_count[order(-townhouse_words_count$count),]

#ammenities count for condominium dataset
condominium_ammenities <- sqldf('select distinct amenities from tableau_dataset where property_type = "Condominium" ')
condominium_ammenities <- strsplit(as.character(condominium_ammenities$amenities), ",")
condominium_ammenities.freq <- table(unlist(condominium_ammenities))
condominium_words_count <- cbind.data.frame(names(condominium_ammenities.freq),as.integer(condominium_ammenities.freq))
colnames(condominium_words_count) <- c("amenity","count")
condominium_words_count <- condominium_words_count[order(-condominium_words_count$count),]

#ammenities count for property_other dataset
property_other_ammenities <- sqldf('select distinct amenities from tableau_dataset where property_type not in ("House","Apartment","Townhouse","Condominium") ')
property_other_ammenities <- strsplit(as.character(property_other_ammenities$amenities), ",")
property_other_ammenities.freq <- table(unlist(property_other_ammenities))
property_other_words_count <- cbind.data.frame(names(property_other_ammenities.freq),as.integer(property_other_ammenities.freq))
colnames(property_other_words_count) <- c("amenity","count")
property_other_words_count <- property_other_words_count[order(-property_other_words_count$count),]

#Use sqldf to merge all 3 datasets
airbnb_mel_data <- sqldf("select distinct l.lisiting_id,l.zipcode,
                                 l.room_type,l.accommodates,l.bathrooms,l.bedrooms,l.beds,l.bed_type,
                                 l.price,l.security_deposit,l.cleaning_fee,l.guests_included,l.extra_people,
                                 l.review_scores_rating,l.city,l.suburb,l.latitude, l.longitude, l.city_latitude, l.city_longitude,
                                 case when l.property_type = 'House' then 1 else 0 end House,
                                 case when l.property_type = 'Apartment' then 1 else 0 end Apartment,
                                 case when l.property_type = 'Townhouse' then 1 else 0 end Townhouse,
                                 case when l.property_type = 'Condominium' then 1 else 0 end Condominium,
                                 case when l.property_type not in ('House','Apartment','Townhouse','Condominium') then 1 else 0 end property_other,
                                 case when l.amenities like '%Kitchen%' then 1 else 0 end amenities_kitchen,
                                 case when l.amenities like '%Essentials%' then 1 else 0 end amenities_essentials,
                                 case when l.amenities like '%Wifi%' then 1 else 0 end amenities_wifi,
                                 case when l.amenities like '%Washer%' then 1 else 0 end amenities_washer,
                                 case when l.amenities like '%Heating%' then 1 else 0 end amenities_heating,
                                 case when l.amenities like '%Smoke detector%' then 1 else 0 end amenities_smoke_detector,
                                 case when l.amenities like '%TV%' then 1 else 0 end amenities_tv,
                                 case when l.amenities like '%Hangers%' then 1 else 0 end amenities_hangers,
                                 case when l.amenities like '%Iron%' then 1 else 0 end amenities_iron,
                                 case when l.amenities like '%Shampoo%' then 1 else 0 end amenities_shampoo,
                                 case when l.amenities like '%Hair drryer%' then 1 else 0 end amenities_hair_dryer,
                                 case when l.amenities like '%Air conditioning%' then 1 else 0 end amenities_air_conditioning,
                                 case when l.amenities like '%Free parking on premises%' then 1 else 0 end amenities_free_parking_on_premises,
                                 case when l.amenities like '%Free street parking%' then 1 else 0 end amenities_free_street_parking,
                                 case when l.amenities like '%Laptop friendly workspace%' then 1 else 0 end amenities_laptop_friendly_workspace,
                                 case when l.amenities like '%Hot water%' then 1 else 0 end amenities_hot_water,
                                 case when l.amenities like '%Dryer%' then 1 else 0 end amenities_dryer,
                                 case when l.amenities like '%Family/kid friendly%' then 1 else 0 end amenities_family_kid_friendly,
                                 case when l.amenities like '%First aid kit%' then 1 else 0 end amenities_first_aid_kit,
                                 case when l.amenities like '%Refrigerator%' then 1 else 0 end amenities_refrigerator,
                                 case when l.amenities like '%Elevator%' then 1 else 0 end amenities_elevator,
                                 case when l.amenities like '%Step-free access%' then 1 else 0 end amenities_step_free_access,
                                 case when l.amenities like '%Oven%' then 1 else 0 end amenities_oven,
                                 case when l.amenities like '%Stove%' then 1 else 0 end amenities_stove,
                                 case when l.amenities like '%Dishes and silverware' then 1 else 0 end amenities_dishes_silverware,
                                 case when l.amenities like '%Gym%' then 1 else 0 end amenities_gym,
                                 case when l.amenities like '%Pool%' then 1 else 0 end amenities_pool,
                                 case when l.amenities like '%Fire extinguisher%' then 1 else 0 end amenities_fire_extinguisher,
                                 case when l.amenities like '%Bed linens%' then 1 else 0 end amenities_bed_linens,
                                 case when l.amenities like '%Microwave%' then 1 else 0 end amenities_microwave,
                                 case when l.amenities like '%Dishwasher%' then 1 else 0 end amenities_dishwasher,
                                 case when l.amenities like '%Internet%' then 1 else 0 end amenities_internet,
                                 case when l.amenities like '%Cable TV%' then 1 else 0 end amenities_cable_tv
                            from tableau_dataset l  ")

#add dummy variable for room_type and city variable
airbnb_mel_data$room_type_entire_home_apt <- ifelse(airbnb_mel_data$room_type == "Entire home/apt",1,0)
airbnb_mel_data$room_type_private_room <- ifelse(airbnb_mel_data$room_type == "Private room",1,0)
airbnb_mel_data$room_type_shared_room <- ifelse(airbnb_mel_data$room_type == "Shared room",1,0)
airbnb_mel_data$cityBayside <- ifelse(airbnb_mel_data$city=='Bayside', 1,0)
airbnb_mel_data$cityBoroondara <- ifelse(airbnb_mel_data$city=='Boroondara', 1,0)
airbnb_mel_data$cityBrimbank <- ifelse(airbnb_mel_data$city=='Brimbank', 1,0)
airbnb_mel_data$cityCardinia <- ifelse(airbnb_mel_data$city=='Cardinia', 1,0)
airbnb_mel_data$cityCasey <- ifelse(airbnb_mel_data$city=='Casey', 1,0)
airbnb_mel_data$cityDarebin <- ifelse(airbnb_mel_data$city=='Darebin', 1,0)
airbnb_mel_data$cityFrankston <- ifelse(airbnb_mel_data$city=='Frankston', 1,0)
airbnb_mel_data$cityGlenEira <- ifelse(airbnb_mel_data$city=='Glen Eira', 1,0)
airbnb_mel_data$cityGreaterDandenong <- ifelse(airbnb_mel_data$city=='Greater Dandenong', 1,0)
airbnb_mel_data$cityHobsonsBay <- ifelse(airbnb_mel_data$city=='Hobsons Bay', 1,0)
airbnb_mel_data$cityHume <- ifelse(airbnb_mel_data$city=='Hume', 1,0)
airbnb_mel_data$cityKingston <- ifelse(airbnb_mel_data$city=='Kingston', 1,0)
airbnb_mel_data$cityKnox <- ifelse(airbnb_mel_data$city=='Knox', 1,0)
airbnb_mel_data$cityManningham <- ifelse(airbnb_mel_data$city=='Manningham', 1,0)
airbnb_mel_data$cityMaribyrnong <- ifelse(airbnb_mel_data$city=='Maribyrnong', 1,0)
airbnb_mel_data$cityMaroondah <- ifelse(airbnb_mel_data$city=='Maroondah', 1,0)
airbnb_mel_data$cityMelbourne <- ifelse(airbnb_mel_data$city=='Melbourne', 1,0)
airbnb_mel_data$cityMelton <- ifelse(airbnb_mel_data$city=='Melton', 1,0)
airbnb_mel_data$cityMonash <- ifelse(airbnb_mel_data$city=='Monash', 1,0)
airbnb_mel_data$cityMooneeValley <- ifelse(airbnb_mel_data$city=='Moonee Valley', 1,0)
airbnb_mel_data$cityMoreland <- ifelse(airbnb_mel_data$city=='Moreland', 1,0)
airbnb_mel_data$cityNillumbik <- ifelse(airbnb_mel_data$city=='Nillumbik', 1,0)
airbnb_mel_data$cityPortPhillip <- ifelse(airbnb_mel_data$city=='Port Phillip', 1,0)
airbnb_mel_data$cityStonnington <- ifelse(airbnb_mel_data$city=='Stonnington', 1,0)
airbnb_mel_data$cityWhitehorse <- ifelse(airbnb_mel_data$city=='Whitehorse', 1,0)
airbnb_mel_data$cityWhittlesea <- ifelse(airbnb_mel_data$city=='Whittlesea', 1,0)
airbnb_mel_data$cityWyndham <- ifelse(airbnb_mel_data$city=='Wyndham', 1,0)
airbnb_mel_data$cityYarra <- ifelse(airbnb_mel_data$city=='Yarra', 1,0)
airbnb_mel_data$cityYarraRanges <- ifelse(airbnb_mel_data$city=='Yarra Ranges', 1,0)


#Checking counts for City and zipcode and confirming if any overlap zipcode across cities
table(airbnb_mel_data$city, airbnb_mel_data$zipcode)
table(airbnb_mel_data$city)

#Remove room_type and city original variables bcz created dummy variables 
airbnb_mel_data <- select(airbnb_mel_data, -c(room_type,city))

#Final dataset that will be used for model building
final_airbnb_data <- select (airbnb_mel_data, c(accommodates,bathrooms,bedrooms,beds,bed_type,price,security_deposit,cleaning_fee,
                                                extra_people,review_scores_rating,House,Apartment,Condominium,Townhouse,property_other,
                                                amenities_kitchen,amenities_essentials,amenities_wifi,
                                                amenities_washer,amenities_heating,amenities_smoke_detector,amenities_tv,amenities_hangers,
                                                amenities_iron,amenities_shampoo,amenities_hair_dryer,amenities_air_conditioning,
                                                amenities_free_parking_on_premises,amenities_free_street_parking,
                                                amenities_laptop_friendly_workspace,amenities_hot_water,amenities_dryer,
                                                amenities_family_kid_friendly,amenities_first_aid_kit,amenities_refrigerator,amenities_elevator,
                                                amenities_step_free_access,amenities_oven,amenities_stove,amenities_dishes_silverware,amenities_gym,
                                                amenities_pool,amenities_fire_extinguisher,amenities_bed_linens,amenities_microwave,
                                                amenities_dishwasher,amenities_internet,amenities_cable_tv,room_type_entire_home_apt,
                                                room_type_private_room,room_type_shared_room,
                                                cityBayside,cityBoroondara,cityBrimbank,cityCardinia,cityCasey,cityDarebin,cityFrankston,
                                                cityGlenEira,cityGreaterDandenong,cityHobsonsBay,cityHume,cityKingston,cityKnox,cityManningham,
                                                cityMaribyrnong,cityMaroondah,cityMelbourne,cityMelton,cityMonash,cityMooneeValley,cityMoreland,
                                                cityNillumbik,cityPortPhillip,cityStonnington,cityWhitehorse,cityWhittlesea,cityWyndham,cityYarra,
                                                cityYarraRanges,suburb))

#Impute Missing Data
#analyze new dataset
str(final_airbnb_data)

#Call function to checl null values in each variable
missing_final_airbnb_data <- check_null(final_airbnb_data)

#Converting all dummary variables factors
for(i in c(1,16:ncol(final_airbnb_data))) {
  final_airbnb_data[,i] <- as.factor(as.character(final_airbnb_data[,i]))
}

#Impute
impute_airbnb_mel_data <- mice(final_airbnb_data, m=5, blocks=c(missing_final_airbnb_data$var), 
                               defaultMethod = c("mean", "mean", "mean", "pmm", "pmm","pmm","mean"), maxit=30, seed=1)

impute_airbnb_data_comp <- complete(impute_airbnb_mel_data,3) 
summary(impute_airbnb_mel_data)

plot(impute_airbnb_data_comp)

# split dataset into train and test datasets
#Set random seed to ensure same datasets for train and test
set.seed(1)

#train and test
rowcount <- nrow(impute_airbnb_data_comp)
shuffled_airbnb_data <- impute_airbnb_data_comp[sample(rowcount),]

train_dataset <- shuffled_airbnb_data[1:round(0.7 * rowcount),]
test_dataset <- shuffled_airbnb_data[(round(0.7 * rowcount) + 1):rowcount,]


#lm model for price predication
base_lm1 <- lm(price ~ ., train_dataset)

#Check model results
summary(base_lm1)
plot(base_lm1)
ncvTest(base_lm1)

# run this model to force dataset not to be more roubst because original dataset was heteroskedasticity
robust_lm1 <- lm_robust(price ~ ., train_dataset, se_type = "HC3")
summary(robust_lm1)

#to check collinarity between beds and bedtypes. based on this we decided to drop bed_types column from the model
lm_robust(price ~beds, train_dataset, se_type = "HC3")
lm_robust(price ~bed_type, train_dataset, se_type = "HC3")

robust_lm2 <- lm_robust(price ~ accommodates+bathrooms+bedrooms+beds+security_deposit+cleaning_fee+extra_people+review_scores_rating+
                                House+Apartment+Townhouse+
                                amenities_kitchen+amenities_wifi+amenities_washer+amenities_smoke_detector+
                                amenities_tv+amenities_hangers+amenities_iron+amenities_shampoo+amenities_air_conditioning+
                                amenities_free_parking_on_premises+amenities_free_street_parking+amenities_laptop_friendly_workspace+
                                amenities_hot_water+amenities_dryer+amenities_elevator+
                                amenities_step_free_access+amenities_dishes_silverware+amenities_pool+
                                amenities_microwave+amenities_dishwasher+amenities_internet+amenities_cable_tv+
                                room_type_entire_home_apt+room_type_private_room+cityBayside+
                                cityFrankston+cityGreaterDandenong+cityHume+cityKnox+
                                cityMelton+cityNillumbik+
                                cityWhitehorse+cityWhittlesea, train_dataset, se_type = "HC3")

summary(robust_lm2)

#Add new variables for Pred, Residuals and Dataset Flag
train_dataset$predicted <- predict(robust_lm2, train_dataset)
train_dataset$dataset_fl <- 'Train'

#validate model against test dataset
pred_test <- predict(robust_lm2,test_dataset)
summary(pred_test)

#add variables to test dataset
test_dataset$dataset_fl <- 'Test'
test_dataset$predicted <- pred_test

#test & train comparisons
data.frame( R2 = R2(pred_test, test_dataset$price),
            RMSE = RMSE(pred_test, test_dataset$price),
            MAE = MAE(pred_test, test_dataset$price))


#Combine Test and train data set with predicted values for plot
post_airbnb_data <-rbind(train_dataset,test_dataset)

#Write cleansed and merged data in CSV file format. this will be used as source file for Tableau Project
write.csv(post_airbnb_data, file="plot_results.csv", quote = TRUE, row.names = FALSE)
