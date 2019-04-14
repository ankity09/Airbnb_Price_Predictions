### Libraries
install.packages("tidyverse")

library(data.table)
library(tidyverse)
library(R.utils)
### Data Import

apr18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/April_2018/listings.csv", stringsAsFactors = F)
str(apr18)

framenames <- c('jan18','feb18','mar18','apr18','may18','jun18','jul18','aug18','sep18','oct18','nov18','dec18','jan19','feb19','mar19')
df <- apr18
df <- df[,c(1,23,26:29,32:35,37,39:42,44,49:69,71:75,77,80:86,90:92,94:96)]


### removing dollar and comma ###
df$extra_people <- as.numeric(gsub("\\$", "", df$extra_people))
df$price <- gsub("\\$", "", df$price)
df$price <- as.numeric(gsub(",","",df$price))
df$security_deposit <- gsub("\\$", "", df$security_deposit)
df$security_deposit <- as.numeric(gsub(",","",df$security_deposit))
df$security_deposit <- ifelse(is.na(df$security_deposit), 0, df$security_deposit)
df$cleaning_fee <- gsub("\\$", "", df$cleaning_fee)
df$cleaning_fee <- as.numeric(gsub(",","",df$cleaning_fee))
df$cleaning_fee <- ifelse(is.na(df$cleaning_fee), 0, df$cleaning_fee)
df$host_response_rate <- gsub("\\%", "", df$host_response_rate)
df$host_response_rate <- as.numeric(df$host_response_rate)
df$host_is_superhost <- as.numeric(factor(df$host_is_superhost, levels = c("f","t"))) - 1
df$host_identity_verified <- as.numeric(factor(df$host_identity_verified, levels = c("f","t"))) - 1
df$neighbourhood_group <- factor(df$neighbourhood_group)
df$neighbourhood_group_cleansed <- factor(df$neighbourhood_group_cleansed)
df$room_type <- factor(df$room_type)
df$host_response_time <- factor(df$host_response_time, levels = c("N/A","within an hour", "within a few hours", "within a day","a few days or more"))
df$has_availability <- as.numeric(factor(df$has_availability)) 
df$property_type <- factor(df$property_type)
df$bed_type <- factor(df$bed_type)
df$weekly_price <- gsub("\\$", "", df$weekly_price)
df$weekly_price <- as.numeric(gsub(",","",df$weekly_price))
df$monthly_price <- gsub("\\$", "", df$monthly_price)
df$monthly_price <- as.numeric(gsub(",","",df$monthly_price))
df$minimum_nights <- as.numeric(df$minimum_nights)
df$maximum_nights <- as.numeric(df$maximum_nights)
df$neighbourhood <- factor(df$neighbourhood)
df$neighbourhood_cleansed <- factor(df$neighbourhood_cleansed)
df$host_verifications <- factor(df$host_verifications)
df$zipcode <- factor(df$zipcode)
df$host_neighbourhood <- factor(df$host_neighbourhood)
df$host_since <- as.Date(df$host_since)
df$host_listings_count <- as.numeric(df$host_listings_count)
df$host_total_listings_count <- as.numeric(df$host_total_listings_count)
df$accommodates <- as.numeric(df$accommodates)
df$bathrooms <- as.numeric(df$bathrooms)
df$bedrooms <- as.numeric(df$bedrooms)
df$beds <- as.numeric(df$beds)
df$guests_included <- as.numeric(df$guests_included)
df$calculated_host_listings_count <- as.numeric(df$calculated_host_listings_count)
df$is_location_exact <- as.numeric(factor(df$is_location_exact, levels = c("f","t"))) - 1
df$is_location_exact <- as.factor(df$is_location_exact)
df$cancellation_policy <- as.factor(df$cancellation_policy)
df$require_guest_phone_verification <- as.numeric(factor(df$require_guest_phone_verification, levels = c("f","t"))) - 1
df$require_guest_phone_verification <- as.factor(df$require_guest_phone_verification)
df$is_business_travel_ready <- as.numeric(factor(df$is_business_travel_ready, levels = c("f","t"))) - 1
df$is_business_travel_ready <- as.factor(df$is_business_travel_ready)
df$instant_bookable <- as.numeric(factor(df$instant_bookable, levels = c("f","t"))) - 1
df$instant_bookable <- as.factor(df$instant_bookable)


 
str(df)

