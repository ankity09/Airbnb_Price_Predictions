### Libraries
install.packages("tidyverse")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("choroplethrZip")
install.packages("GGally")
install.packages("lubridate")
install.packages("zoo")
#install.packages("scales")
install.packages("ggmap")
#install.packages("scales")
install.packages("stringr")
install.packages("zipcode")
install.packages("leaflet")
install.packages("extracat")
install.packages("gridExtra")

library(devtools)
#install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(GGally)
library(lubridate)
library(zoo)
library(scales)
library(ggmap)
library(scales)
library(stringr)
library(zipcode)
library(leaflet)
library(extracat)
library(gridExtra)
library(data.table)
library(tidyverse)
library(R.utils)
### Data Import

jan18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/January_2018/listings.csv", stringsAsFactors = F)
feb18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/February_2018/listings.csv", stringsAsFactors = F)
mar18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/March_2018/listings.csv", stringsAsFactors = F)
apr18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/April_2018/listings.csv", stringsAsFactors = F)
may18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/May_2018/listings.csv", stringsAsFactors = F)
jun18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/June_2018/listings.csv", stringsAsFactors = F)
jul18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/July_2018/listings.csv", stringsAsFactors = F)
aug18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/August_2018/listings.csv", stringsAsFactors = F)
sep18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/September_2018/listings.csv", stringsAsFactors = F)
oct18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/October_2018/listings.csv", stringsAsFactors = F)
nov18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/November_2018/listings.csv", stringsAsFactors = F)
dec18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/December_2018/listings.csv", stringsAsFactors = F)
jan19 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/January_2019/listings.csv", stringsAsFactors = F)
feb19 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/February_2019/listings.csv", stringsAsFactors = F)
mar19 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/March_2019/listings.csv", stringsAsFactors = F)

data <- data.frame("Names" = c('jan18','feb18','mar18','apr18'))
framenames <- c('jan18','feb18','mar18','apr18')
#framenames <- c('jan18','feb18','mar18','apr18','may18','jun18','jul18','aug18','sep18','oct18','nov18','dec18','jan19','feb19','mar19')

df$is_business_travel_ready <- factor(df$is_business_travel_ready)


### removing dollar and comma ###
clean <- function(x){
    df <- x
    df <- df[,c(1,5,22:23,26:29,32:35,37,39:42,44,49:69,71:75,77,80:86,90:92,94:96)]  
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
    df$is_business_travel_ready <- as.factor(df$is_business_travel_ready)
    df$is_business_travel_ready <- as.numeric(factor(df$is_business_travel_ready, levels = c("f","t"))) - 1
    df$instant_bookable <- as.numeric(factor(df$instant_bookable, levels = c("f","t"))) - 1
    df$instant_bookable <- as.factor(df$instant_bookable)
    df$cancellation_policy <- factor(df$cancellation_policy)
    x <- df
}

jan18 <- clean(jan18)
feb18 <- clean(feb18)
mar18 <- clean(mar18)
apr18 <- clean(apr18)
may18 <- clean(may18)
jun18 <- clean(jun18)
jul18 <- clean(jul18)
aug18 <- clean(aug18)
sep18 <- clean(sep18)
oct18 <- clean(oct18)
nov18 <- clean(nov18)
dec18 <- clean(dec18)
# Issues with following datasets
jan19 <- clean(jan19)
feb19 <- clean(feb19)
mar19 <- clean(mar19)
###############
str(jan18)


data <- bind_rows(jan18,feb18,mar18,apr18,may18,jun18,jul18,aug18,sep18,oct18,nov18,dec18)
str(data)
write.csv(data, file = "final_data.csv")




linear <- lm(price ~., data = data)
