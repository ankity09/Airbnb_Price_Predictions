########################################                                                                         
#    Project: Airbnb Price Prediction
#    Group#: 6
#    Script: NYCAirbnb.R
#    Description: Ingest,Clean,Preprocess
#                 and Joins datasets
########################################



######################                                                                         
#    Package                                                     
#  Installation                                                                      
######################
install.packages("tidyverse")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("choroplethrZip")
install.packages("GGally")
install.packages("lubridate")
install.packages("zoo")
install.packages("ggmap")
install.packages("stringr")
install.packages("zipcode")
install.packages("leaflet")
install.packages("extracat")
install.packages("gridExtra")
install.packages("sparklyr")

######################                                                                         
#    Libraries                                                     
#                                                                        
######################
library(devtools)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(GGally)
library(lubridate)
library(zoo)
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
library(glmnet)
library(sparklyr)
library(sentimentr)


######################                                                                         
#    Ingesting Dataset                                                    
#                                                                         
######################     
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

jan18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/January_2018/reviews.csv", stringsAsFactors = F)
feb18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/February_2018/reviews.csv", stringsAsFactors = F)
mar18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/March_2018/reviews.csv", stringsAsFactors = F)
apr18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/April_2018/reviews.csv", stringsAsFactors = F)
may18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/May_2018/reviews.csv", stringsAsFactors = F)
jun18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/June_2018/reviews.csv", stringsAsFactors = F)
jul18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/July_2018/reviews.csv", stringsAsFactors = F)
aug18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/August_2018/reviews.csv", stringsAsFactors = F)
sep18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/September_2018/reviews.csv", stringsAsFactors = F)
oct18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/October_2018/reviews.csv", stringsAsFactors = F)
nov18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/November_2018/reviews.csv", stringsAsFactors = F)
dec18.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/December_2018/reviews.csv", stringsAsFactors = F)
jan19.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/January_2019/reviews.csv", stringsAsFactors = F)
feb19.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/February_2019/reviews.csv", stringsAsFactors = F)
mar19.reviews <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/March_2019/reviews.csv", stringsAsFactors = F)



######################                                                                         
#    Clean function for                                                     
#    Visualizations                                                                      
######################

viz.clean <- function(x)
  {
    df <- x
    df$amenities <- as.character(df$amenities)
    df <- df %>% mutate(total_amenties= ifelse(nchar(amenities)>2, str_count(amenities, ',')+1, 0))
    df <- df %>% select("id" ,"host_name" ,"host_since" ,"host_response_time" ,"host_response_rate" ,"host_is_superhost" ,"host_neighbourhood" ,"host_listings_count" ,"host_total_listings_count" ,"host_identity_verified" ,
                        "neighbourhood" ,"neighbourhood_cleansed","neighbourhood_group_cleansed" ,"city" ,"zipcode" ,"latitude" ,"longitude" ,"is_location_exact" ,"property_type" ,"room_type" ,"accommodates" ,"bathrooms" ,"bedrooms" ,
                        "beds" ,"bed_type" ,"price" ,"security_deposit" ,"cleaning_fee" ,"guests_included" ,"extra_people" ,"minimum_nights" ,"maximum_nights" ,"availability_30" ,"availability_60" ,"availability_90","availability_365" ,
                        "number_of_reviews" ,"review_scores_rating" ,"review_scores_accuracy" ,"review_scores_cleanliness" ,"review_scores_checkin" ,"review_scores_communication" ,"review_scores_location" ,"review_scores_value" ,
                        "instant_bookable" ,"is_business_travel_ready" ,"cancellation_policy" ,"require_guest_phone_verification" ,"calculated_host_listings_count" ,"reviews_per_month", "total_amenities")
    df$host_name <- as.character(df$host_name)
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
    df$host_is_superhost <- ifelse(is.na(df$host_is_superhost), 0, df$host_is_superhost)
    df$host_identity_verified <- as.numeric(factor(df$host_identity_verified, levels = c("f","t"))) - 1
    df$neighbourhood_group <- factor(df$neighbourhood_group)
    df$neighbourhood_group_cleansed <- factor(df$neighbourhood_group_cleansed)
    df$room_type <- factor(df$room_type)
    df$host_response_time <- factor(df$host_response_time, levels = c("N/A","within an hour", "within a few hours", "within a day","a few days or more"))
    df$property_type <- factor(df$property_type)
    df$bed_type <- factor(df$bed_type)
    df$minimum_nights <- as.numeric(df$minimum_nights)
    df$maximum_nights <- as.numeric(df$maximum_nights)
    df$neighbourhood <- factor(df$neighbourhood)
    df$neighbourhood_cleansed <- factor(df$neighbourhood_cleansed)
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


######################                                                                         
#    Clean function for                                                     
#    Predictive Models                                                                      
######################

pred.clean <- function(x)
  {
  df <- x
  df$amenities <- as.character(df$amenities)
  df <- df %>% mutate(total_amenities= ifelse(nchar(amenities)>2, str_count(amenities, ',')+1, 0))
  df <- df %>% select("id","host_is_superhost" ,"host_response_time",
                      "neighbourhood_group_cleansed","is_location_exact" ,"property_type" ,"room_type" ,"accommodates" ,"bathrooms" ,"bedrooms" ,
                      "price" ,"security_deposit" ,"cleaning_fee" ,"guests_included" ,"extra_people"  ,"availability_30" ,
                      "number_of_reviews" ,"review_scores_rating" ,"review_scores_accuracy" ,"review_scores_cleanliness" ,"review_scores_checkin" ,"review_scores_communication" ,"review_scores_location" ,"review_scores_value" ,
                      "cancellation_policy" , "total_amenities")
  df$price <- gsub("\\$", "", df$price)
  df$price <- as.numeric(gsub(",","",df$price))
  df$security_deposit <- gsub("\\$", "", df$security_deposit)
  df$security_deposit <- as.numeric(gsub(",","",df$security_deposit))
  df$security_deposit <- ifelse(is.na(df$security_deposit), 0, df$security_deposit)
  df$cleaning_fee <- gsub("\\$", "", df$cleaning_fee)
  df$cleaning_fee <- as.numeric(gsub(",","",df$cleaning_fee))
  df$cleaning_fee <- ifelse(is.na(df$cleaning_fee), 0, df$cleaning_fee)
  df$bedrooms <- ifelse(is.na(df$bedrooms), mean(df$bedrooms,na.rm=TRUE), df$bedrooms)
  df$bathrooms <- ifelse(is.na(df$bathrooms), mean(df$bathrooms,na.rm=TRUE), df$bathrooms)
  df$review_scores_rating <- ifelse(is.na(df$review_scores_rating), mean(df$review_scores_rating,na.rm=TRUE), df$review_scores_rating)
  df$review_scores_accuracy <- ifelse(is.na(df$review_scores_accuracy), mean(df$review_scores_accuracy,na.rm=TRUE), df$review_scores_accuracy)
  df$review_scores_cleanliness <- ifelse(is.na(df$review_scores_cleanliness), mean(df$review_scores_cleanliness,na.rm=TRUE), df$review_scores_cleanliness)
  df$review_scores_checkin <- ifelse(is.na(df$review_scores_checkin), mean(df$review_scores_checkin,na.rm=TRUE), df$review_scores_checkin)
  df$review_scores_communication <- ifelse(is.na(df$review_scores_communication), mean(df$review_scores_communication,na.rm=TRUE), df$review_scores_communication)
  df$review_scores_location <- ifelse(is.na(df$review_scores_location), mean(df$review_scores_location,na.rm=TRUE), df$review_scores_location)
  df$review_scores_value <- ifelse(is.na(df$review_scores_value), mean(df$review_scores_value,na.rm=TRUE), df$review_scores_value)
  df$host_is_superhost <- as.numeric(factor(df$host_is_superhost, levels = c("f","t"))) - 1
  df$host_is_superhost <- ifelse(is.na(df$host_is_superhost), 0, df$host_is_superhost)
  df$neighbourhood_group_cleansed <- factor(df$neighbourhood_group_cleansed)
  df$room_type <- factor(df$room_type)
  df$host_response_time <- factor(df$host_response_time, levels = c("N/A","within an hour", "within a few hours", "within a day","a few days or more"))
  df$property_type <- factor(df$property_type)
  df$accommodates <- as.numeric(df$accommodates)
  df$bathrooms <- as.numeric(df$bathrooms)
  df$bedrooms <- as.numeric(df$bedrooms)
  df$guests_included <- as.numeric(df$guests_included)
  df$is_location_exact <- as.numeric(factor(df$is_location_exact, levels = c("f","t"))) - 1
  df$is_location_exact <- as.factor(df$is_location_exact)
  df$cancellation_policy <- as.factor(df$cancellation_policy)
  df$cancellation_policy <- factor(df$cancellation_policy)
  df$property_type <- as.factor(df$property_type)
  x <- df
}

######################                                                                         
#    Sentiment Analysis                                                     
#    on Reviews                                                                      
######################

sentimentanalysis <- function(x)
{
  x$comments <- as.character(x$comments)
  sentiment = sentiment_by(x$comments)
  sentiment$listing_id = x$listing_id
  gradedsentiment <- sentiment %>%
    group_by(listing_id = listing_id) %>%
    summarise(avg_sent = mean(ave_sentiment, na.rm = T))
  return(gradedsentiment)
}

######################                                                                         
#    Function calls for                                                     
#    Predictive Models                                                                      
######################

pred.jan18 <- pred.clean(jan18)
pred.feb18 <- pred.clean(feb18)
pred.mar18 <- pred.clean(mar18)
pred.apr18 <- pred.clean(apr18)
pred.may18 <- pred.clean(may18)
pred.jun18 <- pred.clean(jun18)
pred.jul18 <- pred.clean(jul18)
pred.aug18 <- pred.clean(aug18)
pred.sep18 <- pred.clean(sep18)
pred.oct18 <- pred.clean(oct18)
pred.nov18 <- pred.clean(nov18)
pred.dec18 <- pred.clean(dec18)
pred.jan19 <- pred.clean(jan19)
pred.feb19 <- pred.clean(feb19)
pred.mar19 <- pred.clean(mar19)

######################                                                                         
#    Function calls for                                                     
#    Visualization 
######################

viz.jan18 <- viz.clean(jan18)
viz.feb18 <- viz.clean(feb18)
viz.mar18 <- viz.clean(mar18)
viz.apr18 <- viz.clean(apr18)
viz.may18 <- viz.clean(may18)
viz.jun18 <- viz.clean(jun18)
viz.jul18 <- viz.clean(jul18)
viz.aug18 <- viz.clean(aug18)
viz.sep18 <- viz.clean(sep18)
viz.oct18 <- viz.clean(oct18)
viz.nov18 <- viz.clean(nov18)
viz.dec18 <- viz.clean(dec18)
viz.jan19 <- viz.clean(jan19)
viz.feb19 <- viz.clean(feb19)
viz.mar19 <- viz.clean(mar19)


######################                                                                         
#    Function calls for                                                     
#    Sentiment Analysis 
######################

senti.jan18.reviews <- sentimentanalysis(jan18.reviews)
senti.feb18.reviews <- sentimentanalysis(feb18.reviews)
senti.mar18.reviews <- sentimentanalysis(mar18.reviews)
senti.apr18.reviews <- sentimentanalysis(apr18.reviews)
senti.may18.reviews <- sentimentanalysis(may18.reviews)
senti.jun18.reviews <- sentimentanalysis(jun18.reviews)
senti.jul18.reviews <- sentimentanalysis(jul18.reviews)
senti.aug18.reviews <- sentimentanalysis(aug18.reviews)
senti.sep18.reviews <- sentimentanalysis(sep18.reviews)
senti.oct18.reviews <- sentimentanalysis(oct18.reviews)
senti.nov18.reviews <- sentimentanalysis(nov18.reviews)
senti.dec18.reviews <- sentimentanalysis(dec18.reviews)
senti.jan19.reviews <- sentimentanalysis(jan19.reviews)
senti.feb19.reviews <- sentimentanalysis(fev19.reviews)
senti.mar19.reviews <- sentimentanalysis(mar19.reviews)

###############

#######################################                                                                         
#    Bind All Visualization DataFrames                                                      
#    Write Out Dataset
#######################################

viz.data <- bind_rows(viz.jan18,viz.feb18,viz.mar18,viz.apr18,viz.may18,viz.jun18,viz.jul18,viz.aug18,viz.sep18,viz.oct18,viz.nov18,viz.dec18,viz.jan19,viz.feb19,viz.mar19)
write.csv(viz.data, file = "airbnb_visualization_data.csv")


###############################                                                                        
#   Bind & Join 2018 Prediction 
#   and Sentiment Datasets
#   
#   Write out Dataset
###############################

pred.data <- bind_rows(pred.jan18,pred.feb18,pred.mar18,pred.apr18,pred.may18,pred.jun18,pred.jul18,pred.aug18,pred.sep18,pred.oct18,pred.nov18,pred.dec18)
senti.data <- bind_rows(senti.jan18.reviews,senti.feb18.reviews,senti.mar18.reviews,senti.apr18.reviews,senti.may18.reviews,senti.jun18.reviews,senti.jul18.reviews,senti.aug18.reviews,senti.sep18.reviews,senti.oct18.reviews,senti.nov18.reviews,senti.dec18.reviews)

prediction.data <- full_join(pred.data,senti.data, by = c("id","listing_id"))
write.csv(prediction.data, file = "airbnb_prediction_data.csv")




