### Libraries
install.packages("fread")
install.packages("R.utils")
install.packages("devtools")
install.packages("tidyverse")

library(devtools)
devtools::install_github("airbnb/Rbnb")

library(Rbnb)
library(data.table)
library(dplyr)
library(R.utils)
### Data Import
apr18 <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/April_2018/listings.csv", stringsAsFactors = F)
str(apr18)

framenames <- c('jan18','feb18','mar18','apr18','may18','jun18','jul18','aug18','sep18','oct18','nov18','dec18','jan19','feb19','mar19')
df <- apr18
df <- df[,c(1,23,26:29,32:35,37,39:42,44,46,49:69,71:75,77,80:86,90:92,94:96)]
str(df)

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

cleaning <- function(df){
  df$extra_people <- as.numeric(gsub("\\$", "", df$extra_people))
  df$price <- gsub("\\$", "", df$price)
  df$price <- as.numeric(gsub(",","",df$price))
  df$security_deposit <- gsub("\\$", "", df$security_deposit)
  df$security_deposit <- as.numeric(gsub(",","",df$security_deposit))
  df$security_deposit <- ifelse(is.na(df$security_deposit), 0, df$security_deposit)
  df$cleaning_fee <- gsub("\\$", "", df$cleaning_fee)
  df$cleaning_fee <- as.numeric(gsub(",","",df$cleaning_fee))
  df$cleaning_fee <- ifelse(is.na(df$cleaning_fee), 0, df$cleaning_fee)
}

cleaning(df)

### 
head(df)
