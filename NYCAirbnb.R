### Libraries
install.packages("fread")
install.packages("R.utils")
install.packages("devtools")
remove.packages("devtools")
library(devtools)
devtools::install_github("airbnb/Rbnb")

library(Rbnb)
library(data.table)
library(R.utils)
### Data Import
dlistings <- read.csv("/Users/ankityadav/Desktop/Spring_2019/DMPA/Project/Data/April_2018/listings.csv", stringsAsFactors = F)
str(dlistings)

df <- dlistings
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
### 

