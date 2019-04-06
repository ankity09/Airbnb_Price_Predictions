### Libraries
install.packages("fread")
install.packages("R.utils")
library(data.table)
library(R.utils)
### Data Import
dlistings <- read.csv("listings.csv", stringsAsFactors = F)
str(dlistings)

df <- dlistings
df <- df[,c(1,23,26:29,32:35,37,39:42,44,46,49:69,71:75,77,80:86,90:92,94:96)]

str(df)
### removing dollar and comma
df$extra_people <- as.numeric(gsub("\\$", "", df$extra_people))
df$price <- as.numeric(gsub("\\$", "", df$price))
###
