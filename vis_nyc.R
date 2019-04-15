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


leaflet(df) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions(),popup = paste0("<b> Name: </b>", df$name , "<br/><b> Host Name: </b>", df$host_name, "<br> <b> Price: </b>", df$price, "<br/><b> Room Type: </b>", df$room_type, "<br/><b> Property Type: </b>", df$property_type
  )) %>% 
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron")

zipReviews <- df %>% group_by(zipcode = zipcode) %>% summarise(avg_loc_review = mean(review_scores_location, na.rm = TRUE))
colnames(zipReviews) <- c("region","value")
zipReviews$region <- as.character(zipReviews$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
g_locations <- zip_choropleth(zipReviews,
                              county_zoom = nyc_fips,
                              title = "Location Review Scores by Region",
                              legend = "Average Score") + ggtitle("Which area is the best?",
                                                                  subtitle = "Map showing Average Location Score by Area") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+ scale_fill_brewer("Location Review Score",palette=3)
g_locations



zipPrices <- df %>% group_by(zipcode = zipcode) %>% summarise(avg_price = mean(price, na.rm = TRUE))
colnames(zipPrices) <- c("region","value")
zipPrices$region <- as.character(zipPrices$region)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
g_price_location <- zip_choropleth(zipPrices,
                                   county_zoom = nyc_fips,
                                   title = "Average Price by Region",
                                   legend = "Average Score") + ggtitle("Which area is expensive?",
                                                                       subtitle = "Map showing Average Price by Area") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+ scale_fill_brewer("Average Price",palette=4)
g_price_location


propertydf <-  df %>% group_by(neighbourhood_group_cleansed, property_type) %>% summarize(Freq = n())
propertydf <- propertydf %>% filter(property_type %in% c("Apartment","House","Condominium","Townhouse", "Loft"))
totalproperty<-  df %>% filter(property_type %in% c("Apartment","House","Condominium","Townhouse", "Loft"))%>% group_by(neighbourhood_group_cleansed) %>% summarize(sum = n())
propertyratio <- merge(propertydf, totalproperty, by="neighbourhood_group_cleansed")
propertyratio <- propertyratio %>% mutate(ratio = Freq/sum)
ggplot(propertyratio, aes(x=neighbourhood_group_cleansed, y=ratio, fill = property_type)) +
  geom_bar(position = "dodge",stat="identity") + xlab("Borough") + ylab("Count")+
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Which types of Listings are there in NYC?",
          subtitle = "Map showing Count of Listing Type by Borough ") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) +
  xlab("Neighborhood") + ylab("Percentage")


nyc_map <- get_map(c(left = -74.194098, bottom = 40.538857, right = -73.762397, top = 40.888809), maptype = "toner-lite")
ggmap(nyc_map)

listingsAnim<- df%>% 
  mutate(join_year = year(host_since), join_month = month(host_since),
         join_date = as.Date(paste(join_month, 1, join_year, sep = "/"), '%m/%d/%Y'))
li_data_smry <- listingsAnim %>% 
  count(join_year, join_month) %>% ungroup() %>%
  arrange(join_year, join_month) %>%
  mutate(cumm_n = cumsum(n))
li_data_smry <- li_data_smry[complete.cases(li_data_smry), ]
li_data_smry <- inner_join(li_data_smry, select(listingsAnim, zipcode, latitude, longitude, join_year, join_month, join_date), by = c("join_year" = "join_year", "join_month" = "join_month"))

my_zip_plot <- function(df, plotdate, mapid){
  # create the background map. using the darken argument to make the map filled with black color.
  g <- ggmap(nyc_map, darken = c("0.8", "black")) 
  # split the data frame for all Walmarts before a plot date i.e. a month
  old_df <- filter(df, join_date < plotdate)
  # split the data frame for all Walmarts for the plot date i.e. during a month
  new_df <- filter(df, join_date == plotdate)
  # plot all the Walmarts before the current opening month. Make all the older store locations as shown in circles smaller
  g <- g + geom_point(data = old_df, aes(x = longitude, y = latitude), size = 2, color = "dodgerblue", alpha = 0.2)
  #plot all the Walmarts during the current opening month. Make all the newer store locations as shown in circles bigger to get the "pop" effect
  g <- g + geom_point(data = new_df, aes(x = longitude, y = latitude), size = 5, color = "dodgerblue", alpha = 0.2)
  # remove axis marks, labels, and titles
  g <- g + theme(axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), plot.title = element_blank())  
  # place the label for year 
  g <- g + annotate("text", x = -74.15, y = 40.85, label = "YEAR:", color = "white", size = rel(5), hjust = 0)
  # place the value of for year 
  g <- g + annotate("text", x = -74.15, y = 40.839, label = unique(new_df$join_year), color = "white", size = rel(6), fontface = 2, hjust = 0)
  # place the label for stores opened  
  g <- g + annotate("text", x = -74.15, y = 40.825, label = "LISTING COUNT:", color = "white", size = rel(5), hjust = 0)
  # place cumulative store openings
  g <- g + annotate("text", x = -74.15, y = 40.814, label = comma(unique(new_df$cumm_n)), color = "white", size = rel(6), fontface = 2, hjust = 0)
  # generate the file name for the map. Using str_pad to make the filename same length and prefixed with zeroes. 
  # create a maps directory inside the directory of this script.
  filename <- paste0("maps/img_" , str_pad(mapid, 7, pad = "0"),  ".png")
  #this saves the images created.
  ggsave(filename = filename, plot = g, width = 13, height = 7, dpi = 150, device = "png")
}

li_data_smry  %>%  
  mutate(mapid = group_indices_(li_data_smry, .dots = 'join_date')) %>% 
  group_by(join_date) %>% 
  do(pl = my_zip_plot(li_data_smry, unique(.$join_date), unique(.$mapid)))

my_zip_plot(li_data_smry, unique(li_data_smry$join_date), unique(li_data_smry$mapid))