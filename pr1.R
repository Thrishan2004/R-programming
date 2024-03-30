
# 1. Reading AIRBNB data in .csv format
data <- read.csv("D:/r/nyclist.csv")
head(data,3)
str(data) # to check the datatypes of column values


# 2.Data cleaning and transformation.
library(dplyr) #accessing libraries
library(tidyr) #accessing libraries
is.na(data) #For finding missing values
sum(is.na(data)) #For counting all missing values
#Removing some columns which are not very important for our further analysis
data_1 <- data[, !(names(data) %in% c("id", "host_name", "last_review"))]
head(data_1,3)
data_1$reviews_per_month <- ifelse(is.na(data_1$reviews_per_month), 0, data_1$reviews_per_month)
sum(is.na(data_1$reviews_per_month))

#examining the unique values of n_group so it will help us later fro analysis
unique(data_1$neighbourhood_group)
#examining the unique values of neighbourhood so it will help us later for analysis
length(unique(data_1$neighbourhood))
#examining the unique values of room_type so it will help us later for analysis
unique(data_1$room_type)

#3.Exploratory Data analysis
top_host <- head(sort(table(data_1$host_id),decreasing = TRUE), 10)
write.table(top_host)
top_host_check <- max(data_1$calculated_host_listings_count)
top_host_check
#setting figure size for future visualizations
library(ggplot2)
ggplot2::theme_set(theme_classic(base_size = 10, base_family = "Helvetica"))

top_host_df <- data.frame(top_host)
top_host_df <- top_host_df[, c("Var1", "Freq")]
colnames(top_host_df) <- c("Host_ID", "P_Count")
top_host_df
#Bar plot
barplot(height = top_host_df$P_Count,xlab = "Host_id",ylab = "P_Count",main = "Hosts with the most listings in nyc",names.arg = top_host_df$Host_ID,col = "Blue")
data_1 %>% 
  count(neighbourhood) %>%
  arrange(desc(n)) %>% 
  head(10)
plot(x=data_1$longitude, y=data$latitude, xlab = "Longtitude", ylab = "Latitude")

top_reviewed_listings <- data_1 %>% 
  arrange(desc(number_of_reviews)) %>%
  head(10)

top_reviewed_listings


lat_avrg <- mean(data_1$latitude)
print(paste("Average Latitude value :", lat_avrg))

long_avrg <- mean(data_1$longitude)
print(paste("Average Longitude value :", long_avrg))

barplot(table(data_1$room_type), col = c("red","blue","yellow","green"))


apt <- data_1[data_1$room_type == "Entire home/apt",]
list_apt <- apt %>% 
  group_by(host_id, neighbourhood, neighbourhood_group_cleansed) %>% 
  summarise(apartment = n()) %>% 
  arrange(desc(apartment)) %>% 
  head(10)


private <- data_1[data_1$room_type == "Private room",]
list_private <- private %>% 
  group_by(host_id,neighbourhood) %>% 
  summarise(private = n()) %>% 
  arrange(desc(private)) %>% 
  head(10)


barplot(table(data_1$neighbourhood_group), col = c("red","blue","yellow","green","maroon"))


library(Metrics)
result<-rmse(data_1$longitude,data_1$latitude)
result