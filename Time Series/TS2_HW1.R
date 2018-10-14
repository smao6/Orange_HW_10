##################################
####Orange 10 Time Series HW 1####
##################################


library(readxl)
library(lubridate)
library(tidyverse)
library(Hmisc)
library(zoo)
library(broom)
library(ggfortify)
library(uroot)
library(lmtest)
library(forecast)
library(dplyr)
library(ggplot2)
##################################
#######Well data cleaning#########
##################################

well <- read_excel("G-2866_T.xlsx", sheet = "Well")
summary(well)
hist(well$Corrected)

well_2 <- well %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected) 


#group our data into hourly data and use the mean of depth
well_3<-well_2%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well_3$datetime) 
datetime<-as.data.frame(datetime)
#259

#merge the actual datetime and well_3 and impute the missing values
well_4 <- merge(datetime, well_3, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well_4$avg_well))
#impute the missing value
well_4$avg_well <- na.interp(well_4$avg)
sum(is.na(well_4$avg_well))


##############################
######Rain Data cleaning######
##############################
rain <- read_excel("G-2866_T.xlsx", sheet = "Rain")
summary(rain)
hist(rain$RAIN_FT)
str(rain)
dim(rain)

rain$date <- strptime(format(rain$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain$time  <- strptime(format(rain$Date, "%H:%M:%S"), "%H:%M:%S")
rain <- rain[,c(-1)]
rain<- rain[,c(2,3,1)]
str(rain)

rain_2 <- rain
# create a new variable which is an integer for the hour of each time
rain_2$time_2 = hour(rain_2$time)

rain_2<-  rain_2 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 


#group our data into hourly data and use the sum of rain for an hour
rain_3<-rain_2%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))

#find if there is any missing value  - UPDATE THE TIME IF NEEDED
datetime=seq(
  from=as.POSIXct("2005-10-10 0:00", tz="UTC"),
  to=as.POSIXct("2018-07-09 23:00", tz="UTC"),
  by="hour"
)  
datetime<-as.data.frame(datetime)
missing_hours=length(datetime$datetime)-length(rain_3$datetime)
print(missing_hours)
##0

#merge rain and well
well_4$avg <- as.numeric(well_4$avg)
rain_3$sum <- rain_3$sum

#########################
##########Tide###########
#########################
tide <- read_csv("station_8722859.csv")
summary(tide)
hist(tide$Prediction)

tide_2 <- tide %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(Time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, Date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime,Prediction) 


#group our data into hourly data and use the mean of depth
tide_3<-tide_2%>%
  group_by(datetime)%>%
  summarise(avg_tide=mean(Prediction))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2005-10-10 0:00", tz="UTC"),
  to=as.POSIXct("2018-10-9 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(tide_3$datetime) 
datetime<-as.data.frame(datetime)

#merge the actual datetime and tide_3 and impute the missing values
tide_4 <- merge(datetime, tide_3, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(tide_4$avg_tide))
#impute the missing value
tide_4$avg_tide <- na.interp(tide_4$avg_tide)
sum(is.na(tide_4$avg_tide))


#data for 10 years
well_merge <-inner_join(inner_join(well_4,rain_3,by = NULL, copy = FALSE),tide_4, by=NULL, copy = FALSE)
write.csv(well_merge, file="merge_well.csv", row.names = FALSE)

#subset recent three years data
train_3<-well_merge[67494:93791,]
write.csv(train_3,file="merge_well_3.csv",row.names = FALSE)


################################
#########explore data###########
################################

## time series object train_3
well_ts_train3_well<-ts(train_3$avg_well, frequency = 8766/4)
well_ts_train3_tide<-ts(train_3$avg_tide, frequency = 8766)
well_ts_train3_rain<-ts(train_3$sum_rain, frequency = 8766/4)

##STL decomposition: quarter season 
decomp_stl_well <- stl(well_ts_train3_well, s.window = 7)
plot(decomp_stl_well)
decomp_stl_tide <- stl(well_ts_train3_tide, s.window = 7)
plot(decomp_stl_tide)
decomp_stl_rain <- stl(well_ts_train3_rain, s.window = 7)
plot(decomp_stl_rain)


# create a gg object using the train_3 dataframe with aesthetics: x = datetime and y = avg
ggplot(train_3, aes(datetime, avg_well)) +
  # draw a line plot using the above defined  aesthetics
  geom_line(color = "black") +
  # drop some of the ugly R thematic elements for a simple look
  theme_bw() +
  # label the axes and add a title
  labs(x = "Date And Time (in hours)", y = "Avg Depth of Well (in feet)", title = "Avg Depth of Well From 2015-2018")

##################################
#######Modeling in SAS code#######
##################################
