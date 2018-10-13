library(readxl)
library(lubridate)
library(tidyverse)
library(zoo)
library(forecast)
library(ggplot2)

well <- read_excel("G-2866_T.xlsx", sheet = "Well")
summary(well)
hist(well$Corrected)


#################################################
#######WELL:
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
  summarise(avg=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)  
datetime<-as.data.frame(datetime)
missing_hours=length(seq)-length(well_3$datetime) 
#259
#merge the actual datetime and well_3 
well_4 <- merge(datetime, well_3, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well_4$avg))
#impute the missing value
well_4$avg <- na.interp(well_4$avg)
sum(is.na(well_4$avg))
#since we only have 259 missing observations, we can just leave it there and do



##########################################################3
#######TIDE:

tide$Date

tide <- read_excel("G-2866_T.xlsx", sheet = "Tide")
summary(tide)
hist(tide$Tide_ft)

tide_2 <- tide %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(Time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, Date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, Tide_ft = Tide_ft) 

#group our data into hourly data and use the mean of Tide_ft
tide_3<-tide_2%>%
  group_by(datetime)%>%
  summarise(avg=mean(Tide_ft))

#find if there is any missing value  - UPDATE THE TIME IF NEEDED
datetime=seq(
  from=as.POSIXct("2005-10-10 0:00", tz="UTC"),
  to=as.POSIXct("2006-10-09 23:00", tz="UTC"),
  by="hour"
)  
datetime<-as.data.frame(datetime)
missing_hours=length(datetime$datetime)-length(tide_3$datetime)
print(missing_hours)
#1
#merge the actual datetime and tide_3 
tide_4 <- merge(datetime, tide_3, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(tide_4$avg))
#impute the missing value
tide_4$avg <- na.interp(tide_4$avg)
sum(is.na(tide_4$avg))










##########################################################3
#######RAIN:


rain <- read_excel("G-2866_T.xlsx", sheet = "Rain")
summary(rain)
hist(rain$RAIN_FT)
str(rain)
dim(rain)

rain$date <- strptime(format(rain$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain$time  <- strptime(format(rain$Date, "%H:%M:%S"), "%H:%M:%S")
rain <- rain[,c(-1)]
rain<- rain[,c(2,3,1)]
str(tide)
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


#group our data into hourly data and use the mean of Tide_ft
rain_3<-rain_2%>%
  group_by(datetime)%>%
  summarise(avg=sum(RAIN_FT))

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





well_4$avg_well <- as.numeric(well_4$avg)
tide_4$avg_tide <- as.numeric(tide_4$avg)
rain_3$sum_rain <- rain_3$avg

well_4<- well_4[,c(-2)]
tide_4<- tide_4[,c(-2)]
rain_3<- rain_3[,c(-2)]

well_merge <- merge(merge(well_4, tide_4, by.all=c("datetime"), all.x = TRUE, all.y = TRUE),rain_3, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)


## Only 3 years:
Train3=well_merge[67494:93791,]
plot(Train3$avg_well)

write.csv(Train3, file="Train3_R_new.csv", row.names = FALSE)


















##STL Decomposition:

#Creation of Time Series Data Object

#twice a year
well_stl <- ts(Train3$avg_well, frequency =2191)
#Decomposition ...STL
decomp_stl <- stl(well_stl, s.window = 7, na.action = na.approx)
plot(decomp_stl)


seas_pass=well_stl-decomp_stl$time.series[,1]

seas_pass2= well_stl-decomp_stl$time.series[,1] - decomp_stl$time.series[,2] 

plot(well_stl, col = "black", main = "Corrected Well Height (ft) - Seasonally Adjusted", xlab = "", ylab = "Number of Passengers (Thousands)", lwd = 2)
lines(seas_pass, col = "red", lwd = 2)


general_well <- data.frame("well_stl" = as.numeric(well_stl),"seas_pass" = as.numeric(seas_pass),
                           "seas_pass2" =  as.numeric(seas_pass2))
general_well$index <- as.factor(seq.int(nrow(general_well)))


##adding a box with legend:
ggplot()+
  geom_line(data = general_well, size = 1,aes(color = "Actual", x = index, y = well_stl, group=1)) +
  geom_line(data =general_well , size = 1, aes(color="Predicted",x= index, y = seas_pass, group = 1))+
  labs(x = "\nYears", y = "Well Height (ft)\n", title = "Corrected Well Water - Seasonally Adjusted\n") +
  theme(plot.title = element_text(hjust = 0.5, size=28),text = element_text(size=16),
        axis.title.x = element_text(size= 25), axis.title.y = element_text(size= 25)) + 
  scale_x_discrete(breaks=seq(1,26298,8765),
                   labels=c("1" = "2015","8766" = "2016","17531" = "2017","26296" = "2018")) + 
  scale_colour_manual("Parameters",values=c("black","red"))





ggplot()+
  geom_line(data = general_well, size = 1,aes(color = "Actual", x = index, y = well_stl, group=1)) +
  geom_line(data =general_well , size = 1, aes(color="Adjusted",x= index, y = seas_pass2, group = 1))+
  labs(x = "\nYears", y = "Well Height (ft)\n", title = "Corrected Well Water - Seasonally + Trend Adjusted\n") +
  theme(plot.title = element_text(hjust = 0.5, size=28),text = element_text(size=16),
        axis.title.x = element_text(size= 25), axis.title.y = element_text(size= 25)) + 
  scale_x_discrete(breaks=seq(1,26298,8765),
                   labels=c("1" = "2015","8766" = "2016","17531" = "2017","26296" = "2018")) + 
  scale_colour_manual("Parameters",values=c("black","red"))






