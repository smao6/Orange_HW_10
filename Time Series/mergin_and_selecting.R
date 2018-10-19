rm(list=ls()); gc()

##################################
####     Visualization HW     ####
##################################

library(readxl)
library(lubridate)
library(tidyverse)
library(Hmisc)
library(zoo)
library(broom)
library(uroot)
library(lmtest)
library(forecast)
library(dplyr)
library(ggplot2)
library(gtools)


setwd("D:/Time_Series/Homework")


##################################
##  Well G-1260_T cleaning  ######
##################################
#The begin data is 10/1/2007 at 1:00 am and end date should be 6/8/2018 at 11:45 am.*


# Saving File Locations and Uploading SAS File #

input.fileG1260  <- "G-1260_T.xlsx"

#read in sheet from file
well_G1260 <- read_excel("G-1260_T.xlsx", sheet = "Well")

well2_G1260 <- well_G1260 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G1260<-well2_G1260%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G1260$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #508

#merge the actual datetime and well3_G1260 and impute the missing values
well4_G1260 <- merge(datetime, well3_G1260, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G1260$avg_well))
#impute the missing value
well4_G1260$avg_well <- na.interp(well4_G1260$avg)
sum(is.na(well4_G1260$avg_well))

well4_G1260$well <- "G-1260_T"
well4_G1260 <-as.data.frame(well4_G1260)


####rain#####

input.fileG1260T  <- "G-1260_T.xlsx"
rain_G1260T <- read_excel("G-1260_T.xlsx", sheet = "Rain")


rain_G1260T$date <- strptime(format(rain_G1260T$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G1260T$time  <- strptime(format(rain_G1260T$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G1260T <- rain_G1260T[,c(-1)]
rain_G1260T<- rain_G1260T[,c(2,3,1)]
str(rain_G1260T)

rain2_G1260T <- rain_G1260T
# create a new variable which is an integer for the hour of each time
rain2_G1260T$time_2 = hour(rain2_G1260T$time)

rain2_G1260T<-  rain2_G1260T %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G1260T<-rain2_G1260T%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))

#0 missing values 

rain3_G1260T$well <- "G-1260_T"



##combining rain and well:

well4_G1260$avg_well <- as.numeric(well4_G1260$avg_well)
well4_G1260<- well4_G1260[,c(-3)]

well_G1260_merge <- merge(well4_G1260, rain3_G1260T, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well_G1260_merge)




###########RESTRICTION:
#The begin data is 10/1/2007 at 1:00 am and end date should be 6/8/2018 at 11:45 am. 

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-06-08 11:45", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1


#merge the actual datetime and well3_G1260 and impute the missing values
well_G1260_merge_final <- merge(well_G1260_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well_G1260_merge_final)


well_G1260_merge_final$inclusion[is.na(well_G1260_merge_final$inclusion)] <- 0
well_G1260_merge_final <- subset(well_G1260_merge_final,inclusion==1 )



sum(is.na(well_G1260_merge_final$avg_well))
sum(is.na(well_G1260_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well_G1260_merge_final, file="well_G1260_merge.csv", row.names = FALSE)











##################################
###    Well G-3549 cleaning    ###
##################################

#read in sheet from file
well_G3549 <- read_excel("G-3549.xlsx", sheet = "Well")

well2_G3549 <- well_G3549 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G3549<-well2_G3549%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G3549$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #857

#merge the actual datetime and well3_G3549 and impute the missing values
well4_G3549 <- merge(datetime, well3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G3549$avg_well))
#impute the missing value
well4_G3549$avg_well <- na.interp(well4_G3549$avg)
sum(is.na(well4_G3549$avg_well))

well4_G3549$well <- "G-3549"
well4_G3549 <-as.data.frame(well4_G3549)




####rain#####

rain_G3549 <- read_excel("G-3549.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)



rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))

#0 missing values 

rain3_G3549$well <- "G-3549"


##combining rain and well:

well4_G3549$avg_well <- as.numeric(well4_G3549$avg_well)
well4_G3549<- well4_G3549[,c(-3)]

well_G3549_merge <- merge(well4_G3549, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well_G3549_merge)






###########RESTRICTION:

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-06-12 23:45", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well_G3549_merge_final <- merge(well_G3549_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well_G3549_merge_final)


well_G3549_merge_final$inclusion[is.na(well_G3549_merge_final$inclusion)] <- 0
well_G3549_merge_final <- subset(well_G3549_merge_final,inclusion==1 )



sum(is.na(well_G3549_merge_final$avg_well))
sum(is.na(well_G3549_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well_G3549_merge_final, file="well_G3549_merge.csv", row.names = FALSE)
















##################################
###    Well G561_T cleaning    ###
##################################
#read in sheet from file
well_G561 <- read_excel("G-561_T.xlsx", sheet = "Well")

well2_G561 <- well_G561 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G561<-well2_G561%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-05-1 0:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G561$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #4022

#merge the actual datetime and well3_G561 and impute the missing values
well4_G561 <- merge(datetime, well3_G561, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G561$avg_well))
#impute the missing value
well4_G561$avg_well <- na.interp(well4_G561$avg)
sum(is.na(well4_G561$avg_well))

well4_G561$well <- "G-561_T"
well4_G561 <-as.data.frame(well4_G561)




################################################################
####rain#####
################################################################



rain_G3549 <- read_excel("G-561_T.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)



rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "G-561_T"


##combining rain and well:

well4_G561$avg_well <- as.numeric(well4_G561$avg_well)
well4_G561<- well4_G561[,c(-3)]

well4_G561_merge <- merge(well4_G561, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_G561_merge)




###########RESTRICTION:

datetime=seq(
  from=as.POSIXct("2007-10-05 12:00", tz="UTC"),
  to=as.POSIXct("2018-06-12 23:45", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_G561_merge_final <- merge(well4_G561_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_G561_merge_final)


well4_G561_merge_final$inclusion[is.na(well4_G561_merge_final$inclusion)] <- 0
well4_G561_merge_final <- subset(well4_G561_merge_final,inclusion==1 )



sum(is.na(well4_G561_merge_final$avg_well))
sum(is.na(well4_G561_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_G561_merge_final, file="well_G561_merge.csv", row.names = FALSE)






















##################################
###   Well G-2147 cleaning     ###
##################################

input.fileG2147 <- "G-2147_T.xlsx"

#read in sheet from file
well_G2147 <- read_excel("G-2147_T.xlsx", sheet = "Well")

well2_G2147 <- well_G2147 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G2147 <-well2_G2147%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-10 0:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G2147$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #970

#merge the actual datetime and well3_G2147 and impute the missing values
well4_G2147 <- merge(datetime, well3_G2147, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G2147$avg_well))
#impute the missing value
well4_G2147$avg_well <- na.interp(well4_G2147$avg)
sum(is.na(well4_G2147$avg_well))

well4_G2147$well <- "G-2147_T"
well4_G2147 <-as.data.frame(well4_G2147)




################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("G-2147_T.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "G2147"


##combining rain and well:

well4_G2147$avg_well <- as.numeric(well4_G2147$avg_well)
well4_G2147<- well4_G2147[,c(-3)]

well_G2147_merge <- merge(well4_G2147, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well_G2147_merge)



###########RESTRICTION:

datetime=seq(
  from=as.POSIXct("2007-10-10 12:00", tz="UTC"),
  to=as.POSIXct("2018-06-08 9:15", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well_G2147_merge_final <- merge(well_G2147_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well_G2147_merge_final)


well_G2147_merge_final$inclusion[is.na(well_G2147_merge_final$inclusion)] <- 0
well_G2147_merge_final <- subset(well_G2147_merge_final,inclusion==1 )



sum(is.na(well_G2147_merge_final$avg_well))
sum(is.na(well_G2147_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well_G2147_merge_final, file="well_G2147_merge.csv", row.names = FALSE)









##################################
###    Well G-2866 cleaning    ###
##################################

input.fileG2866 <- "G-2866_T.xlsx"

#read in sheet from file
well_G2866 <- read_excel("G-2866_T.xlsx", sheet = "Well")

well2_G2866 <- well_G2866 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G2866 <-well2_G2866%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G2866$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #259

#merge the actual datetime and well3_G2866 and impute the missing values
well4_G2866 <- merge(datetime, well3_G2866, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G2866$avg_well))
#impute the missing value
well4_G2866$avg_well <- na.interp(well4_G2866$avg)
sum(is.na(well4_G2866$avg_well))

well4_G2866$well <- "G-2866_T"
well4_G2866 <-as.data.frame(well4_G2866)




################################################################
####rain#####
################################################################

rain_G3549 <-read_excel("G-2866_T.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "G2866"


##combining rain and well:

well4_G2866$avg_well <- as.numeric(well4_G2866$avg_well)
well4_G2866<- well4_G2866[,c(-3)]

well_G2866_merge <- merge(well4_G2866, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well_G2866_merge)




###########RESTRICTION:

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-06-08 9:30", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well_G2866_merge_final <- merge(well_G2866_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well_G2866_merge_final)


well_G2866_merge_final$inclusion[is.na(well_G2866_merge_final$inclusion)] <- 0
well_G2866_merge_final <- subset(well_G2866_merge_final,inclusion==1 )



sum(is.na(well_G2866_merge_final$avg_well))
sum(is.na(well_G2866_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well_G2866_merge_final, file="well_G2866_merge.csv", row.names = FALSE)


















##################################
####    Well F-45 cleaning    ####
##################################

input.fileF45 <- "F-45.xlsx"

#read in sheet from file
well_F45 <- read_excel("F-45.xlsx", sheet = "Well")

well2_F45 <- well_F45 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_F45 <-well2_F45%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_F45$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #4712

#merge the actual datetime and well3_F45 and impute the missing values
well4_F45<- merge(datetime, well3_F45, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_F45$avg_well))
#impute the missing value
well4_F45$avg_well <- na.interp(well4_F45$avg)
sum(is.na(well4_F45$avg_well))

well4_F45$well <- "F-45"
well4_F45 <-as.data.frame(well4_F45)




################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("F-45.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "F-45"


##combining rain and well:

well4_F45$avg_well <- as.numeric(well4_F45$avg_well)
well4_F45<- well4_F45[,c(-3)]

well4_F45_merge <- merge(well4_F45, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_F45_merge)





###########RESTRICTION:

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-03-26 10:45", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_F45_merge_final <- merge(well4_F45_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_F45_merge_final)


well4_F45_merge_final$inclusion[is.na(well4_F45_merge_final$inclusion)] <- 0
well4_F45_merge_final <- subset(well4_F45_merge_final,inclusion==1 )



sum(is.na(well4_F45_merge_final$avg_well))
sum(is.na(well4_F45_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_F45_merge_final, file="well4_F45_merge.csv", row.names = FALSE)














##################################
####    Well F-179 cleaning   ####
##################################

#read in sheet from file
well_F179 <- read_excel("F-179.xlsx", sheet = "Well")

well2_F179 <- well_F179 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_F179 <-well2_F179%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-4 10:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_F179$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #955

#merge the actual datetime and well3_F179 and impute the missing values
well4_F179<- merge(datetime, well3_F179, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_F179$avg_well))
#impute the missing value
well4_F179$avg_well <- na.interp(well4_F179$avg)
sum(is.na(well4_F179$avg_well))

well4_F179$well <- "F-179"
well4_F179 <-as.data.frame(well4_F179)




################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("F-179.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "F179"


##combining rain and well:

well4_F179$avg_well <- as.numeric(well4_F179$avg_well)
well4_F179<- well4_F179[,c(-3)]

well4_F179_merge <- merge(well4_F179, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_F179_merge)



###########RESTRICTION:

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-06-04 10:45", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_F179_merge_final <- merge(well4_F179_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_F179_merge_final)


well4_F179_merge_final$inclusion[is.na(well4_F179_merge_final$inclusion)] <- 0
well4_F179_merge_final <- subset(well4_F179_merge_final,inclusion==1 )



sum(is.na(well4_F179_merge_final$avg_well))
sum(is.na(well4_F179_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_F179_merge_final, file="well4_F179_merge.csv", row.names = FALSE)






















##################################
####    Well F-319 cleaning   ####
##################################

input.fileF319 <- "F-319.xlsx"

#read in sheet from file
well_F319 <- read_excel("F-319.xlsx", sheet = "Well")

well2_F319 <- well_F319 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_F319 <-well2_F319%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-4 11:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_F319$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #2022

#merge the actual datetime and well3_F319 and impute the missing values
well4_F319<- merge(datetime, well3_F319, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_F319$avg_well))
#impute the missing value
well4_F319$avg_well <- na.interp(well4_F319$avg)
sum(is.na(well4_F319$avg_well))

well4_F319$well <- "F-319"
well4_F319 <-as.data.frame(well4_F319)




################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("F-319.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "F319"


##combining rain and well:

well4_F319$avg_well <- as.numeric(well4_F319$avg_well)
well4_F319<- well4_F319[,c(-3)]

well4_F319_merge <- merge(well4_F319, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_F319_merge)


################################################################
###########RESTRICTION:
################################################################

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-04-10 00:00", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_F319_merge_final <- merge(well4_F319_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_F319_merge_final)


well4_F319_merge_final$inclusion[is.na(well4_F319_merge_final$inclusion)] <- 0
well4_F319_merge_final <- subset(well4_F319_merge_final,inclusion==1 )



sum(is.na(well4_F319_merge_final$avg_well))
sum(is.na(well4_F319_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_F319_merge_final, file="well4_F319_merge.csv", row.names = FALSE)











##################################
####   Well G-580A cleaning   ####
##################################

input.fileG580A <- "G-580A.xlsx"

#read in sheet from file
well_G580A <- read_excel("G-580A.xlsx", sheet = "Well")

well2_G580A <- well_G580A %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G580A <-well2_G580A%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 0:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G580A$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #4484

#merge the actual datetime and well3_G580A and impute the missing values
well4_G580A<- merge(datetime, well3_G580A, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G580A$avg_well))
#impute the missing value
well4_G580A$avg_well <- na.interp(well4_G580A$avg)
sum(is.na(well4_G580A$avg_well))

well4_G580A$well <- "G-580A"
well4_G580A <-as.data.frame(well4_G580A)



################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("G-580A.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "G580A"


##combining rain and well:

well4_G580A$avg_well <- as.numeric(well4_G580A$avg_well)
well4_G580A<- well4_G580A[,c(-3)]

well4_G580A_merge <- merge(well4_G580A, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_G580A_merge)



################################################################
###########RESTRICTION:
################################################################

datetime=seq(
  from=as.POSIXct("2007-10-01 12:00", tz="UTC"),
  to=as.POSIXct("2018-04-09 11:00", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_G580A_merge_final <- merge(well4_G580A_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_G580A_merge_final)


well4_G580A_merge_final$inclusion[is.na(well4_G580A_merge_final$inclusion)] <- 0
well4_G580A_merge_final <- subset(well4_G580A_merge_final,inclusion==1 )



sum(is.na(well4_G580A_merge_final$avg_well))
sum(is.na(well4_G580A_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_G580A_merge_final, file="well4_G580A_merge.csv", row.names = FALSE)











##################################
####   Well G-852 cleaning    ####
##################################


#read in sheet from file
well_G852 <- read_excel("G-852.xlsx", sheet = "Well")

well2_G852 <- well_G852 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(Time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, Date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G852 <-well2_G852%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2000-07-28 8:00", tz="UTC"),
  to=as.POSIXct("2018-07-06 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G852$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #66489

#merge the actual datetime and well3_G580A and impute the missing values
well4_G852<- merge(datetime, well3_G852, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G852$avg_well))
#impute the missing value
well4_G852$avg_well <- na.interp(well4_G852$avg)
sum(is.na(well4_G852$avg_well))

well4_G852$well <- "G-852"
well4_G852 <-as.data.frame(well4_G852)





################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("G-852.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "G852"


##combining rain and well:

well4_G852$avg_well <- as.numeric(well4_G852$avg_well)
well4_G852<- well4_G852[,c(-3)]

well4_G852_merge <- merge(well4_G852, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_G852_merge)


################################################################
###########RESTRICTION:
################################################################

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-04-09 13:42", tz="UTC"),
  by="hour"
)


datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_G852_merge_final <- merge(well4_G852_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_G852_merge_final)


well4_G852_merge_final$inclusion[is.na(well4_G852_merge_final$inclusion)] <- 0
well4_G852_merge_final <- subset(well4_G852_merge_final,inclusion==1 )



sum(is.na(well4_G852_merge_final$avg_well))
sum(is.na(well4_G852_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_G852_merge_final, file="well4_G852_merge.csv", row.names = FALSE)












##################################
####   Well G-860 cleaning   ####
##################################

#read in sheet from file
well_G860 <- read_excel("G-860.xlsx", sheet = "Well")

well2_G860 <- well_G860 %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G860 <-well2_G860%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2005-10-10 0:00", tz="UTC"),
  to=as.POSIXct("2018-6-30 19:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G860$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #21148

#merge the actual datetime and well3_G-860 and impute the missing values
well4_G860<- merge(datetime, well3_G860, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G860$avg_well))
#impute the missing value
well4_G860$avg_well <- na.interp(well4_G860$avg)
sum(is.na(well4_G860$avg_well))

well4_G860$well <- "G-860"
well4_G860 <-as.data.frame(well4_G860)





################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("G-860.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "G860"


##combining rain and well:

well4_G860$avg_well <- as.numeric(well4_G860$avg_well)
well4_G860<- well4_G860[,c(-3)]

well4_G860_merge <- merge(well4_G860, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_G860_merge)




################################################################
###########RESTRICTION:
################################################################

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-06-05 00:30", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_G860_merge_final <- merge(well4_G860_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_G860_merge_final)


well4_G860_merge_final$inclusion[is.na(well4_G860_merge_final$inclusion)] <- 0
well4_G860_merge_final <- subset(well4_G860_merge_final,inclusion==1 )


sum(is.na(well4_G860_merge_final$avg_well))
sum(is.na(well4_G860_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_G860_merge_final, file="well4_G860_merge.csv", row.names = FALSE)










##################################
##   Well G-1220-T cleaning   ####
##################################

#read in sheet from file
well_G1220T <- read_excel("G-1220_T.xlsx", sheet = "Well")

well2_G1220T <- well_G1220T %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G1220T <-well2_G1220T%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G1220T$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #17694

#merge the actual datetime and well3_G1220T and impute the missing values
well4_G1220T<- merge(datetime, well3_G1220T, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G1220T$avg_well))
#impute the missing value
well4_G1220T$avg_well <- na.interp(well4_G1220T$avg)
sum(is.na(well4_G1220T$avg_well))

well4_G1220T$well <- "G-1220_T"
well4_G1220T <-as.data.frame(well4_G1220T)






################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("G-1220_T.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "G1220T"


##combining rain and well:

well4_G1220T$avg_well <- as.numeric(well4_G1220T$avg_well)
well4_G1220T<- well4_G1220T[,c(-3)]

well4_G1220T_merge <- merge(well4_G1220T, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_G1220T_merge)
str(well4_G1220T_merge)



################################################################
###########RESTRICTION:
################################################################

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-04-21 20:45", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_G1220T_merge_final <- merge(well4_G1220T_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_G1220T_merge_final)


well4_G1220T_merge_final$inclusion[is.na(well4_G1220T_merge_final$inclusion)] <- 0
well4_G1220T_merge_final <- subset(well4_G1220T_merge_final,inclusion==1 )


sum(is.na(well4_G1220T_merge_final$avg_well))
sum(is.na(well4_G1220T_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_G1220T_merge_final, file="well_G1220T_merge.csv", row.names = FALSE)













##################################
##   Well PB-1680_T cleaning   ####
##################################

#read in sheet from file
well_G1220T <- read_excel("PB-1680_T.xlsx", sheet = "Well")

well2_G1220T <- well_G1220T %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = hour(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected)

#group our data into hourly data and use the mean of depth
well3_G1220T <-well2_G1220T%>%
  group_by(datetime)%>%
  summarise(avg_well=mean(depth))

#find if there is any missing value
datetime=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)
missing_hours=length(datetime)-length(well3_G1220T$datetime) 
datetime<-as.data.frame(datetime)
missing_hours #17694

#merge the actual datetime and well3_G1220T and impute the missing values
well4_G1220T<- merge(datetime, well3_G1220T, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
sum(is.na(well4_G1220T$avg_well))
#impute the missing value
well4_G1220T$avg_well <- na.interp(well4_G1220T$avg)
sum(is.na(well4_G1220T$avg_well))

well4_G1220T$well <- "PB-1680_T"
well4_G1220T <-as.data.frame(well4_G1220T)




################################################################
####rain#####
################################################################

rain_G3549 <- read_excel("PB-1680_T.xlsx", sheet = "Rain")

rain_G3549$date <- strptime(format(rain_G3549$Date, "%Y/%m/%d"), "%Y/%m/%d")
rain_G3549$time  <- strptime(format(rain_G3549$Date, "%H:%M:%S"), "%H:%M:%S")
rain_G3549 <- rain_G3549[,c(-1)]
rain_G3549<- rain_G3549[,c(2,3,1)]
str(rain_G3549)


rain2_G3549 <- rain_G3549
# create a new variable which is an integer for the hour of each time
rain2_G3549$time_2 = hour(rain2_G3549$time)


rain2_G3549<-  rain2_G3549 %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ymd_h(datetime)) %>%
  # select only the new datetime variable and rename the Tide_ft variable to depth
  select(datetime, RAIN_FT = RAIN_FT) 

#group our data into hourly data and use the mean of Tide_ft
rain3_G3549<-rain2_G3549%>%
  group_by(datetime)%>%
  summarise(sum_rain=sum(RAIN_FT))


rain3_G3549$well <- "PB-1680_T"


##combining rain and well:

well4_G1220T$avg_well <- as.numeric(well4_G1220T$avg_well)
well4_G1220T<- well4_G1220T[,c(-3)]

well4_G1220T_merge <- merge(well4_G1220T, rain3_G3549, by.all=c("datetime"), all.x = TRUE, all.y = TRUE)

dim(well4_G1220T_merge)
str(well4_G1220T_merge)






################################################################
###########RESTRICTION:
################################################################

datetime=seq(
  from=as.POSIXct("2007-10-01 1:00", tz="UTC"),
  to=as.POSIXct("2018-02-08 9:15", tz="UTC"),
  by="hour"
)

datetime<-as.data.frame(datetime)
datetime$inclusion <-  1

#merge the actual datetime and well3_G1260 and impute the missing values
well4_G1220T_merge_final <- merge(well4_G1220T_merge, datetime, by.all=c("datetime"), all.x = TRUE, all.y = TRUE )
#View(well4_G1220T_merge_final)


well4_G1220T_merge_final$inclusion[is.na(well4_G1220T_merge_final$inclusion)] <- 0
well4_G1220T_merge_final <- subset(well4_G1220T_merge_final,inclusion==1 )


sum(is.na(well4_G1220T_merge_final$avg_well))
sum(is.na(well4_G1220T_merge_final$sum_rain))
#impute the missing value
#well4_G1260$avg_well <- na.interp(well4_G1260$avg)


write.csv(well4_G1220T_merge_final, file="well_PB1680_T_merge.csv", row.names = FALSE)

