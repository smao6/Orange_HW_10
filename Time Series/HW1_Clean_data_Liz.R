library(readxl)
library(lubridate)
library(tidyverse)
library(zoo)

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
well_4$avg[is.na(well_4$avg)] <- mean(well_4$avg, na.rm = T)
sum(is.na(well_4$avg))
#since we only have 259 missing observations, we can just leave it there and do
#split into training and validation dataset (24*7=168 in validation set)
Train=well_4[1:93623,]
Test=well_4[93624:93791,]

# create a gg object using the well_3 dataframe with aesthetics: x = datetime and y = avg
ggplot(well_4, aes(datetime, avg)) +
  # draw a line plot using the above defined aesthetics
  geom_line(color = "black") +
  # drop some of the ugly R thematic elements for a simple look
  theme_bw() +
  # label the axes and add a title
  labs(x = "Date And Time (in hours)", y = "Avg Depth of Well (in feet)", title = "Avg Depth of Well From 2007-2018")

#clean well data in google dirve folder
write.csv(well_4, file = "well_clean.csv",row.names=FALSE)
write.csv(Train, file="well_train.csv", row.names = FALSE )
write.csv(Test, file="well_test.csv", row.names = FALSE)






## Only 3 years:
Train3=well_4[67494:93791,]

write.csv(Train3, file="Train3_R.csv", row.names = FALSE)

##STL Decomposition:

#Creation of Time Series Data Object

#twice a year
well_stl <- ts(Train3$avg, frequency =2191)
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






