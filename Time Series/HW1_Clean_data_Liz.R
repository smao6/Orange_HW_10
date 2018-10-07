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



##STL Decomposition:
#Creation of Time Series Data Object
well_stl <- ts(well_4$avg, frequency =8760)

#Decomposition ...STL
decomp_stl <- stl(well_stl, s.window = 7, na.action = na.approx)
plot(decomp_stl)

plot(well_stl, col = "grey", main = "Well - Trend/Cycle", xlab = "", ylab = "Well Avg.", lwd = 2)
lines(decomp_stl$time.series[,2], col = "red", lwd = 2)

seas_pass=well_stl-decomp_stl$time.series[,1]
plot(well_stl, col = "grey", main = "Well - Seasonally Adjusted", xlab = "", ylab = "Well Avg.", lwd = 2)
lines(seas_pass, col = "red", lwd = 2)
