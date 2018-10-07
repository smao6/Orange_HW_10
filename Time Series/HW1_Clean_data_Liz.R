library(readxl)
library(lubridate)
library(tidyverse)
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
seq=seq(
  from=as.POSIXct("2007-10-1 1:00", tz="UTC"),
  to=as.POSIXct("2018-6-12 23:00", tz="UTC"),
  by="hour"
)  
missing_hours=length(seq)-length(well_3$datetime) 
#259

# create a gg object using the well_3 dataframe with aesthetics: x = datetime and y = avg
ggplot(well_3, aes(datetime, avg)) +
  # draw a line plot using the above defined aesthetics
  geom_line(color = "black") +
  # drop some of the ugly R thematic elements for a simple look
  theme_bw() +
  # label the axes and add a title
  labs(x = "Date And Time (in hours)", y = "Avg Depth of Well (in feet)", title = "Avg Depth of Well From 2007-2018")

#clean well data in google dirve folder
write.csv(well_3, file = "well_clean.csv",row.names=FALSE)
