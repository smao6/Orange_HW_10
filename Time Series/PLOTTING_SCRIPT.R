##PLOTTING:

library(readxl)
library(zoo)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(seasonal)
library(dplyr)
library(ggplot2)

setwd("D:/Time_Series/Homework")
input.file1 <- "to_plot.csv"
#input.file1 <- "general_plot.csv"
df <- read.csv(input.file1, header = TRUE)

#df <- df[c(2193:26298),]


sum(is.na(df$avg))
sum(is.na(df$Forecast.for))

error=df$avg-df$Forecast.for
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(df$avg))
print(MAE)
print(MAPE*100)

df$index <- as.factor(seq.int(nrow(df)))

# Plotting desired results using GGPlot
# Actual vs. Predicted
ggplot()+
  geom_line(data = df, size = 1,aes(color = "Actual", x = index, y = avg, group=1)) +
  geom_line(data =df , size = 1, aes(color="Predicted",x= index, y = Forecast.for, group = 1))+
  labs(x = "\nWeek", y = "Corrected Well Height (ft)\n", title = "Corrected Well Water Heights for Well G-2866_T\n") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size=16)) + 
  scale_x_discrete(breaks=seq(24,168,24),
                   labels=c("24" = "Day 1","48" = "Day 2","72" = "Day 3","96" = "Day 4","120" = "Day 5","144" = "Day 6", "168" = "Day 7")) + 
  scale_colour_manual("Parameters",values=c("#00AFBB","black"))





















