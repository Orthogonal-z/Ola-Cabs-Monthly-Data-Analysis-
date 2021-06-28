#Set the working directory
setwd("C:/Users/kapil/Desktop/GitHub/Ola Cabs Dataset Analytics")
getwd()

#Load Libraries
install.packages("DT")
install.packages("scales")
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(tidyr)
library(DT)
library(scales)


#Import the dataset
april <- read.csv("C:\\Users\\kapil\\Desktop\\GitHub\\Ola Cabs Dataset Analytics\\April.csv")
may <- read.csv("C:\\Users\\kapil\\Desktop\\GitHub\\Ola Cabs Dataset Analytics\\May.csv")
june <- read.csv("C:\\Users\\kapil\\Desktop\\GitHub\\Ola Cabs Dataset Analytics\\June.csv")
july <- read.csv("C:\\Users\\kapil\\Desktop\\GitHub\\Ola Cabs Dataset Analytics\\July.csv")
august <- read.csv("C:\\Users\\kapil\\Desktop\\GitHub\\Ola Cabs Dataset Analytics\\August.csv")
september <- read.csv("C:\\Users\\kapil\\Desktop\\GitHub\\Ola Cabs Dataset Analytics\\september.csv")


# Combine the datasets in one dataframe
# There is no missing value in the dataset
# A total of 4534327 rows and 4 columns, all are numeric except Base which is character 
Data <- rbind(april,may,june,july,august,september)
View(Data)
head(Data)
summary(Data)  
str(Data)      


# Covert Date.Time column in a posix format with lubridate function 
Data$Date.Time <- as.POSIXct(Data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

# Separating the Time from Date.time column 
Data$Time <- format(as.POSIXct(Data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), 
                    format = "%H:%M:%S")


# Separating Year, Month, Day and Day of the week from Date.Time column 
Data$Date.Time <- ymd_hms((Data$Date.Time))
Data$day <- format(day((Data$Date.Time)))
Data$month <- format(month((Data$Date.Time), label = T))
Data$year <- format(year(Data$Date.Time))
Data$day_of_the_week <- format(wday(Data$Date.Time), label = T)

class(Data$day_of_the_week)


# Convert hour/minute/seconds to factor
Data$hour <- factor(hour(hms(Data$Time)))
Data$minute <- factor(minute(hms(Data$Time)))
Data$seconds <- factor(second(hms(Data$Time)))

head(Data)


# Grouping the Data with respect to hour
hour_data <- Data %>% 
             group_by(hour) %>% 
             summarise(Total = n())
datatable(Hour)


# Visualising the hour_data, we just created 
# According to the graph most operations happens during 15 to 21 hours, means 3 to 9 PM 
ggplot(hour_data, aes(hour, Total)) +
  geom_bar(stat = "identity", fill = "black", col = "blue") +
  ggtitle("Trips By Hours") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma) 



# Grouping the data with respect to month and hour 
month_hour_data <- Data %>% 
                   group_by(month, hour) %>% 
                   summarise(Total = n())

datatable(month_hour_data)


# Visualising the month_hour_data, we just created 
# According to the graph september month has more customer than the other month  
ggplot(month_hour_data, aes(hour, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hours & Month") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma)  



# Grouping the data with respect to september month  
septem_hour_data <- Data %>% 
                    group_by(hour, month) %>% 
                    filter(month == "Sep") %>% 
                    summarise(Total = n())



# Visualising the septem_hour_data, we just created 
ggplot(septem_hour_data, aes(hour, Total, fill = hour)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hours & Month for September") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma)
  


# Grouping the data with respect to april month
apr_hour_data <- Data %>% 
  group_by(hour, month) %>% 
  filter(month == "Apr") %>% 
  summarise(Total = n())


# Visualising the septem_hour_data, we just created 
ggplot(apr_hour_data, aes(hour, Total, fill = hour)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hours & Month for April") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma)   



# Group the data with respect to Day
day_data <- Data %>% 
            group_by(day) %>% 
            summarise(Total = n())

datatable(day_data)


# Visualising the day_data, we just created 
ggplot(day_data, aes(day, Total, fill = hour)) + 
  geom_bar(stat = "identity", fill = "#0D97ac", col = "black") +
  ggtitle("Trips by Day") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma) 



# Group the data with respect to month
monthly_data <- Data %>% 
  group_by(month) %>% 
  summarise(Total = n())

datatable(monthly_data)


# Visualising the monthly_data, we just created 
# September month has more sales than the other, it is because of boosted marketing in september
ggplot(monthly_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "identity", fill = "#0D97ac", col = "black") +
  ggtitle("Trips by Month") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = comma) 


# Visualising the Base column
# Base B02512 & B02764 are not much active or profitable because of lower sales or few no. of customers from there 
ggplot(Data, aes(Base)) +
  geom_bar(fill = "#d55534") +
  ggtitle("Trip by Bases") +
  scale_y_continuous(labels = comma) 


# Visualising the Base & month column 
# Dodge overlap one column into other side by side 
# In BASE B02682 we dont have much profit from there but in month of september after the marketing suddenly trips started increasing 
# Boosted marketing helps increasing sales in BASE B02598 & B02682
ggplot(Data, aes(Base, fill = month)) +
  geom_bar(position = "dodge") +
  ggtitle("Trips by Base and Month")
  scale_y_continuous(labels = comma) 


# Group the data with respect to day & hour
day_and_hour <- Data %>% 
                group_by(day, hour) %>% 
                dplyr::summarize(Total = n())


# Visualising the day_and_hour, we just created 
# In the heatmap, the white portion represent more profit
# it means 6 to 8 in the morning and 4 to 9 in the evening are more profitable hours in a day 
ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(col = "white") +
  ggtitle("Heat map by hour and day")  
