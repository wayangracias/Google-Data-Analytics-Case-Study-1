## Google Data Analytics Capstone Project: Case Study 1 - Cyclistic Bike Share

####loading libraries
```{r}
install.packages("tidyverse") #calculations
library(tidyverse) 
install.packages("lubridate") #dates
library(lubridate)
install.packages("hms") #time
library(hms)
install.packages("data.table") #exporting data frame
library(data.table)
install.packages("scales") #scales 
library(scales)
```

##### Loading the csv files
```{r}
sep21_df <- read.csv("sep21.csv")
oct21_df <- read.csv("oct21.csv")
nov21_df <- read.csv("nov21.csv")
dec21_df <- read.csv("dec21.csv")
jan22_df <- read.csv("jan22.csv")
feb22_df <- read.csv("feb22.csv")
mar22_df <- read.csv("mar22.csv")
apr22_df <- read.csv("apr22.csv")
may22_df <- read.csv("may22.csv")
june22_df <- read.csv("june22.csv")
july22_df <- read.csv("july22.csv")
aug22_df <- read.csv("aug22.csv")
```

##### Combine all the csv files into one year data frame
```{r}
cyclistic_df <- rbind(sep21_df, oct21_df, nov21_df, dec21_df, jan22_df, feb22_df, mar22_df, apr22_df, may22_df, june22_df, july22_df, aug22_df)
```

##### Remove individual month data frames to clear up space in environment

```{r}
remove(sep21_df, oct21_df, nov21_df, dec21_df, jan22_df, feb22_df, mar22_df, apr22_df, may22_df, june22_df, july22_df, aug22_df)
```

##### Create new data frame to contain new columns

```{r}
cyclistic_date <- cyclistic_df
```

##### Changing datatype of started_at and ended_at from character to datetime

```{r}
cyclistic_date$started_at <- as.POSIXct(cyclistic_date$started_at, format = "%Y-%m-%d %H:%M:%S")
cyclistic_date$ended_at <- as.POSIXct(cyclistic_date$ended_at, format = "%Y-%m-%d %H:%M:%S")
```

##### Calculate ride length by subtracting ended_at time from started_at time and converted it to minutes

```{r}
cyclistic_date$ride_length <- difftime(cyclistic_date$ended_at, cyclistic_date$started_at, units = "mins")
```

##### Converting ride_length datataype from difftime to numeric
```{r}
cyclistic_date$ride_length <- as.numeric(cyclistic_date$ride_length)
```


##### Create columns for date, day of week, month, day, year, time, hour

```{r}
cyclistic_date$date <- as.Date(cyclistic_date$started_at)
```

##### Create columns for day of week

```{r}
cyclistic_date$day_of_week <- format(as.Date(cyclistic_date$date), "%A") 
```

#### create column for month_year
```{r}
cyclistic_data$month_year <- format(as.Date(cyclistic_data$date), "%B %Y")
```

##### Create columns for month, day, year

```{r}
cyclistic_date$month <- format(as.Date(cyclistic_date$date), "%B")
cyclistic_date$day <- format(as.Date(cyclistic_date$date), "%d")
cyclistic_date$year <- format(as.Date(cyclistic_date$date), "%Y")
```

##### create columns for time and hour

```{r}
cyclistic_date$time <- format(as.POSIXct(cyclistic_date$started_at), format = "%H:%M:%S")
cyclistic_date$hour <- format(as.POSIXct(cyclistic_date$started_at), format = "%H")
```

##### Create columns for different seasons

```{r}
cyclistic_date <- cyclistic_date %>%
  mutate(season =
           case_when(month == "March" ~ "Spring",
                     month == "April" ~ "Spring",
                     month == "May" ~ "Spring",
                     month == "June" ~ "Summer",
                     month == "July" ~ "Summer",
                     month == "August" ~ "Summer",
                     month == "September" ~ "Fall",
                     month == "October" ~ "Fall",
                     month == "November" ~ "Fall",
                     month == "December" ~ "Winter",
                     month == "January" ~ "Winter",
                     month == "February" ~ "Winter"))
```

##### Create column for different time of day: Night, Morning, Afternoon, Evening

```{r}
cyclistic_date <- cyclistic_date %>%
  mutate(time_of_day = 
           case_when(hour == "00" ~ "Night",
                     hour == "01" ~ "Night",
                     hour == "02" ~ "Night",
                     hour == "03" ~ "Night",
                     hour == "04" ~ "Night",
                     hour == "05" ~ "Night",
                     hour == "06" ~ "Morning",
                     hour == "07" ~ "Morning",
                     hour == "08" ~ "Morning",
                     hour == "09" ~ "Morning",
                     hour == "10" ~ "Morning",
                     hour == "11" ~ "Morning",
                     hour == "12" ~ "Afternoon",
                     hour == "13" ~ "Afternoon",
                     hour == "14" ~ "Afternoon",
                     hour == "15" ~ "Afternoon",
                     hour == "16" ~ "Afternoon",
                     hour == "17" ~ "Afternoon",
                     hour == "18" ~ "Evening",
                     hour == "19" ~ "Evening",
                     hour == "20" ~ "Evening",
                     hour == "21" ~ "Evening",
                     hour == "22" ~ "Evening",
                     hour == "23" ~ "Evening"))
```

##### **Data Cleaning**

##### Remove rows with NA values

```{r}
cyclistic_date <- na.omit(cyclistic_date)
```

##### Remove duplicate rows

```{r}
cyclistic_date <- distinct(cyclistic_date)
```

##### Remove rows which ride_length <=0

```{r}
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <= 0),]
```

##### Filter empty cells in start_station_name and end_station_name 

```{r}
cyclistic_date <- cyclistic_date %>% 
  filter(start_station_name!="", end_station_name!="")
```

##### Check summary statistics
```{r}
summary(cyclistic_date)
```

##### Finding outliers of ride length
```{r}
q1 <- 6.28
q3 <- 19.75
iqr <- q3-q1
iqr
```
#### lower and upper cutoffs of outlier
```{r}
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
```

##### remove outliers
```{r}
cyclistic_data <- subset(cyclistic_date, cyclistic_date$ride_length > lower & cyclistic_date$ride_length < upper)
```

##### Check summary statistics
```{r}
summary(cyclistic_data)
```


##### Save cleaned data in csv format

```{r}
write.csv(cyclistic_data, file = "cyclistic_v2.csv", row.names = FALSE)
```

##### Open cleaned data

```{r}
cyclistic_data <- read.csv("cyclistic_v2.csv")
```

##### Compare members and casual users

###### **mean**

```{r}
aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = mean)
```
###### **median**

```{r}
aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = median)
```
###### **max**

```{r}
aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = max)
```
###### **min**
```{r}
aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual, FUN = min)
```

###### **average ride time by each day for each members**

```{r}
aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual + cyclistic_data$day_of_week, FUN = mean)
```

#Data Visualization by using R

##number of rides by both members
```{r}
cyclistic_data %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_col() + 
  labs(title = "Number of Rides Between Members (Sep 2021 - Aug 2022)", x = "Membership Status", y = "Number of Rides")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = "."))+
  geom_text(aes(label = scales::comma(number_of_rides)), vjust = -0.2, size = 4)+
  theme(legend.position = "none")
```

##average duration by both members
```{r}
cyclistic_data %>% 
  group_by(member_casual) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  ggplot(aes(x = member_casual, y = average_duration, fill = member_casual)) +
  geom_col() + 
  labs(title = "Average Duration Between Members (Sep 2021 - Aug 2022)", x = "Membership Status", y = "Average Duration (mins)")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
  decimal.mark = "."))+
  geom_text(aes(label = sprintf("%0.2f", average_duration)), vjust = -0.2, size = 3)+
  theme(legend.position = "none")
```

##number of rides by rideable type

```{r}
cyclistic_data %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",decimal.mark = ".")) +
  labs(title = "Number of Rides by Rideable Type (Sep 2021 - Aug 2022)", x = "Rideable Type", y = "Number of Rides", fill = "Membership Status")+
  geom_text(aes(label = scales::comma(number_of_rides)), vjust = -0.2, size = 3)
```

#avg duration by rideable type
```{r}
cyclistic_data %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = average_duration, fill = member_casual)) + geom_col(position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = ".")) +
  labs(title = "Average Duration by Rideable Type (Sep 2021 - Aug 2022)", x = "Rideable Type", y = "Average Duration (mins)", fill = "Membership Status")+
  geom_text(aes(label = sprintf("%0.2f", average_duration)), vjust = -0.2, size = 3)
```

##Analyze ridership data by member type and weekday

###### **ordered day of week**
```{r}
cyclistic_data$day_of_week <- ordered(cyclistic_data$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) 
```
```{r}
aggregate(cyclistic_data$ride_length ~ cyclistic_data$member_casual + cyclistic_data$day_of_week, FUN = mean)
```

#number of rides by weekday
#Version 1
```{r}
cyclistic_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Total Number of Rides by Day of Week (Sep 2021 - Aug 2022)", x = "Day of Week", y = "Number of Rides", fill = "Membership Status") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                           decimal.mark = ".")) +
  geom_text(aes(label = scales::comma(number_of_rides)), vjust = -0.2, size = 2)
```

#version 2
```{r}
cyclistic_data %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week, rideable_type) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                  decimal.mark =".")) +
  labs(title = "Total Number of Rides by Day of Week (Sep 2021 - Aug 2022)", x = "Day of Week", y = "Number of Rides", fill = "Rideable Type") + facet_wrap(~member_casual)
```

##Avg Duration by day of week
#Version 1
```{r}
cyclistic_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Average Duration by Day of Week (Sep 2021 - Aug 2022)", x = "Day of Week", y = "Average Duration (mins)", fill = "Membership Status")+
  geom_text(aes(label = sprintf("%0.2f", average_duration)), vjust = -0.2, size =3)
```

#Version 2
```{r}
cyclistic_data %>%
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week, rideable_type) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = rideable_type)) + geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Average Duration by Day of Week (Sep 2021 - Aug 2022)", x = "Day of Week", y = "Average Duration (mins)", fill = 'Rideable Type')+
  facet_wrap(~member_casual)
```

#number of rides by month

##order months
```{r}
cyclistic_data$month <- ordered(cyclistic_data$month, levels = c("September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August"))
```

#version 1
```{r}
cyclistic_data %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Total Number of Rides by Month (Sep 2021 - Aug 2022)", x = "Month", y = "Number of Rides", fill = "Membership Status") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = ".")) +
  geom_text(aes(label = scales::comma(number_of_rides)), vjust = -0.2, size = 2)
```

#version 2
```{r}
cyclistic_data %>% 
  group_by(member_casual, month, rideable_type) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month, rideable_type) %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark =".")) +
  scale_x_discrete(labels = c('Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug'))+
  labs(title = "Total Number of Rides by Month (Sep 2021 - Aug 2022)", x = "Month", y = "Number of Rides", fill = "Rideable Type") + facet_wrap(~member_casual) +
  theme(panel.spacing = unit(1, "cm"))
```

#average duration by month
#version 1
```{r}
cyclistic_data %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Average Duration by Month (Sep 2021 - Aug 2022)", x = "Month", y = "Average Duration (mins)", fill = "Membership Status") +
  geom_text(aes(label = sprintf("%0.2f", average_duration)), vjust = -0.2, size = 2)
```

#version 2
```{r}
cyclistic_data %>% 
  group_by(member_casual, month, rideable_type) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month, rideable_type) %>% 
  ggplot(aes(x = month, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark =".")) +
  scale_x_discrete(labels = c('Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug'))+
  labs(title = "Average Duration by Month (Sep 2021 - Aug 2022)", x = "Month", y = "Average Duration (mins)", fill = "Rideable Type") + facet_wrap(~member_casual) +
  theme(panel.spacing = unit(1, "cm"))
```

#number of rides by season
#order season
```{r}
cyclistic_data$season <- ordered(cyclistic_data$season, levels = c("Fall", "Winter", "Spring", "Summer"))
```

#version 1
```{r}
cyclistic_data %>% 
  group_by(member_casual, season) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, season) %>% 
  ggplot(aes(x = season, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Total Number of Rides by Season (Sep 2021 - Aug 2022)", x = "Season", y = "Number of Rides", fill = "Membership Status") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = ".")) +
  geom_text(aes(label = scales::comma(number_of_rides)), vjust = -0.2, size = 3)
```

#version 2
```{r}
cyclistic_data %>% 
  group_by(member_casual, season, rideable_type) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, season, rideable_type) %>% 
  ggplot(aes(x = season, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark =".")) +
  labs(title = "Total Number of Rides by Season (Sep 2021 - Aug 2022)", x = "Season", y = "Number of Rides", fill = "Rideable Type") + facet_wrap(~member_casual) +
  theme(panel.spacing = unit(1, "cm"))
```

#average duration by season
#version 1
```{r}
cyclistic_data %>% 
  group_by(member_casual, season) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, season) %>% 
  ggplot(aes(x = season, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Average Duration by Season (Sep 2021 - Aug 2022)", x = "Season", y = "Average Duration (mins)", fill = "Membership Status") +
  geom_text(aes(label = sprintf("%0.2f", average_duration)), vjust = -0.2, size = 3)
```

#version 2
```{r}
cyclistic_data %>% 
  group_by(member_casual, season, rideable_type) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, season, rideable_type) %>% 
  ggplot(aes(x = season, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark =".")) +
  labs(title = "Average Duration by Season (Sep 2021 - Aug 2022)", x = "Season", y = "Average Duration (mins)", fill = "Rideable Type") + facet_wrap(~member_casual) +
  theme(panel.spacing = unit(1, "cm"))
```

#number of rides by hour
```{r}
cyclistic_data %>% 
  group_by(member_casual, hour, day_of_week) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hour, day_of_week) %>% 
  ggplot(aes(x = hour, y = number_of_rides, fill = member_casual)) +
  geom_col() +
  scale_x_continuous(breaks = seq(00,23,1))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark =".")) +
  labs(title = "Number of Rides During Each Week by Hour (Sep 2021 - Aug 2022)", x = "Hours of The Week", y = "Number of Rides", fill = "Membership Status") + facet_wrap(~day_of_week)+
  guides(shape = guide_legend(override.aes = list(size = 0.1)))+
  guides(color = guide_legend(override.aes = list(size = 0.1)))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 5))
```

#average duration by hour
```{r}
cyclistic_data %>% 
  group_by(member_casual, hour, day_of_week) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, hour, day_of_week) %>% 
  ggplot(aes(x = hour, y = average_duration, fill = member_casual)) +
  geom_col() +
  scale_x_continuous(breaks = seq(00,23,1))+
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark =".")) +
  labs(title = "Average Duration During Each Week by Hour (Sep 2021 - Aug 2022)", x = "Hours of The Week", y = "Average Duration (mins)", fill = "Membership Status") + facet_wrap(~day_of_week)+
  guides(shape = guide_legend(override.aes = list(size = 0.1)))+
  guides(color = guide_legend(override.aes = list(size = 0.1)))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(axis.text.x = element_text(size = 5)) 
```

#number of rides by time
```{r}
cyclistic_data %>% 
  group_by(member_casual, time_of_day) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, time_of_day) %>% 
  ggplot(aes(x = time_of_day, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Number of Rides by Time of Day (Sep 2021 - Aug 2022)", x = "Time of Day", y = "Number of Rides", fill = "Membership Status") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",",
                                                   decimal.mark = "."))
```

#average duration by time of day
```{r}
cyclistic_data %>% 
  group_by(member_casual, time_of_day) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, time_of_day) %>% 
  ggplot(aes(x = time_of_day, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.5) + 
  labs(title = "Average Duration by Time of Day (Sep 2021 - Aug 2022)", x = "Time of Day", y = "Average Duration (mins)", fill = "Membership Status") +
  geom_text(aes(label = sprintf("%0.2f", average_duration)), vjust = -0.2)
```

#references and inspirations: https://github.com/kellyjadams/google-capstone-project/blob/main/FinalAnalysis.R
