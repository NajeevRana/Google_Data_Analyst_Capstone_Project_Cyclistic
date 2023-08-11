To determine how the two types of customers - "Casual" and "Member" use the service differently I used 12 months of historical/Old data for a bike-share company. Throughout this project, I used Excel and Rstudio.

Table of Content
-----------------
•	Ask
•	Prepare
•	Process
•	Analyze
•	Share
•	Act

Ask
-----------------------------------------------------------------------------------------------
Business Task: Determine how annual members and casual riders use cyclistic bikes differently?

Prepare
------------------------------------------------------------------------------------------------
The data for this analysis is provided by Google for the project in the form of CSV files. The data does not contain any sensitive information and is public. The data used is also the most current available.

Process
-------------------------------------------------------------------------------------------------
During this phase, The data was processed for analysis and I used the R programming language. I could use the SQL but I want to use the built-in feature of R to document(R markdown) and ggplot2 to visualize. Using Excel and Google Sheets are not many useful tools when working with thousands of rows of data elements.

Processing steps: -
--------------------
Load packages used for analysis
--------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(anytime)
library(stringr)
library(janitor)

## Load data into R for analysis

df1<- read.csv("/google-data-analytics-capstone-cyclistic-data/202207-divvy-tripdata.csv")
df2<- read.csv("/google-data-analytics-capstone-cyclistic-data/202208-divvy-tripdata.csv")
df3<- read.csv("/google-data-analytics-capstone-cyclistic-data/202209-divvy-tripdata.csv")
df4<- read.csv("/google-data-analytics-capstone-cyclistic-data/202210-divvy-tripdata.csv")
df5<- read.csv("/google-data-analytics-capstone-cyclistic-data/202211-divvy-tripdata.csv")
df6<- read.csv("/google-data-analytics-capstone-cyclistic-data/202212-divvy-tripdata.csv")
df7<- read.csv("/google-data-analytics-capstone-cyclistic-data/202301-divvy-tripdata.csv")
df8<- read.csv("/google-data-analytics-capstone-cyclistic-data/202302-divvy-tripdata.csv")
df9<- read.csv("/google-data-analytics-capstone-cyclistic-data/202303-divvy-tripdata.csv")
df10<- read.csv("/google-data-analytics-capstone-cyclistic-data/202304-divvy-tripdata.csv")
df11<- read.csv("/google-data-analytics-capstone-cyclistic-data/202305-divvy-tripdata.csv")
df12<- read.csv("/google-data-analytics-capstone-cyclistic-data/202306-divvy-tripdata.csv")

Make sure all the files have the same number of columns and the same data type for each column before merging the data in one file else integration won't be possible.
##compare datasets columns

compare_df_cols(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

Once we check and find everything is in order, we can go ahead and integrate the files into one file/Dataset
##Bind Datasets

trek<- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

Character and numeric data were found on the "start_station_id" and "end_station_id columns so remove those to improve the integrity.
## Removing "start_station_id" and "end_station_id" columns because both character and Numeric data enter in those columns

trek<- trek %>%
  select(-c(start_station_id, end_station_id))
  
The start and End dates were in character format. We need to change that to a date-time format.
##Change the stated_at and ended_at datatype to date-time format.

trek$started_at <- anytime(trek$started_at, tz="America/Chicago")
trek$ended_at <- anytime(trek$ended_at, tz="America/Chicago")


##added few new columns "week_of_days", "date", "day", "month", "year", "trip_duration"
trek<- mutate(trek,week_of_days = weekdays(started_at))
trek<- mutate(trek,date = as.Date(started_at))
trek<-mutate(trek,day = format(started_at,"%d"))
trek<- mutate(trek,month= format(started_at,"%m"))
trek<- mutate(trek,year= format(started_at,"%y"))
trek<- mutate(trek,trip_durattion= difftime(ended_at,started_at,units = "mins"))

##verify the column added
colnames(trek)

## Remove the column started_at and ended_at
trek<- subset(trek, select = -c(started_at,ended_at))

##understanding of data
str(trek)

##changing trip_duration to numeric
trek$trip_durattion <- as.numeric(as.character(trek$trip_durattion))

## confirming trip_duration is numeric
is.numeric(trek$trip_durattion)

## Data cleaning 
## Remove unnecessary white space
trek %>% mutate(across(where(is.character), str_squish))

##check NA for data set 
sum(is.na(trek))

##Remove rows with NA
trek <- na.omit(trek)

##Verify removal of rows with NA
sum(is.na(trek))

## check duplicate ride_id
sum(duplicated(trek$ride_id))

##check for rows where trip_durattion is less than 0
filter(trek,trip_durattion<0)

##remove rows where trip_durattion is less than 0
trek<-subset(trek,trip_durattion>0)

##check the data for a better understanding
glimpse(trek)

##Analyze the data
## Statistical summary of data
summary(trek$trip_durattion)

Min.    1st Qu.   Median    Mean     3rd Qu.     Max. 
0.02     5.50     9.70      15.33    17.28      32035.45 

##Arrange week of days in order
trek$week_of_days<- ordered(trek$week_of_days,levels=c("Sunday","Monday","Tuesday","Wednesday","Thrusday","Friday","Saturday"))

##Compare mean, median, max ride duration in minutes for members and casual users 

aggregate(trek$trip_durattion ~ trek$member_casual + trek$rideable_type,FUN = mean)
aggregate(trek$trip_durattion ~ trek$member_casual + trek$rideable_type,FUN= median)
aggregate(trek$trip_durattion ~ trek$member_casual + trek$rideable_type,FUN = max)

##Analyzed ridership data by user type and weekday - average duration in minutes 

trek %>% mutate(week_of_days = wday(date,label = TRUE)) %>% 
  group_by(member_casual, week_of_days) %>%
  summarise (number_of_rides = n()
             ,average_duration = mean(trip_durattion)) %>%
  arrange(member_casual,week_of_days)

Casual and member riders are beginning their trips to determine if there are any meaningful differences in their behavior that can be included in the marketing plan. We will look at trips by season.
##Generate seasonal trip chart by rider classification

trek %>%
  ggplot(aes(x = date,fill=member_casual,color=member_casual)) +
  geom_area(stat = 'bin', position = 'identity',alpha = 0.5) + 
  ggtitle("Seasonal Usage By Rider Classification") +
  xlab('Date')+
  ylab('Number of Rides') +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_bw()

  ![image](https://github.com/NajeevRana/Google_Data_Analyst_Capstone_Project_Cyclistic/assets/140980104/c8218c58-04a9-4245-9c36-f77953efa34b)

The number of rides for casual riders was much higher than for member riders on Saturday and Sunday. The number of member riders stayed mostly constant from Sunday – Saturday. It seemed like casual riders used the service more for leisure and member riders use it for daily transportation to and from work.

##Generate day of the week trip view and user type by rider classification

trek%>%
  ggplot(aes(x= wday(date,label=TRUE),fill = member_casual))+
  geom_bar(position= 'dodge')+
  ggtitle('Days of Week Usage by Rider and user type') +
  geom_text(stat = 'count',
            aes(label = scales::comma(..count..)), 
            position = position_dodge(width = 0.85),
            hjust = 1.25,
            angle = 90) +
  xlab('Day of Week') + 
  ylab('Number of Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  theme_bw()

![image](https://github.com/NajeevRana/Google_Data_Analyst_Capstone_Project_Cyclistic/assets/140980104/d6c7c0c8-c9d0-4c81-9206-c8b4795ea9ea)


##Visualized the number of rides by month and user type.

trek %>% 
  mutate(month) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") +
  ggtitle(label = "Number of Rides by Month and User Type") +
  xlab('Months') + 
  ylab('Number of Rides') +
  scale_x_discrete(labels = month.abb, expand = expansion(add = c(0,0))) +
  scale_y_continuous(label = scales::label_comma())
![image](https://github.com/NajeevRana/Google_Data_Analyst_Capstone_Project_Cyclistic/assets/140980104/080fcee8-029d-403c-a78d-ea019c86938b)

Showed the average ride durations by day (Sunday-Saturday) for casual and member riders. Overall, casuals ride duration were about two times members each day of the week.
##Visualized average duration by weekday and user type.

trek %>% 
  mutate(week_of_days = wday(date, label = TRUE)) %>% 
  group_by(member_casual, week_of_days) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(member_casual, week_of_days)  %>% 
  ggplot(aes(x = week_of_days, y = average_duration, fill = member_casual)) +
  ggtitle(label = "Average Duration by Weekday and User Type") +
  geom_col(position = "dodge") +
  xlab('Day Of Week') + 
  ylab('Average Duration') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0)))

  ![Average Duration by Weekday and User Type](https://github.com/NajeevRana/Google_Data_Analyst_Capstone_Project_Cyclistic/assets/140980104/2dde9fdd-38c5-4af8-a198-cbd99db29cd6)

Showed the number of rides in 2023 was 2238959 for casual and 3534116 member riders totaling 5,773,075. There was a difference of 1295157 rides between casual and member riders. Therefore, casual riders were aware of the Cyclistic bike-sharing program and used it for their daily transportation needs too. This discovery showed that there is an untapped population of casual riders for Cyclistic to expand its membership base and facilitate future growth.
## Visualized the number of rides by year and user type

trek %>% 
  mutate(year = "2023") %>% 
  group_by(member_casual, year) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(member_casual, year)  %>% 
  ggplot(aes(x = year, y = number_of_rides, fill = member_casual)) +
  ggtitle(label = "Number Of Rides by Year and User Type") +
  geom_col(position = "dodge") +
  xlab('Year') + 
  ylab('Number_Of_Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0))) +
  geom_text(aes(label = number_of_rides), hjust = .5, vjust = 0, position = position_dodge(.9))

![Number Of Rides by Year and User Type](https://github.com/NajeevRana/Google_Data_Analyst_Capstone_Project_Cyclistic/assets/140980104/6226c9cf-e146-4d63-a705-dd6a1f3e26e6)


Showed the number of rides in 2023 by bike type was 2492097 (classic bike), 138854 (docked bike), and 3142124 (electric bikes) for casual and member riders. Both casual and member riders prefer classic bikes.
## Visualized the number of rides by year and bike type

trek %>% 
  mutate(year = "2023") %>% 
  group_by(rideable_type, year) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(rideable_type, year)  %>% 
  ggplot(aes(x = year, y = number_of_rides, fill = rideable_type)) +
  ggtitle(label = "Number of Rides by Year and Bike Type") +
  geom_col(position = "dodge") +
  xlab('Year') + 
  ylab('Number_Of_Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0))) +
  geom_text(aes(label = number_of_rides), hjust = .5, vjust = -.1, position = position_dodge(.9))

  ![Number of Rides by Year and Bike Type](https://github.com/NajeevRana/Google_Data_Analyst_Capstone_Project_Cyclistic/assets/140980104/55478522-73fa-4876-909a-d46b202f1946)


##Visualized the number of rides by weekday and bike type

trek %>% 
  mutate(week_of_days = wday(date, label = TRUE)) %>% 
  group_by(rideable_type, week_of_days) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_durattion)) %>% 
  arrange(rideable_type, week_of_days)  %>% 
  ggplot(aes(x = week_of_days, y = number_of_rides, fill = rideable_type)) +
  ggtitle(label = "Number of Rides by Week of Day and Bike Type") +
  geom_col(position = "dodge") +
  xlab('Week Of Day') + 
  ylab('Number_Of_Rides') +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_discrete(expand = expansion(add = c(0, 0)))

![Number of Rides by Week of Day and Bike Type](https://github.com/NajeevRana/Google_Data_Analyst_Capstone_Project_Cyclistic/assets/140980104/37c37ff3-e485-4d1a-bfdc-4047bee2875e)

