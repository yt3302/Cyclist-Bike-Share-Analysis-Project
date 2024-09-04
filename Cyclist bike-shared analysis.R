### How do annual members and casual riders use Cyclistic bikes differently?

## Prepare
# Load library
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(dplyr) #helps clean data
library(tidyr) #helps clean data

# Import data
tripdata_202212 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202212-divvy-tripdata.csv")
tripdata_202301 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202301-divvy-tripdata.csv")
tripdata_202302 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202302-divvy-tripdata.csv")
tripdata_202303 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202303-divvy-tripdata.csv")
tripdata_202304 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202304-divvy-tripdata.csv")
tripdata_202305 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202305-divvy-tripdata.csv")
tripdata_202306 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202306-divvy-tripdata.csv")
tripdata_202307 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202307-divvy-tripdata.csv")
tripdata_202308 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202308-divvy-tripdata.csv")
tripdata_202309 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202309-divvy-tripdata.csv")
tripdata_202310 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202310-divvy-tripdata.csv")
tripdata_202311 <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 1/Raw Data/202311-divvy-tripdata.csv")

# Check columns name
colnames(tripdata_202212)
colnames(tripdata_202301)
colnames(tripdata_202302)
colnames(tripdata_202303)
colnames(tripdata_202304)
colnames(tripdata_202305)
colnames(tripdata_202306)
colnames(tripdata_202307)
colnames(tripdata_202308)
colnames(tripdata_202309)
colnames(tripdata_202310)
colnames(tripdata_202311)

# Check the structure
str(tripdata_202212)
str(tripdata_202301)
str(tripdata_202302)
str(tripdata_202303)
str(tripdata_202304)
str(tripdata_202305)
str(tripdata_202306)
str(tripdata_202307)
str(tripdata_202308)
str(tripdata_202309)
str(tripdata_202310)
str(tripdata_202311)

# Combine all dataset
all_trips <- bind_rows(
  tripdata_202212,
  tripdata_202301,
  tripdata_202302,
  tripdata_202303,
  tripdata_202304,
  tripdata_202305,
  tripdata_202306,
  tripdata_202307,
  tripdata_202308,
  tripdata_202309,
  tripdata_202310,
  tripdata_202311
)


## Process
# Numbers of rows
nrow(all_trips)

colnames(all_trips)

head(all_trips)

str(all_trips)

# Statistical summary of numeric data
summary(all_trips)

# Convert data type
all_trips <- all_trips %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )

str(all_trips)

# List the date, year, month, day into columns 
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") #"%A" returns full name of the day

colnames(all_trips)
head(all_trips)

# Calculate the difference in time into ride_length
all_trips <- all_trips %>%
  mutate(
    ride_length = difftime(ended_at, started_at, units = "mins")
  )

colnames(all_trips)
head(all_trips)
str(all_trips)

# Convert data type to numeric for calculation
all_trips$ride_length <- as.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length)

View(all_trips)

# Remove unnecessary data
all_trips2 <- all_trips[!(all_trips$ride_length <= 0 | all_trips$ride_length > 1440),]
all_trips2 <- drop_na(all_trips2)
summary(all_trips2)
View(all_trips2)

# Check duplicate data
duplicates <- duplicated(all_trips2)
num_duplicates <- sum(duplicates)
num_duplicates


## Analyze
# Summary statistics by user type
summary_by_type <- all_trips2 %>%
  group_by(member_casual) %>%
  summarise(
    Total_Rides = n(),
    Avg_Ride_Length = mean(ride_length, na.rm = TRUE),
    Median_Ride_Length = median(ride_length, na.rm = TRUE),
    Min_Ride_Length = min(ride_length, na.rm = TRUE),
    Max_Ride_Length = max(ride_length, na.rm = TRUE)
  )

print(summary_by_type)

# Aggregating ride counts by month and user type
monthly_trends <- all_trips2 %>%
  group_by(year, month, member_casual) %>%
  summarise(Total_Rides = n(), .groups = 'drop')
print(n=24, monthly_trends)
# Plot monthly ride trends in line plot
ggplot(monthly_trends, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = Total_Rides, color = member_casual)) +
  geom_line() +
  labs(title = "Monthly Ride Trends by User Type", x = "Month", y = "Total Rides") +
  theme_minimal()
# Plot monthly ride trends in bar plot
ggplot(monthly_trends, aes(x = month, y = Total_Rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monthly Ride Trends by User Type", x = "Month", y = "Total Rides") +
  theme_minimal()

# Aggregating ride counts by day of week and user type
day_of_week_trends <- all_trips2 %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  group_by(day_of_week, member_casual) %>%
  summarise(Total_Rides = n(), .groups = 'drop')
print(day_of_week_trends)
# Plot day of week usage
ggplot(day_of_week_trends, aes(x = day_of_week, y = Total_Rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Rides by Day of Week and User Type", x = "Day of Week", y = "Total Rides") +
  theme_minimal()

# Aggregating average ride lengths by month and user type
monthly_ride_length <- all_trips2 %>%
  group_by(year, month, member_casual) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop')
print(n=24, monthly_ride_length)
# Plot monthly ride length in line plot
ggplot(monthly_ride_length, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = average_ride_length, color = member_casual)) +
  geom_line() +
  labs(title = "Monthly Ride Length by User Type", x = "Month", y = "Average Ride Length") +
  theme_minimal()
# Plot monthly ride trends in bar plot
ggplot(monthly_ride_length, aes(x = month, y = average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monthly Ride Length by User Type", x = "Month", y = "Average Ride Length") +
  theme_minimal()

# Aggregating average ride lengths by day of week and user type
daily_ride_length <- all_trips2 %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  group_by(day_of_week, member_casual) %>%
  summarise(average_ride_length = mean(ride_length), .groups = 'drop')
print(daily_ride_length)
# Plot day of week ride length
ggplot(daily_ride_length, aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Ride Length by Day of Week and User Type", x = "Day of Week", y = "Average Ride Length") +
  theme_minimal()
