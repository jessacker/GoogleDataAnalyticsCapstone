####Importing Process####

## loading libraries
library(tidyverse)
library(skimr)
library(janitor)

##loading raw data
df01 <- read_csv("Cyclistic/Original Data/202108-divvy-tripdata.csv")
df02 <- read_csv("Cyclistic/Original Data/202109-divvy-tripdata.csv")
df03 <- read_csv("Cyclistic/Original Data/202110-divvy-tripdata.csv")
df04 <- read_csv("Cyclistic/Original Data/202111-divvy-tripdata.csv")
df05 <- read_csv("Cyclistic/Original Data/202112-divvy-tripdata.csv")
df06 <- read_csv("Cyclistic/Original Data/202201-divvy-tripdata.csv")
df07 <- read_csv("Cyclistic/Original Data/202202-divvy-tripdata.csv")
df08 <- read_csv("Cyclistic/Original Data/202203-divvy-tripdata.csv")
df09 <- read_csv("Cyclistic/Original Data/202204-divvy-tripdata.csv")
df10 <- read_csv("Cyclistic/Original Data/202205-divvy-tripdata.csv")
df11 <- read_csv("Cyclistic/Original Data/202206-divvy-tripdata.csv")
df12 <- read_csv("Cyclistic/Original Data/202207-divvy-tripdata.csv")

##view and compare data frames
str(df01)

compare_df_cols(df01, df02, df03, df04, df05, df06, 
                df07, df08, df09, df10, df11, df12)

##combine into single data frame
raw_bike_share <- rbind(df01, df02, df03, df04, df05, df06, 
                    df07, df08, df09, df10, df11, df12)

write_csv(raw_bike_share, "raw_bike_share.csv")

##view raw stats of single DF and write to csv
raw_bike_share_summary <- skim_without_charts(raw_bike_share)

write_csv(raw_bike_share_summary, "raw_bike_share_summary.csv")

##remove original data frames to save memory resources
rm(df01, df02, df03, df04, df05, df06, df07, df08, df09, df10, df11, df12)

####Cleaning Process####

##check and remove NAs
clean_bike_share <- na.omit(raw_bike_share)

clean_bike_share_summary <- skim_without_charts(clean_bike_share)

write_csv(clean_bike_share_summary, "clean_bike_share_summary.csv")

na_rows <- raw_bike_share %>% 
  filter_all(any_vars(is.na(.)))

##test against raw data from january to see if blank values were removed
filter(clean_bike_share, ride_id == "857B71104B437577" | 
                      ride_id == "A468B05CD592BA68")

##no duplicates to remove - checked number of rows vs unique values in clean ride_id

##create ride duration and remove negative values
clean_bike_share <- mutate(clean_bike_share, 
                             ride_duration = as.numeric(difftime(ended_at, started_at, units = "mins")))

negative_trip_durations <- clean_bike_share %>% 
  select(ride_id, rideable_type, started_at, ended_at, member_casual, ride_duration) %>% 
  filter(ride_duration <= 0)

clean_bike_share <- clean_bike_share %>% 
  filter(ride_duration > 0)

##create week, month, and quarter columns
clean_bike_share <- clean_bike_share %>% 
  mutate(weekday = weekdays(started_at), month = months(started_at))

clean_bike_share <- clean_bike_share %>% 
  mutate(quarter = case_when(
    month %in% c("January", "February", "March") ~ "Q1",
    month %in% c("April", "May", "June") ~ "Q2",
    month %in% c("July", "August", "September") ~ "Q3",
    TRUE ~ "Q4"))

##comparing ride_duration values with raw data to see if dropping NAs significantly altered data
raw_ride_duration <- mutate(raw_bike_share, 
                     ride_duration = as.numeric(difftime(ended_at, started_at, units = "mins"))) %>%
  select(ride_duration) %>% 
  filter(ride_duration > 0)

summary(raw_ride_duration)
summary(clean_bike_share$ride_duration)

##save clean dataset
write_csv(clean_bike_share, "clean_bike_share.csv")
