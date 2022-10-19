####Visualization Set-up####

## loading libraries
library(tidyverse)
library(skimr)
library(kableExtra)

options(scipen = 999)

##loading dataset
clean_bike_share <- read_csv("clean_bike_share.csv")

str(clean_bike_share)

##ratio of total members vs casual
ratio_pie <- clean_bike_share %>% 
  group_by(member_casual) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent = n / sum(n)) %>% 
  arrange(percent) %>% 
  mutate(labels = scales::percent(percent))

hsize <- 1.5

ratio_pie <- ratio_pie %>% 
  mutate(x = hsize)

ggplot(ratio_pie, aes(x = hsize, y = percent, fill = member_casual)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c(1, "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "User Type")) +
  scale_fill_manual(values = c("#deed05", "#4095a5")) +
  coord_polar(theta = "y") + 
  xlim(c(0.2, hsize + 0.5)) +
  theme_void()

##by ride type
rideable_type <- table(rideable_type = clean_bike_share$rideable_type, 
                   member_type = clean_bike_share$member_casual) %>% 
  as.data.frame() %>% 
  pivot_wider(rideable_type, names_from = member_type, values_from = Freq) %>% 
  mutate(total = casual + member, percent_casual = round((casual / total)*100, 0), 
         percent_member = round((member / total)*100), 0) %>%
  select(rideable_type, percent_casual, percent_member)

##members vs casual by quarter - probably delete/not useful due to data starting in August
ggplot(clean_bike_share, aes(fill = member_casual, x = quarter)) +
  geom_bar(position = 'dodge')

##Factor relevel to organize as desired by weeks and months
clean_bike_share <- 
  mutate(clean_bike_share, 
         weekday = fct_relevel(weekday, c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                          "Thursday", "Friday", "Saturday")))

clean_bike_share <- 
  mutate(clean_bike_share, 
         month = fct_relevel(month, c("August", "September", "October", "November",
                                      "December", "January", "February", "March",
                                      "April", "May", "June", "July")))

##ratio of members vs casual by month
ggplot(clean_bike_share, aes(fill = member_casual, x = month)) +
  guides(fill = guide_legend(title = "User Type")) +
  scale_fill_manual(values = c("#deed05", "#4095a5")) +
  geom_bar(position = 'dodge') +
  theme_bw()

monthly_ratio <- table(month = clean_bike_share$month, 
                        member_type = clean_bike_share$member_casual) %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = member_type, values_from = Freq) %>% 
  mutate(total = casual + member, percent_casual = round((casual / total)*100), 
         percent_member = round((member / total)*100)) %>%
  select(month, percent_casual, percent_member)

summer_avg <- monthly_ratio[c("1", "2", "11", "12"), ]

mean(summer_avg$percent_casual)

mean(summer_avg$percent_member)

winter_avg <- monthly_ratio[c(4:7), ]

mean(winter_avg$percent_casual)

mean(winter_avg$percent_member)

##ratio of members vs casual by weekday
ggplot(clean_bike_share, aes(fill = member_casual, x = weekday)) +
  guides(fill = guide_legend(title = "User Type")) +
  scale_fill_manual(values = c("#deed05", "#4095a5")) +
  geom_bar(position = 'dodge') +
  labs(x = "Day of Week", y = "Number of Riders") +
  theme_bw()

weekly_ratio <- table(weekday = clean_bike_share$weekday, 
                       member_type = clean_bike_share$member_casual) %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = member_type, values_from = Freq) %>% 
  mutate(total = casual + member, percent_casual = round((casual / total)*100), 
         percent_member = round((member / total)*100)) %>%
  select(weekday, percent_casual, percent_member)

weekday_avg <- weekly_ratio[c(2:6), ]

mean(weekday_avg$percent_member)

weekend_avg <- weekly_ratio[c("1", "7"), ]

##average ride length
trip_length <- clean_bike_share %>% 
  select(member_casual, ride_duration, weekday) %>% 
  arrange(desc(member_casual))

trip_avg_weekday <- aggregate(ride_duration ~ member_casual + weekday, 
                              trip_length, mean)

ggplot(trip_avg_weekday, aes(fill = member_casual, x = weekday, y = ride_duration)) +
  guides(fill = guide_legend(title = "User Type")) +
  scale_fill_manual(values = c("#deed05", "#4095a5")) +
  geom_col(position = 'dodge') +
  theme_bw()

trip_avg_wide <- pivot_wider(trip_avg_weekday, names_from = member_casual, 
                                   values_from = ride_duration)

##sum by rider type of start time by hour
trip_start <- clean_bike_share %>% 
  select(member_casual, started_at, weekday) %>% 
  mutate(started_at = format(started_at, format = "%H"))

ggplot(trip_start, aes(fill = member_casual, x = started_at)) +
  guides(fill = guide_legend(title = "User Type")) +
  scale_fill_manual(values = c("#deed05", "#4095a5")) +
  geom_bar(position = "dodge") +
  theme_bw()

trip_start_sum <- aggregate(trip_start$started_at, 
                            by = list(hour = trip_start$started_at, member_type = trip_start$member_casual), 
                            FUN = length)

trip_start_wide <- trip_start_sum %>%  
  pivot_wider(names_from = member_type, values_from = x) %>%
  mutate(total = casual + member)

##popular stations by start
pop_station_start <- table(start_station = clean_bike_share$start_station_name, 
                           member_type = clean_bike_share$member_casual)

popular_start_stations <- table(start_station = clean_bike_share$start_station_name, 
                                member_type = clean_bike_share$member_casual) %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = member_type, values_from = Freq)

colSums(popular_start_stations != 0)##number of start stations used by casuals vs members

popular_start_casual <- popular_start_stations %>% 
  filter(casual > 10000) %>% 
  select(start_station, casual) %>% 
  arrange(desc(casual))

popular_start_member <- popular_start_stations %>% 
  filter(member > 10000) %>% 
  select(start_station, member) %>% 
  arrange(desc(member))

popular_start_shared <- merge(x = popular_start_casual, y = popular_start_member) %>% 
  mutate(total = casual + member) %>% 
  arrange(desc(total))

##popular stations by end
pop_station_end <- table(end_station = clean_bike_share$end_station_name, 
                           member_type = clean_bike_share$member_casual)

popular_end_casual <- pop_station_end %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = member_type, values_from = Freq) %>% 
  filter(casual > 10000) %>% 
  select(end_station, casual) %>% 
  arrange(desc(casual))
 
pop_stations_casual <- popular_start_casual %>% 
  rename(station_name = start_station)

pop_stations_end <- popular_end_casual %>% 
  rename(station_name = end_station)

popular_casual_combined <- merge(x = pop_stations_casual, 
                                 y = pop_stations_end, all = TRUE) %>% 
  aggregate(casual ~ station_name, sum) %>% 
  filter(casual > 20000) %>% 
  arrange(desc(casual))