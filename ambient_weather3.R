#Kiara Acevedo Martinez
#kiaraacevedo@intermetro.edu
#Started: 7/1/2024
#Meteorological data from the dates of 07 June 2023 to 31 August 2023

####Load libraries####
library(tidyverse)
library(patchwork)
library(psych)

####Load and wrangle data####
url <- "https://raw.githubusercontent.com/kacev1/ambient_weather/main/ambient-weather-20230607-20230831-2.csv" #data from github

data_met <- read_csv(url) %>% 
  rename(date = Date, #Changed names of columns for easier coding
         simple_date = "Simple Date", out_temp = "Outdoor Temperature (°C)", 
         feels_like = "Feels Like (°C)", dew_point = "Dew Point (°C)",
         wind_speed = "Wind Speed (km/hr)", wind_gust = "Wind Gust (km/hr)",
         max_daily_gust = "Max Daily Gust (km/hr)", wind_direction = "Wind Direction (°)",
         hourly_rain = "Hourly Rain (mm/hr)", event_rain = "Event Rain (mm)",
         daily_rain = "Daily Rain (mm)", weekly_rain = "Weekly Rain (mm)",
         month_rain = "Monthly Rain (mm)", total_rain = "Total Rain (mm)",
         rel_pressure = "Relative Pressure (mmHg)", humidity = "Humidity (%)",
         uv_radiation_index = "Ultra-Violet Radiation Index", solar_radiation = "Solar Radiation (W/m^2)",
         in_temp = "Indoor Temperature (°C)", in_humidity = "Indoor Humidity (%)",
         abs_pressure = "Absolute Pressure (mmHg)", out_battery = "Outdoor Battery",
         in_feels_like = "Indoor Feels Like (°C)", in_dew_point = "Indoor Dew Point (°C)") %>%
  #mutate(month = month(simple_date, label = T)) %>% #Created month, week, time, time of day, and date only columns
  mutate(month = ifelse(month(simple_date) == 6, "Jun",
                        ifelse(month(simple_date) == 7, "Jul",
                               ifelse(month(simple_date) == 8, "Aug","OTHER")))) %>%
  mutate(week_year = week(simple_date)) %>% 
  mutate(time = format(simple_date, "%H:%M:%S"),
         time_day = case_when(time >= "06:30:00" & time <= "18:30:00" ~ "am",TRUE ~ "pm")) %>%
  mutate(date_only = as.Date(simple_date)) %>% #changed variable to date class
  mutate(time = parse_time(time)) %>% #changed variable to HMS class
  #drop_na() %>%   #258 rows were removed
  select(-c(out_battery, date)) #removed date and outdoor battery columns

data_met <- data_met %>%
  group_by(week_year) %>%
  mutate(
    start_date = min(date_only),
    end_date = max(date_only),
    week_date_range = paste(format(start_date, "%B %d"), "-", format(end_date, "%d"))
  ) %>%
  ungroup() %>%
  mutate(week_date_range = factor(week_date_range, levels = unique(week_date_range[order(week_year)]))) %>%
  select(-c(start_date, end_date))

#####Descriptive statistics
des_data <- data_met %>%
  select(where(is.numeric)) %>%
  select(-week_year) %>%
  describe()
des_data

des_month_data <- data_met %>%
  select(where(is.numeric)) %>%
  select(-week_year) %>%
  describeBy(group=data_met$month)
des_month_data

des_month_timeday_data <- data_met %>%
  select(where(is.numeric)) %>%
  select(-week_year) %>%
  describeBy(group=data_met$month + data_met$time_day)
des_month_timeday_data

des_week_data <- data_met %>%
  select(where(is.numeric)) %>%
  select(-week_year) %>%
  describeBy(group=data_met$week_year)
des_week_data


####Box plot of outdoor temperature by week####
ggplot(data_met, aes(x = week_date_range, y = out_temp)) +
  geom_boxplot(aes(group = week_date_range)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Week Date Range", y = "Outdoor Temperature (°C)", title = "Outdoor Temperature by Week")

####Facet wrap am & pm####
ggplot(data_met, aes(x = week_date_range, y = out_temp)) +
  geom_boxplot(aes(group = week_date_range)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Week Date Range", y = "Outdoor Temperature (°C)", title = "Outdoor Temperature by Week and Time of Day") +
  facet_wrap(~ time_day)

#####Weekly temperature by am & pm####
ggplot(data_met, aes(x = week_date_range, y = out_temp, color = time_day)) +
  geom_boxplot(aes(group = interaction(week_date_range, time_day))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Week Date Range", y = "Outdoor Temperature (°C)", color = "Time of Day", title = "Outdoor Temperature by Week and Time of Day")
#w/ averages
ggplot(data_met, aes(x = week_date_range, y = out_temp, color = time_day)) +
  geom_boxplot(aes(group = interaction(week_date_range, time_day))) +
  stat_summary(aes(group = time_day), fun = mean, geom = "point", shape = 4, size = 3, color = "black", position = position_dodge(width = 0.75)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Week Date Range", y = "Outdoor Temperature (°C)", color = "Time of Day", title = "Outdoor Temperature by Week and Time of Day")

