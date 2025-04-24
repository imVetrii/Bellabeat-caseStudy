# Load required packages

library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(skimr)

# Load data (csv files)
daily_activity <- read_csv("New folder/OneDrive/Documents/bellabeatCaseStudy/dailyActivity_merged.csv")
sleep_day <- read_csv("New folder/OneDrive/Documents/bellabeatCaseStudy/sleepDay_merged.csv")
hourly_steps <- read_csv("New folder/OneDrive/Documents/bellabeatCaseStudy/hourlySteps_merged.csv")
weight_logInfo <- read_csv("New folder/OneDrive/Documents/bellabeatCaseStudy/weightLogInfo_merged.csv")
heart_rate <- read_csv("New folder/OneDrive/Documents/bellabeatCaseStudy/heartrate_seconds_merged.csv")

### Data Cleaning and Preparation..
# Convert dates to proper format

daily_activity <- daily_activity %>%
  mutate(ActivityDate = mdy(ActivityDate))

sleep_day <- sleep_day %>%
  mutate(SleepDay = mdy_hms(SleepDay)) %>%
  mutate(SleepDate = as.Date(SleepDay))

hourly_steps <- hourly_steps %>%
  mutate(ActivityHour = mdy_hms(ActivityHour))

heart_rate <- heart_rate %>%
  mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p")) # Convert timestamp

# Check for missing values
skim(daily_activity)
skim(sleep_day)

## Number of unique users in each dataset
cat("Daily activity users:", n_distinct(daily_activity$Id), "\n")
cat("Sleep data users:", n_distinct(sleep_day$Id), "\n")
cat("Hourly steps users:", n_distinct(hourly_steps$Id), "\n")

# Average days tracked per user
daily_activity %>%
  group_by(Id) %>%
  summarise(DaysTracked = n()) %>%
  summarise(AvgDaysTracked = mean(DaysTracked))

# Daily step distribution
ggplot(daily_activity, aes(x = TotalSteps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue") +
  labs(title = "Distribution of Daily Steps",
       x = "Total Steps",
       y = "Count") +
  theme_minimal()

# Activity intensity throughout the day
hourly_steps %>%
  mutate(Hour = hour(ActivityHour)) %>%
  group_by(Hour) %>%
  summarise(AvgSteps = mean(StepTotal)) %>%
  ggplot(aes(x = Hour, y = AvgSteps)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Average Steps Throughout the Day",
       x = "Hour of Day",
       y = "Average Steps") +
  theme_minimal()


##Sleep Patterns Analysis
# Sleep duration distribution
ggplot(sleep_day, aes(x = TotalMinutesAsleep/60)) +
  geom_histogram(binwidth = 0.5, fill = "blue") +
  labs(title = "Distribution of Sleep Duration",
       x = "Hours Asleep",
       y = "Count") +
  theme_minimal()

# 2. Create hourly_hr dataframe
hourly_hr <- heart_rate %>%
  mutate(Hour = hour(Time)) %>%  # Extract hour (0-23)
  group_by(Hour) %>%
  summarise(AvgHR = mean(Value, na.rm = TRUE)) # Calculate average BPM

# Relationship between time in bed and actual sleep
ggplot(sleep_day, aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Time in Bed vs Actual Sleep",
       x = "Total Time in Bed (minutes)",
       y = "Total Minutes Asleep") +
  theme_minimal()

##Activity vs Sleep Analysis
# Merge activity and sleep data
activity_sleep <- daily_activity %>%
  inner_join(sleep_day, by = c("Id", "ActivityDate" = "SleepDate"))

# Steps vs Sleep duration
ggplot(activity_sleep, aes(x = TotalSteps, y = TotalMinutesAsleep/60)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Daily Steps vs Sleep Duration",
       x = "Total Steps",
       y = "Hours Asleep") +
  theme_minimal()

##User Segmentation
# Create user segments based on activity level
user_segments <- daily_activity %>%
  group_by(Id) %>%
  summarise(AvgSteps = mean(TotalSteps),
            AvgActiveMinutes = mean(FairlyActiveMinutes + VeryActiveMinutes)) %>%
  mutate(ActivityLevel = case_when(
    AvgSteps < 5000 ~ "Sedentary",
    AvgSteps >= 5000 & AvgSteps < 7500 ~ "Lightly Active",
    AvgSteps >= 7500 & AvgSteps < 10000 ~ "Moderately Active",
    AvgSteps >= 10000 ~ "Very Active"
  ))

# Plot distribution of user segments
ggplot(user_segments, aes(x = ActivityLevel, fill = ActivityLevel)) +
  geom_bar() +
  labs(title = "Distribution of User Activity Levels",
       x = "Activity Level",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

##Key Insights and Recommendations for Bellabeat
# Calculate key metrics
avg_steps <- mean(daily_activity$TotalSteps, na.rm = TRUE)
avg_sleep <- mean(sleep_day$TotalMinutesAsleep, na.rm = TRUE)/60
sleep_efficiency <- mean(sleep_day$TotalMinutesAsleep/sleep_day$TotalTimeInBed, na.rm = TRUE)
active_users <- mean(user_segments$AvgSteps >= 7500)

cat("Key Insights:\n")
cat("- Average daily steps:", round(avg_steps), "\n")
cat("- Average sleep duration:", round(avg_sleep, 1), "hours\n")
cat("- Average sleep efficiency:", round(sleep_efficiency*100, 1), "%\n")
cat("- Percentage of moderately/very active users:", round(active_users*100, 1), "%\n")

cat("\nRecommendations for Bellabeat:\n")
cat("1. Focus on sleep tracking features as sleep efficiency varies significantly\n")
cat("2. Develop programs to help sedentary users increase activity gradually\n")
cat("3. Create reminders for users who show patterns of high activity but poor sleep\n")
cat("4. Implement time-of-day specific activity suggestions based on peak hours\n")
cat("5. Consider gamification to help users reach 7,500+ steps (moderate activity level)\n")

##Next Steps You Could Take:
hourly_hr <- heart_rate %>%
  mutate(Hour = hour(Time)) %>%      # Extract hour (0-23) from timestamp
  group_by(Hour) %>%                 # Group by hour of day
  summarise(AvgHR = mean(Value))     # Calculate average BPM per hour

ggplot(hourly_hr, aes(x = Hour, y = AvgHR)) + 
  geom_line(color = "red")
##final conculation
ggplot(hourly_hr, aes(x = Hour, y = AvgHR)) + 
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Average Heart Rate by Hour of Day",
       x = "Hour of Day (0-23)", 
       y = "Average BPM") +
  scale_x_continuous(breaks = seq(0, 23, 2)) + # Show even hours only
  theme_minimal()

ggplot(Activity_data, aes(x = TotalSteps, fill = Activity_Level)) +
  geom_density(alpha = 0.6) +
  labs(title = "Step Distribution by Activity Level")


# Add activity level classification (replace 'activity_data' with your dataframe name)
daily_activity <- daily_activity %>%  
  mutate(
    Activity_Level = case_when(
      TotalSteps >= 10000 ~ "Highly Active",
      TotalSteps >= 7500  ~ "Moderately Active", 
      TotalSteps >= 5000  ~ "Lightly Active",
      TRUE               ~ "Non-Active"
    ),
    # Convert to ordered factor for better plotting
    Activity_Level = factor(Activity_Level, 
                            levels = c("Non-Active", "Lightly Active", 
                                       "Moderately Active", "Highly Active"))
  )


ggplot(daily_activity, aes(x = TotalSteps, fill = Activity_Level)) +
  geom_density(alpha = 0.6, color = NA) +  # Remove density line borders
  labs(
    title = "Step Distribution by Activity Level",
    subtitle = "Non-active users dominate the <5,000 steps range",
    x = "Daily Steps",
    y = "Density",
    fill = "Activity Level"
  ) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) 
theme_minimal() +
  theme(legend.position = "bottom")

active_threshold <- 7500
user_activity_levels <- daily_activity %>%
  group_by(Id) %>%
  summarise(avg_steps = mean(TotalSteps)) %>%
  mutate(activity_status = ifelse(avg_steps >= active_threshold, "Active", "Non-Active"))

daily_activity_compare <- daily_activity %>%
  left_join(user_activity_levels, by = "Id")

user_activity_levels <- daily_activity %>%
  group_by(Id) %>%
  summarise(avg_steps = mean(TotalSteps)) %>%
  mutate(activity_status = ifelse(avg_steps >= active_threshold, "Active", "Non-Active"))

##daily activity (active and non active)
daily_activity_compare <- daily_activity %>%
  left_join(user_activity_levels, by = "Id")


# Steps comparison
ggplot(daily_activity_compare, aes(x = TotalSteps, fill = activity_status)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = active_threshold, linetype = "dashed") +
  labs(title = "Daily Steps: Active vs. Non-Active Users")

# sleepday (active and non active)
sleep_compare <- sleep_day %>%
  left_join(user_activity_levels, by = "Id")

# Sleep duration comparison
ggplot(sleep_compare, aes(x = TotalMinutesAsleep/60, fill = activity_status)) +
  geom_boxplot() +
  labs(title = "Sleep Duration by Activity Level", x = "Hours Asleep")

#hourly Steps(active and nonactive)
hourly_steps_compare <- hourly_steps %>%
  left_join(user_activity_levels, by = "Id")

#Activity patterns by hour
hourly_steps_compare %>%
  mutate(Hour = hour(ActivityHour)) %>%
  group_by(Hour, activity_status) %>%
  summarise(avg_steps = mean(StepTotal)) %>%
  ggplot(aes(x = Hour, y = avg_steps, color = activity_status)) +
  geom_line(size = 1.2) +
  labs(title = "Hourly Step Patterns Comparison")


#Heart rate
hr_compare <- heart_rate %>%
  left_join(user_activity_levels, by = "Id")

# Resting HR comparison
hr_compare %>%
  mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  filter(hour(Time) %in% 3:5) %>%  # Early morning = resting HR
  group_by(Id, activity_status) %>%
  summarise(resting_hr = mean(Value)) %>%
  ggplot(aes(x = activity_status, y = resting_hr, fill = activity_status)) +
  geom_violin() +
  labs(title = "Resting Heart Rate Comparison")


#calories
calories_compare <- daily_activity %>%
  left_join(user_activity_levels, by = "Id")

ggplot(calories_compare, aes(x = Calories, fill = activity_status)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Caloric Burn Distribution")

#combined summary
comparison_summary <- list(
  daily_activity_compare %>% group_by(activity_status) %>% 
    summarise(avg_steps = mean(TotalSteps), avg_calories = mean(Calories)),
  
  sleep_compare %>% group_by(activity_status) %>% 
    summarise(avg_sleep = mean(TotalMinutesAsleep)/60),
  
  hr_compare %>% 
    mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p")) %>%
    filter(hour(Time) %in% 3:5) %>%
    group_by(activity_status) %>%
    summarise(avg_resting_hr = mean(Value))
) %>% reduce(left_join, by = "activity_status")

knitr::kable(comparison_summary, digits = 1)

write_csv(daily_activity_compare, "active_vs_inactive_daily_activity.csv")
write_csv(sleep_compare, "active_vs_inactive_sleep.csv")
wrire_csv(calories_compare,"calories_by_activity_status.csv")
write_csv(hr_compare,"heartrate_by_activity_status.csv")
write_csv(hourly_steps_compare,"hourly_steps_by_activity_status.csv")