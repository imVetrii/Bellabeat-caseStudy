---
---
title: "bellaBeatCaseStudy"
author: "Vetri"
date: "2025-04-24"
output: github_document
---
```{r}

```


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
### 📌 Introduction

This project analyzes FitBit fitness tracker data for Bellabeat using R. It explores trends in daily activity, sleep patterns, heart rate, and more to generate product recommendations.

```{r cars}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(skimr)
```
# Load datasets (adjust file paths as needed)

```{r}
daily_activity <- read_csv("dailyActivity_merged.csv")
sleep_day <- read_csv("sleepDay_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
weight_logInfo <- read_csv("weightLogInfo_merged.csv")
heart_rate <- read_csv("heartrate_seconds_merged.csv")
```
### Data Cleaning and Preparation..
## Convert dates to proper format
```{r}
daily_activity <- daily_activity %>%
  mutate(ActivityDate = mdy(ActivityDate))

sleep_day <- sleep_day %>%
  mutate(SleepDay = mdy_hms(SleepDay)) %>%
  mutate(SleepDate = as.Date(SleepDay))

hourly_steps <- hourly_steps %>%
  mutate(ActivityHour = mdy_hms(ActivityHour))

heart_rate <- heart_rate %>%
  mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p")) # Convert timestamp

```
# Check for missing values
```{r}
skim(daily_activity)
skim(sleep_day)
```
## Number of unique users in each dataset`
```{r}
cat("Daily activity users:", n_distinct(daily_activity$Id), "\n")
cat("Sleep data users:", n_distinct(sleep_day$Id), "\n")
cat("Hourly steps users:", n_distinct(hourly_steps$Id), "\n")

```
## Average days tracked per user

```{r}
daily_activity %>%
  group_by(Id) %>%
  summarise(DaysTracked = n()) %>%
  summarise(AvgDaysTracked = mean(DaysTracked))
```
# Daily step distribution
```{r}
ggplot(daily_activity, aes(x = TotalSteps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue") +
  labs(title = "Distribution of Daily Steps",
       x = "Total Steps",
       y = "Count") +
  theme_minimal()
```
# Activity intensity average steps throughout the day
```{r}
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

```
##Sleep Patterns Analysis
# Sleep duration distribution
```{r}
ggplot(sleep_day, aes(x = TotalMinutesAsleep/60)) +
  geom_histogram(binwidth = 0.5, fill = "blue") +
  labs(title = "Distribution of Sleep Duration",
       x = "Hours Asleep",
       y = "Count") +
  theme_minimal()

```

# Create hourly_hr dataframe
```{r}
hourly_hr <- heart_rate %>%
  mutate(Hour = hour(Time)) %>%  # Extract hour (0-23)
  group_by(Hour) %>%
  summarise(AvgHR = mean(Value, na.rm = TRUE)) # Calculate average BPM

```
# Relationship between time in bed and actual sleep
```{r}
ggplot(sleep_day, aes(x = TotalTimeInBed, y = TotalMinutesAsleep)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Time in Bed vs Actual Sleep",
       x = "Total Time in Bed (minutes)",
       y = "Total Minutes Asleep") +
  theme_minimal()

```
##Activity vs Sleep Analysis
# Merge activity and sleep data

```{r}
activity_sleep <- daily_activity %>%
  inner_join(sleep_day, by = c("Id", "ActivityDate" = "SleepDate"))
```
# Steps vs Sleep duration
```{r}
ggplot(activity_sleep, aes(x = TotalSteps, y = TotalMinutesAsleep/60)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Daily Steps vs Sleep Duration",
       x = "Total Steps",
       y = "Hours Asleep") +
  theme_minimal()
```
##User Segmentation
# Create user segments based on activity level

```{r}
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
```
# Plot distribution of user segments

```{r}
ggplot(user_segments, aes(x = ActivityLevel, fill = ActivityLevel)) +
  geom_bar() +
  labs(title = "Distribution of User Activity Levels",
       x = "Activity Level",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
```
##Key Insights and Recommendations for Bellabeat
# Calculate key metrics
```{r}
avg_steps <- mean(daily_activity$TotalSteps, na.rm = TRUE)
avg_sleep <- mean(sleep_day$TotalMinutesAsleep, na.rm = TRUE)/60
sleep_efficiency <- mean(sleep_day$TotalMinutesAsleep/sleep_day$TotalTimeInBed, na.rm = TRUE)
active_users <- mean(user_segments$AvgSteps >= 7500)

```
```{r}
cat("Key Insights:\n")
cat("- Average daily steps:", round(avg_steps), "\n")
cat("- Average sleep duration:", round(avg_sleep, 1), "hours\n")
cat("- Average sleep efficiency:", round(sleep_efficiency*100, 1), "%\n")
cat("- Percentage of moderately/very active users:", round(active_users*100, 1), "%\n")

```
```{r}
cat("\nRecommendations for Bellabeat:\n")
cat("1. Focus on sleep tracking features as sleep efficiency varies significantly\n")
cat("2. Develop programs to help sedentary users increase activity gradually\n")
cat("3. Create reminders for users who show patterns of high activity but poor sleep\n")
cat("4. Implement time-of-day specific activity suggestions based on peak hours\n")
cat("5. Consider gamification to help users reach 7,500+ steps (moderate activity level)\n")

```
##Next Steps You Could Take:
```{r}
hourly_hr <- heart_rate %>%
  mutate(Hour = hour(Time)) %>%      # Extract hour (0-23) from timestamp
  group_by(Hour) %>%                 # Group by hour of day
  summarise(AvgHR = mean(Value))     # Calculate average BPM per hour

```

```{r}
ggplot(hourly_hr, aes(x = Hour, y = AvgHR)) + 
  geom_line(color = "red")
```
```{r}
ggplot(hourly_hr, aes(x = Hour, y = AvgHR)) + 
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Average Heart Rate by Hour of Day",
       x = "Hour of Day (0-23)", 
       y = "Average BPM") +
  scale_x_continuous(breaks = seq(0, 23, 2)) + # Show even hours only
  theme_minimal()
```
# Add activity level classification (replace 'activity_data' with your dataframe name)
```{r}
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

```
```{r}
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
```
```{r}

active_threshold <- 7500
user_activity_levels <- daily_activity %>%
  group_by(Id) %>%
  summarise(avg_steps = mean(TotalSteps)) %>%
  mutate(activity_status = ifelse(avg_steps >= active_threshold, "Active", "Non-Active"))
```
```{r}

daily_activity_compare <- daily_activity %>%
  left_join(user_activity_levels, by = "Id")

```
```{r}
user_activity_levels <- daily_activity %>%
  group_by(Id) %>%
  summarise(avg_steps = mean(TotalSteps)) %>%
  mutate(activity_status = ifelse(avg_steps >= active_threshold, "Active", "Non-Active"))

```
## ##daily activity (active and non active)
```{r}
daily_activity_compare <- daily_activity %>%
  left_join(user_activity_levels, by = "Id")

```
## # Steps comparison
```{r}
ggplot(daily_activity_compare, aes(x = TotalSteps, fill = activity_status)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = active_threshold, linetype = "dashed") +
  labs(title = "Daily Steps: Active vs. Non-Active Users")
```
## sleepday (active and non active)

```{r}
sleep_compare <- sleep_day %>%
  left_join(user_activity_levels, by = "Id")
```

# Sleep duration comparison
```{r}
ggplot(sleep_compare, aes(x = TotalMinutesAsleep/60, fill = activity_status)) +
  geom_boxplot() +
  labs(title = "Sleep Duration by Activity Level", x = "Hours Asleep")

```
#hourly Steps(active and nonactive)
```{r}
hourly_steps_compare <- hourly_steps %>%
  left_join(user_activity_levels, by = "Id")

```

#Activity patterns by hour
```{r}
hourly_steps_compare %>%
  mutate(Hour = hour(ActivityHour)) %>%
  group_by(Hour, activity_status) %>%
  summarise(avg_steps = mean(StepTotal)) %>%
  ggplot(aes(x = Hour, y = avg_steps, color = activity_status)) +
  geom_line(size = 1.2) +
  labs(title = "Hourly Step Patterns Comparison")

```
#Heart rate
```{r}
hr_compare <- heart_rate %>%
  left_join(user_activity_levels, by = "Id")

```
# Resting HR comparison
```{r}
hr_compare %>%
  mutate(Time = as.POSIXct(Time, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  filter(hour(Time) %in% 3:5) %>%  # Early morning = resting HR
  group_by(Id, activity_status) %>%
  summarise(resting_hr = mean(Value)) %>%
  ggplot(aes(x = activity_status, y = resting_hr, fill = activity_status)) +
  geom_violin() +
  labs(title = "Resting Heart Rate Comparison")

```
#calories
```{r}
calories_compare <- daily_activity %>%
  left_join(user_activity_levels, by = "Id")
```
```{r}
ggplot(calories_compare, aes(x = Calories, fill = activity_status)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  labs(title = "Caloric Burn Distribution")
```
#combined summary
```{r}
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
```
