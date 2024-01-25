library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(psych)

daily_activity_merged <- read.csv("C:\\Users\\Wm. Lenord Stage\\Downloads\\coursera\\case_study_2\\Fitabase Data 4.12.16-5.12.16\\dailyActivity_merged.csv")
hourly_calories_merged <-read.csv("C:\\Users\\Wm. Lenord Stage\\Downloads\\coursera\\case_study_2\\Fitabase Data 4.12.16-5.12.16\\hourlyCalories_merged.csv")
hourly_intensities_merged <- read.csv("C:\\Users\\Wm. Lenord Stage\\Downloads\\coursera\\case_study_2\\Fitabase Data 4.12.16-5.12.16\\hourlyIntensities_merged.csv")
sleep_day_merged <- read.csv("C:\\Users\\Wm. Lenord Stage\\Downloads\\coursera\\case_study_2\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv")
weight_merged <- read.csv("C:\\Users\\Wm. Lenord Stage\\Downloads\\coursera\\case_study_2\\Fitabase Data 4.12.16-5.12.16\\weightLogInfo_merged.csv")
minuteSleep_merged <- read.csv("C:\\Users\\Wm. Lenord Stage\\Downloads\\coursera\\case_study_2\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv")

n_distinct(daily_activity_merged$Id)
n_distinct(hourly_calories_merged$Id)
n_distinct(hourly_intensities_merged$Id)
n_distinct(sleep_day_merged$Id) # this has 24 participants in it. 
n_distinct(weight_merged$Id)
options(scipen=999)#dumps scientific notation
plot.new

hist(weight_merged$BMI, main="BMI of 8 Customers",
     xlab='BMI', ylab='Frequency', breaks = 10, col='orange')

hist(weight_merged$BMI, main="BMI of 8 Customers",
     xlab='BMI', ylab='Frequency', breaks = 10, col='orange', 
     legend('top',legend = 'Fit people do use the app'))

daily_activity_merged_b <- daily_activity_merged %>% 
  mutate(across(
    .cols = matches('TotalDistance'),
    .fns = ~ round(as.integer(.2))))# changes total distance from a dbl to an 
                                    # int with 2 decimals

group_by_damb <- daily_activity_merged_b %>% 
  group_by(Id) %>% 
  summarise_at(vars(TrackerDistance),list(mean=mean))

ggplot(data=group_by_damb, aes(x = Id, y = mean))+
  geom_point() + labs(title='Mean of Distance Tracked by Id Number', 
                      caption = 'Min:.634, Mean:5.38, Max:13.21')+geom_smooth()+ theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white",color = "black"))

group_by_damb %>% #A summary that confirms that 5.3 km is the mean distance 
  select(mean) %>% 
  summary()

group_by_damc <- daily_activity_merged_b %>% 
  group_by(Id) %>% 
  summarise_at(vars(TotalSteps),list(mean=mean))

ggplot(data=group_by_damc, aes(x = Id, y = mean))+
  geom_point() + labs(title='Mean of Steps by Id Number', 
                      caption = 'Min:916, Mean:7,519, Max:16,040')+geom_smooth()
+ theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white",color = "black"))

group_by_damc %>% #A summary that confirms that 7519 is the mean mean number of steps 
  select(mean) %>% 
  summary()

group_by_dasm <- daily_activity_merged %>% 
  group_by(Id) %>% 
  summarise_at(vars(SedentaryMinutes),list(mean=mean))
group_by_dasm$hours <- (group_by_dasm$mean/60)

ggplot(data=group_by_dasm, aes(x = Id, y = hours))+
  geom_point() + labs(title='Mean of Sedentary Hours by Id Number', 
                      caption = 'Min:11.04, Mean:16.65, Max:21.96')+geom_smooth()+ theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white",color = "black"))

group_by_dasm %>% #A summary that confirms that 999 is the mean number of sedentary minutes
  select(hours) %>% 
  summary()

group_by_dasm$hours <- (group_by_dasm$mean/60)

group_by_msm <- minuteSleep_merged %>% 
  group_by(Id) %>% 
  summarise_at(vars(TotalMinutesAsleep),list(mean=mean))
group_by_msm$hours <- (group_by_msm$mean/60)

ggplot(data=group_by_msm, aes(x = Id, y = hours))+
  geom_point() + labs(title='Mean of Sleep Hours by Id Number', 
                      caption = 'Min:1.017, Mean:6.29, Max:10.86')+geom_smooth()+ theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white",color = "black"))

group_by_msm %>% #A summary that confirms that 6.29 is the mean number of sedentary hours
  select(hours) %>% 
  summary()

daily_activity_merged$VeryActiveMinutes <- as.numeric(as.character(daily_activity_merged$VeryActiveMinutes))
daily_activity_merged$FairlyActiveMinutes <- as.numeric(as.character(daily_activity_merged$FairlyActiveMinutes))

daily_activity_merged$Very_Fairly_combined <- daily_activity_merged$VeryActiveMinutes + daily_activity_merged$FairlyActiveMinutes
daily_activity_merged$Very_Fairly_combined_hours <- daily_activity_merged$Very_Fairly_combined/60

daily_activity_merged_d <- daily_activity_merged %>% 
  group_by(Id) %>% #groups and make a new df of the very and fairly active hours mean
  summarise_at(vars(Very_Fairly_combined_hours),list(mean_VF_Hours=mean))

ggplot(data=daily_activity_merged_d, aes(x = Id, y = mean_VF_Hours))+#this make the vizz
  geom_point() + labs(title='Mean of Very and Fairly Active Hours', 
                      caption = 'Min:.005, Mean:.55, Max:1.95')+geom_smooth()+ theme(
                        plot.margin = margin(1, 1, 1, 1, "cm"),
                        panel.background = element_rect(fill = "white"),
                        plot.background = element_rect(fill = "white",color = "black"))
daily_activity_merged_d %>% 
  select(mean_VF_Hours) %>% 
  summary()#this give the stats to write into the viz

