---
title: "Case_Study_Bellabeat_Project"
author: "Gwaro_Moses"
date: "2023-10-17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Case study: Bellabeat ##Scenario Bellabeat a high tech manufacturer of health focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. You have been asked to focus on one of Bellabeat's products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company. Products offered by Bellabeat ○ Bellabeat app: The Bellabeat app provides users with health data related to their activity, sleep, stress, menstrual cycle, and mindfulness habits. This data can help users better understand their current habits and make healthy decisions. The Bellabeat app connects to their line of smart wellness products. ○ Leaf: Bellabeat's classic wellness tracker can be worn as a bracelet, necklace, or clip. The Leaf tracker connects to the Bellabeat app to track activity, sleep, and stress. ○ Time: This wellness watch combines the timeless look of a classic timepiece with smart technology to track user activity, sleep, and stress. The Time watch connects to the Bellabeat app to provide you with insights into your daily wellness. ○ Spring: This is a water bottle that tracks daily water intake using smart technology to ensure that you are appropriately hydrated throughout the day. The Spring bottle connects to the Bellabeat app to track your hydration levels. ○ Bellabeat membership: Bellabeat also offers a subscription-based membership program for users. Membership gives users 24/7 access to fully personalized guidance on nutrition, activity, sleep, health and beauty, and mindfulness based on their lifestyle and goals.

##About the company Urška Sršen and Sando Mur founded Bellabeat, a high-tech company that manufactures health-focused smart products. Sršen used her background as an artist to develop beautifully designed technology that informs and inspires women around the world. Collecting data on activity, sleep, stress, and reproductive health has allowed Bellabeat to empower women with knowledge about their own health and habits. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women.

By 2016, Bellabeat had opened offices around the world and launched multiple products. Bellabeat products became available through a growing number of online retailers in addition to their own e-commerce channel on their website. The company has invested in traditional advertising media, such as radio, out-of-home billboards, print, and television, but focuses on digital marketing extensively. Bellabeat invests year-round in Google Search, maintaining active Facebook and Instagram pages, and consistently engages consumers on Twitter. Additionally, Bellabeat runs video ads on Youtube and display ads on the Google Display Network to support campaigns around key marketing dates.

Sršen knows that an analysis of Bellabeat's available consumer data would reveal more opportunities for growth. She has asked the marketing analytics team to focus on a Bellabeat product and analyze smart device usage data in order to gain insight into how people are already using their smart devices. Then, using this information, she would like high-level recommendations for how these trends can inform Bellabeat marketing strategy.

The case study will follow the 6 steps data analysis process which are; • Ask • Prepare • Process • Analyze • Share • Act

##ASK

###Main objective Analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart devices. Select one Bellabeat product to apply these insights to in the presentation.

###Other objectives 1. What are some trends in smart device usage? 2. How could these trends apply to Bellabeat customers? 3. How could these trends help influence Bellabeat marketing strategy?

###Stakeholders • Primary stakeholder - Urška Sršen: Bellabeat's cofounder and Chief Creative Officer. • Secondary stakeholders o Sando Mur: Mathematician and Bellabeat's cofounder; key member of the Bellabeat executive team. o Bellabeat marketing analytics team: A team of data analysts responsible for collecting, analyzing, and reporting data that helps guide Bellabeat's marketing strategy.

###Deliverables 1. A clear summary of the business task. 2. A description of all data sources used. 3. Documentation of any cleaning or manipulation of data. 4. A summary of your analysis. 5. Supporting visualizations and key findings. 6. Your top high-level content recommendations based on your analysis.

##PREPARE

###Dataset used <FitBit Fitness Tracker Data> (CC0: Public Domain, dataset made available through Mobius): This Kaggle data set contains personal fitness tracker from thirty FitBit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users' habits. The dataset contains 18 CSV files and confirms to the ROCCC approach. • Reliability. Data is from 30 Fitbit users who agreed to submit their personal tracker data. The data was generated by a survey distributed via Amazon Mechanical Turk. • Original. The source is from the original 30 Fitbit users who consented to share their data. • Comprehensive. Data contains daily, hourly and minute level outputs on physical activity, heart rate and sleep monitoring. • Current. The data is not current and is from March 2016 to May 2016. • Cited. The data is not cited.

###Limitations of datasets. *The dataset is from only 30 users, however I would have preferred a bigger sample for analysis.* Data for sleep activity is missing for 6 users while the data for weight is missing data for 22 users. \*The data is from a third party hence I cannot acertain its intergrity or accuracy.

#PROCESS ##Cleaning datasets. Upload packages to create a good working environment

```{r install and load packages}
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("plotly")
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(here)
library(skimr)
library(janitor)
library(plotly)
```

upload the 3 main datasets

```{r import data}
dailyActivity <- read_csv("Raw data/dailyActivity.csv")
sleepDay <- read_csv("Raw data/sleepDay.csv")
weightLogInfo <- read_csv("Raw data/weightLogInfo.csv")
```

Check for missing data

```{r missing data}
sum(is.na(sleepDay))
sum(is.na(dailyActivity))
sum(is.na(weightLogInfo))
```

Assumption: The missing data in weightloginfo dataset is because most respondents didn't know their fat content.

Checking for duplicates and droping the duplicates

```{r duplicate data}
sum(duplicated(sleepDay))
sum(duplicated(dailyActivity))
sum(duplicated(weightLogInfo))

sleepDay <- sleepDay[!duplicated(sleepDay), ]
sum(duplicated(sleepDay))
```

Add a column for the day of the week represented by the date.

```{r add day of the week}
dailyActivity <- dailyActivity %>% mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))
```

## Merge the 3 datasets and save into a new .csv file

Clean date columns in sleepDay and weightLogInfo dataset to be uniform with dailyActivity dataset

```{r split datetime column into date $ tame}
Sleep <- separate(sleepDay, col = SleepDay, into = c("Date", "Time"), sep=" ")
Weight <- separate(weightLogInfo, col = Date, into = c("Date", "Time"), sep=" ")
Activity <- dailyActivity %>% rename(Date = ActivityDate)
```

```{r merge datasets using Id}
merged1 <- merge(Activity,Sleep,by = c("Id", "Date"), all=TRUE)
merged_data <- merge(merged1, Weight, by = c("Id", "Date"), all=TRUE)
```

Check for duplicates in the merged dataset

```{r checking for duplicates}
sum(duplicated(merged_data))
```

Arrange days of the week in chronological order for ease of ploting.

```{r arrange weekday}
merged_data$Weekday <- factor(merged_data$Weekday, levels= c("Monday", 
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
merged_data[order(merged_data$Weekday), ]
```

Save the merged Dataset and all clean dataset in a new folder

```{r save merged data and all clean datasets}
write_csv(merged_data, "Clean Data/merged_data.csv")
write_csv(Activity, "Clean Data/Activity.csv")
write_csv(Sleep, "Clean Data/Sleep.csv")
write_csv(Weight, "Clean Data/Weight.csv")
```

#ANALYSE \## Perform calculations and identify trends and relationships in the data.

Days people are most active

```{r mostactive days}
ggplot(data = merged_data) +
  geom_bar(mapping = aes(x=Weekday)) +
  labs(title = "Days most Active")+
  theme(axis.text.x = element_text(angle = 45))
```

Weekly and hourly summary Average steps per day

```{r average steps daily}
ggplot(data=merged_data, aes(x=Weekday, y=TotalSteps, fill=Weekday))+ 
  geom_bar(stat="identity", fill="blue")+
  labs(title="Weekly average steps per day", y="Total Steps")+
  theme(axis.text.x = element_text(angle = 45))
```

Average calories burnt weekly

```{r}
ggplot(data=merged_data, aes(x=Weekday, y=Calories, fill=Weekday))+ 
  geom_bar(stat="identity", fill="blue")+
  theme(axis.text.x = element_text(angle = 45))
```

Average minutes spent sleeping per week

```{r sleep average per week}
ggplot(data=merged_data, aes(x=Weekday, y=TotalMinutesAsleep, fill=Weekday))+ 
  geom_bar(stat="identity", fill="blue")+
  labs(title="Total Minutes Asleep During the Week", y="Total Minutes Asleep")+
theme(axis.text.x = element_text(angle = 45))
```

Average minutes spent sedentary weekly

```{r sedentary minutes}
ggplot(data=merged_data, aes(x=Weekday, y=SedentaryMinutes, fill=Weekday))+ 
  geom_bar(stat="identity", fill="blue")+
  labs(title="Sedentary Minutes weekly", y="Sedentary Minutes")+
  theme(axis.text.x = element_text(angle = 45))
```

Average distance covered weekly

```{r distance coverd}
ggplot(data=merged_data, aes(x=Weekday, y=TotalDistance, fill=Weekday))+ 
  geom_bar(stat="identity", fill="blue")+
  labs(title="Average Distance Covered Weekly", y="Distance covered")+
 theme(axis.text.x = element_text(angle = 45))
```

Statistical summary on all 3 datasets

```{r summary of all 3 datasets}
Activity %>%
 dplyr::select(TotalSteps,
         TotalDistance,
         VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes,
         Calories) %>%
  summary()

Sleep %>%  
  dplyr::select(TotalSleepRecords,
  TotalMinutesAsleep,
  TotalTimeInBed) %>%
  summary()

Weight %>% 
  dplyr::select(WeightPounds, BMI) %>%
  summary()
```

Analysis on active users

```{r}
active_users <- Activity %>%
  filter(FairlyActiveMinutes >= 21.4 | VeryActiveMinutes>=10.7) %>% 
  group_by(Id) %>% 
  count(Id) 

total_minutes <- sum(Activity$SedentaryMinutes, Activity$VeryActiveMinutes, Activity$FairlyActiveMinutes, Activity$LightlyActiveMinutes)
sedentary_percentage <- sum(Activity$SedentaryMinutes)/total_minutes*100
lightly_percentage <- sum(Activity$LightlyActiveMinutes)/total_minutes*100
fairly_percentage <- sum(Activity$FairlyActiveMinutes)/total_minutes*100
active_percentage <- sum(Activity$VeryActiveMinutes)/total_minutes*100
```

Piechart on average level of activeness

```{r pie chart on activeness}
percentage <- data.frame(
  level=c("Sedentary", "Lightly", "Fairly", "Very Active"),
  minutes=c(sedentary_percentage,lightly_percentage,fairly_percentage,active_percentage)
)

plot_ly(percentage, labels = ~level, values = ~minutes, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(title = 'Activity Level Minutes',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

```
Relationships in the data

```{r activeness vs calories}
active_minutes_vs_calories <- ggplot(data = merged_data) + 
  geom_point(mapping=aes(x=Calories, y=FairlyActiveMinutes), color = "maroon", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x, mapping=aes(x=Calories, y=FairlyActiveMinutes, color=FairlyActiveMinutes), color = "maroon", se = FALSE) +
  
  geom_point(mapping=aes(x=Calories, y=VeryActiveMinutes), color = "forestgreen", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=Calories, y=VeryActiveMinutes, color=VeryActiveMinutes), color = "forestgreen", se = FALSE) +
  
  geom_point(mapping=aes(x=Calories, y=LightlyActiveMinutes), color = "orange", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=Calories, y=LightlyActiveMinutes, color=LightlyActiveMinutes), color = "orange", se = FALSE) +
  
  geom_point(mapping=aes(x=Calories, y=SedentaryMinutes), color = "blue", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=Calories, y=SedentaryMinutes, color=SedentaryeMinutes), color = "blue", se = FALSE) +
  
  annotate("text", x=4800, y=160, label="Very Active", color="black", size=3)+
  annotate("text", x=4800, y=0, label="Fairly Active", color="black", size=3)+
  annotate("text", x=4800, y=500, label="Sedentary", color="black", size=3)+
  annotate("text", x=4800, y=250, label="Lightly  Active", color="black", size=3)+
  labs(x = "Calories", y = "Active Minutes", title="Calories vs Active Minutes")
```

###Annalysis on sleep

Sleep time in hours instead of minutes

```{r convert sleep to hours}
sleep_day_in_hour <-Sleep
sleep_day_in_hour$TotalMinutesAsleep <- sleep_day_in_hour$TotalMinutesAsleep/60
sleep_day_in_hour$TotalTimeInBed <- sleep_day_in_hour$TotalTimeInBed/60
head(sleep_day_in_hour)
```

Check for sleep outliers

```{r sleep outliers}
sum(sleep_day_in_hour$TotalMinutesAsleep > 9)
sum(sleep_day_in_hour$TotalTimeInBed > 9)
sum(sleep_day_in_hour$TotalMinutesAsleep < 2)
sum(sleep_day_in_hour$TotalTimeInBed < 2)
```

How many users spend more than 55 minutes awake in bed

```{r more than 55' awake}
awake_in_bed <- mutate(Sleep, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)
awake_in_bed <- awake_in_bed %>% 
  filter(AwakeTime >= 55) %>% 
  group_by(Id) %>% 
  arrange(AwakeTime, desc=TRUE) 
  
n_distinct(awake_in_bed$Id)
```
13 people spend more than 55 minutes awake in bed

Relationship between minutes asleep and calories

```{r calories vs asleep}
ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y = Calories, color=TotalMinutesAsleep))+ 
  geom_point()+ 
  labs(title="Total Minutes Asleep vs Calories")+
  xlab("Total Minutes Alseep")+
  stat_smooth(method=lm)+
  scale_color_gradient(low="red", high="blue")
```

Relationship between sleep hours and sedentary hours

```{r sleep vs sedentary}
ggplot(data=merged_data, aes(x=TotalMinutesAsleep/60 ,y=SedentaryMinutes/60, color=TotalSteps))+ 
  geom_point()+
  scale_color_gradient(low="steelblue", high="orange") +
  ylab("sedentary hours")+
  xlab("total sleep hours")
```

