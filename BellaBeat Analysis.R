install.packages("tidyverse")
library(tidyverse)

dailyActivity_merged <- read.csv("dailyActivity_merged.csv")

head(dailyActivity_merged)
View(dailyActivity_merged)
str(dailyActivity_merged)

## Clean The Data

# Remove rows that don't make sense/show the fitness device was not worn enough
# to gather good data. This includes removing any rows where sedentary minutes
# were 1440 as this is 24 hours or the entire day. 

clean_dailyActivity_merged <- 
  subset(dailyActivity_merged, SedentaryMinutes != 1440)

str(clean_dailyActivity_merged)

# After seeing the structure of our new data, we see that the number of rows
# decreased from 940 to 861. This will make our visualizations more accurate
# by excluding those rows with no activity as these days are usually when the
# device is not worn.

# We also see that one of the columns data types is not accurate. The 
# ActivityDate column is char and not date. Next we will include code to change
# this so that we can use the date column in visualizations.

clean_dailyActivity_merged$ActivityDate <- as.Date(clean_dailyActivity_merged$ActivityDate, "%m/%d/%Y")
str(clean_dailyActivity_merged)

#Now the ActivityDate column is in date format

View(clean_dailyActivity_merged)

#I want to see what each rows total minutes worn is to use this data for further analysis
#I will do this by creating a new column in our cleaned data set and adding all the 
#categories with minutes of exertion 

clean_dailyActivity_merged$TotalMinutesWorn <- clean_dailyActivity_merged$VeryActiveMinutes +
  clean_dailyActivity_merged$FairlyActiveMinutes + clean_dailyActivity_merged$LightlyActiveMinutes +
  clean_dailyActivity_merged$SedentaryMinutes

View(clean_dailyActivity_merged)

# After viewing the TotalMinutesWorn data, it shows that there are several rows that
# have 1440 minutes worn, which is all day. I want to create another column that shows
# whether or not a person wore their device all day long

clean_dailyActivity_merged$WornAllDay <- clean_dailyActivity_merged$TotalMinutesWorn == 1440

View(clean_dailyActivity_merged)

#After viewing the WornAllDay column we see that the values do return TRUE if worn 1440 and FALSE if not

ggplot(data = clean_dailyActivity_merged) + 
  geom_point(mapping=aes(x=TotalMinutesWorn, y=Calories)) +
  geom_smooth(mapping=aes(x=TotalMinutesWorn, y=Calories)) +
  ggtitle("Calories burned by Total Minutes Worn") 

cor(clean_dailyActivity_merged$TotalMinutesWorn, clean_dailyActivity_merged$Calories)

# This shows us the positive correlation between Total Minutes Worn and Calories burned.
# The correlation Coefficient is 0.1623407 which is not strong but over 0 which means there is 
# a slight correlation

# After being curious as to which days of the week contain the most active days I searched
# for a function that could turn the dates from my ActivityDate column into a new column creating weekdays

clean_dailyActivity_merged$Weekday <- weekdays(clean_dailyActivity_merged$ActivityDate)

View(clean_dailyActivity_merged)

## This shows a new column made with each rows weekday so we can now analyze by weekday

ggplot(data=clean_dailyActivity_merged) +
       geom_bar(mapping=aes(x= factor(clean_dailyActivity_merged$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),fill=Weekday))+
        labs(title = "Number of Times Device Worn Each Weekday") +
        ylab("Count") +
        xlab("Day of the Week")
  
#This bar chart shows the number of times each day of the week has recorded data
#It shows that the most active days are Tuesday, Wednesday, and Thursday. The middle of the week.


# Now we want to view the number of times the device was worn all day versus not
# This will show us which group will have a better


ggplot(data = clean_dailyActivity_merged) +
  geom_bar(mapping = aes(x=WornAllDay, fill=WornAllDay)) +
  ggtitle("Number of Times Device Worn/Not Worn All Day") +
  xlab("Was Device Worn All Day") +
  ylab("Count")

  
