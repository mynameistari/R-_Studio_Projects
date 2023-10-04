#Installed and loaded the necessary packages needed such as tidyverse
install.packages("tidyverse")

library(tidyverse)

#Imported the datasets needed for analysis and view
daily_activity<- read.csv("C:/Users/Captain Joe/Documents/Datasets/archive (3)/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

View(daily_activity)

sleep_df<- read.csv("C:/Users/Captain Joe/Documents/Datasets/archive (3)/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

weight_df<- read.csv("C:/Users/Captain Joe/Documents/Datasets/archive (3)/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

View(sleep_df)

View(weight_df)

#Calculated for the average of values in two columns
mean(daily_activity$TotalSteps)

mean(daily_activity$TotalDistance)

#Dropped null values
na.omit(daily_activity)

#Calculated the total sum of values in a column with unique IDs attached 
#and stored in a dataframe
sum_of_calories_per_user_stored<- aggregate(daily_activity$Calories~daily_activity$Id, data=daily_activity, FUN= sum)

View(sum_of_calories_per_user_stored)

#Determined the highest value in the new dataframe 
#and calculated the average value
max(sum_of_calories_per_user_stored)

mean(sum_of_calories_per_user_stored$`daily_activity$Calories`)

#Calculated the total sum of values in a column with unique IDs attached 
#and stored in a dataframe
sum_of_total_steps_per_user_stored<- aggregate(daily_activity$TotalSteps~daily_activity$Id, data=daily_activity, FUN= sum)

View(sum_of_total_steps_per_user_stored)

#Changed the column names of two data frames
colnames(sum_of_total_steps_per_user_stored)[colnames(sum_of_total_steps_per_user_stored)=="daily_activity$Id"]<- "User_ID"

colnames(sum_of_total_steps_per_user_stored)[colnames(sum_of_total_steps_per_user_stored)=="daily_activity$Total_Steps"]<- "Sum_of_Total_Steps"

colnames(sum_of_calories_per_user_stored)[colnames(sum_of_calories_per_user_stored)=="daily_activity$Id"]<- "User_ID"

colnames(sum_of_calories_per_user_stored)[colnames(sum_of_calories_per_user_stored)=="daily_activity$Calories"]<- "Total_Calories_per_User"

#Merged the two data frames into one
merged_df<- merge(sum_of_calories_per_user_stored, sum_of_total_steps_per_user_stored, by = "User_ID", all.x= TRUE)

View(merged_df)

#Changed the column names of the new merged data frame
colnames(merged_df)[colnames(merged_df)=="daily_activity$TotalSteps"]<- "Sum_of_Total_Steps"

#Calculated the average values across a column with respective IDs
#and stored them in a new data frame
average_calories_per_user<- aggregate(daily_activity$Calories~daily_activity$Id, data=daily_activity, FUN= mean)

average_total_steps_per_user<- aggregate(daily_activity$TotalSteps~daily_activity$Id, data=daily_activity, FUN= mean)

View(average_calories_per_user)

View(average_total_steps_per_user)

#Changed the column names of the two new data frames
colnames(average_calories_per_user)[colnames(average_calories_per_user)=="daily_activity$Id"]<- "User_ID"

colnames(average_calories_per_user)[colnames(average_calories_per_user)=="daily_activity$Calories"]<- "Average_Calories_per_User"

colnames(average_total_steps_per_user)[colnames(average_total_steps_per_user)=="daily_activity$Id"]<- "User_ID"

colnames(average_total_steps_per_user)[colnames(average_total_steps_per_user)=="daily_activity$TotalSteps"]<- "Average_Steps_Taken_per_User"

#Merged the two new data frames into one
merged_df_2<- merge(average_calories_per_user, average_total_steps_per_user, by = "User_ID", all.x= TRUE)

View(merged_df_2)

#Calculated the average values across a column with respective IDs
#and stored them in a new data frame
average_active_minutes_per_user<- aggregate(daily_activity$VeryActiveMinutes~daily_activity$Id, data=daily_activity, FUN= mean)

View(average_active_minutes_per_user)

colnames(average_active_minutes_per_user)[colnames(average_active_minutes_per_user)=="daily_activity$Id"]<- "User_ID"

colnames(average_active_minutes_per_user)[colnames(average_active_minutes_per_user)=="daily_activity$VeryActiveMinutes"]<- "Average_Active_Minutes_per_User"

#Merged the two new data frames that were created from merging
final_merged_df<- merge(merged_df, merged_df_2, by = "User_ID", all.x= TRUE)

View(final_merged_df)

#Added a new dataframe to the final merged dataframe
final_merged_df<-merge(final_merged_df, average_active_minutes_per_user, by = "User_ID", all.x= TRUE)

#Aggregated the average values stored in two columns and made into a data frames
total_minutes_spent_asleep<- aggregate(sleep_df$TotalMinutesAsleep~sleep_df$Id, data=sleep_df, FUN= sum)

total_time_in_bed<- aggregate(sleep_df$TotalTimeInBed~sleep_df$Id, data=sleep_df, FUN= sum)

View(total_minutes_spent_asleep)

View(total_time_in_bed)

#Aggregated the average values stored in a column and made into a data frame
average_bmi_per_user<- aggregate(weight_df$BMI~weight_df$Id, data=weight_df, FUN= mean)

View(average_bmi_per_user)

#Change column names
colnames(total_minutes_spent_asleep)[colnames(total_minutes_spent_asleep)=="sleep_df$Id"]<- "User_ID"

colnames(total_minutes_spent_asleep)[colnames(total_minutes_spent_asleep)=="sleep_df$TotalMinutesAsleep"]<- "Total_Minutes_Spent_Asleep"


#Change column names
colnames(total_time_in_bed)[colnames(total_time_in_bed)=="sleep_df$Id"]<- "User_ID"

colnames(total_time_in_bed)[colnames(total_time_in_bed)=="sleep_df$TotalTimeInBed"]<- "Total_Time_In_Bed"

#Change column names
colnames(average_bmi_per_user)[colnames(average_bmi_per_user)=="weight_df$Id"]<- "User_ID"

colnames(average_bmi_per_user)[colnames(average_bmi_per_user)=="weight_df$BMI"]<- "BMI_per_User"

#Merged the newly created data frames to the fianl data frame
final_merged_df<-merge(final_merged_df, total_minutes_spent_asleep, by = "User_ID", all.x= TRUE)

final_merged_df<-merge(final_merged_df, total_time_in_bed, by = "User_ID", all.x= TRUE)

final_merged_df<-merge(final_merged_df, average_bmi_per_user, by = "User_ID", all.x= TRUE)


#Calculated and printed the correlation value between two variables
correlation <- cor(final_merged_df$Average_Steps_Taken_per_User, final_merged_df$Average_Calories_per_User)

print(correlation)

#Plotted a scatter plot and made a correlation line between the two variables
ggplot(final_merged_df, aes(x=Average_Calories_per_User, y=Average_Steps_Taken_per_User)) + 
  geom_point() + geom_smooth(method="lm", formula= y~x, color ="blue", se= FALSE) +
  labs(title="Correlation Between Steps Taken And Calories Burnt", x= "Average Calories Burnt per User", y="Average Steps Taken per User", caption= paste("Correlation:", round(correlation,2))) + 
  theme_minimal()

#Calculated and printed the correlation value between two variables
correlation_2 <- cor(final_merged_df$Average_Calories_per_User, final_merged_df$Average_Active_Minutes_per_User)

print(correlation_2)

#Plotted a scatter plot and made a correlation line between the two variables
ggplot(final_merged_df, aes(x=Average_Calories_per_User,y=Average_Active_Minutes_per_User))+
  geom_point() + geom_smooth(method="lm", formula= y~x, color ="red", se= FALSE) +
  labs(title="Correlation Between Activeness And Calories Burnt", x= "Average Calories Burnt per User", y="Average Active Minutes per User", caption= paste("Correlation:", round(correlation_2,2))) + 
  theme_minimal()
