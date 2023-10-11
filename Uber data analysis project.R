#Loaded necessary packages and loaded the dataset to be used
library(tidyverse)
uber_dataset <- read.csv("C:/Users/Captain Joe/Downloads/archive (4)/UberDataset.csv")
View(uber_dataset)

#Noticed a discrepancy in the dataset and corrected it with an ifelse statement
uber_dataset$START<- ifelse(uber_dataset$START=="Kar?chi", "Karachi", uber_dataset$START)
uber_dataset$STOP<- ifelse(uber_dataset$STOP=="Kar?chi", "Karachi", uber_dataset$STOP)


#Aggregated the average value of miles for each trip according to purpose and stored it in a new dataframe
average_number_of_miles_per_purpose<- aggregate(uber_dataset$MILES~uber_dataset$PURPOSE, data=uber_dataset, FUN=mean)
View(average_number_of_miles_per_purpose)

#Changed column names and a column value
colnames(average_number_of_miles_per_purpose)[colnames(average_number_of_miles_per_purpose)=="uber_dataset$MILES"]<- "Average_Number_of_Miles"

colnames(average_number_of_miles_per_purpose)[colnames(average_number_of_miles_per_purpose)=="uber_dataset$PURPOSE"]<- "Purpose"

average_number_of_miles_per_purpose[1, "Purpose"]<- "Unspecified"

#Aggregated the total count of trips according to purpose and stored it in a new dataframe
total_number_of_trips_per_purpose<- aggregate(uber_dataset$START_DATE~uber_dataset$PURPOSE, data=uber_dataset, FUN=length)
View(total_number_of_trips_per_purpose)

#Changed column names and a column value
colnames(total_number_of_trips_per_purpose)[colnames(total_number_of_trips_per_purpose)=="uber_dataset$START_DATE"]<- "Total_Number_of_Trips"

colnames(total_number_of_trips_per_purpose)[colnames(total_number_of_trips_per_purpose)=="uber_dataset$PURPOSE"]<- "Purpose"

total_number_of_trips_per_purpose[1, "Purpose"]<- "Unspecified"

#Aggregated the total count of trips according to category and stored it in a new dataframe
total_number_of_trips_per_category<- aggregate(uber_dataset$START_DATE~uber_dataset$CATEGORY, data=uber_dataset, FUN=length)
View(total_number_of_trips_per_category)

#Changed column names and a column value
colnames(total_number_of_trips_per_category)[colnames(total_number_of_trips_per_category)=="uber_dataset$CATEGORY"]<- "Category"

colnames(total_number_of_trips_per_category)[colnames(total_number_of_trips_per_category)=="uber_dataset$START_DATE"]<- "Total_Number_of_Trips"

total_number_of_trips_per_category[1, "Category"]<- "Unspecified"

#Aggregated the total count of trips according to starting location and stored it in a new dataframe
total_number_of_trips_per_starting_location<- aggregate(uber_dataset$START_DATE~uber_dataset$START, data=uber_dataset, FUN=length)
View(total_number_of_trips_per_starting_location)

#Changed column names and a column value
colnames(total_number_of_trips_per_starting_location)[colnames(total_number_of_trips_per_starting_location)=="uber_dataset$START"]<- "Starting_Point"

colnames(total_number_of_trips_per_starting_location)[colnames(total_number_of_trips_per_starting_location)=="uber_dataset$START_DATE"]<- "Total_Number_of_Trips"

total_number_of_trips_per_starting_location[1, "Starting_Point"]<- "Unspecified"

#Aggregated the total count of trips according to ending location and stored it in a new dataframe
total_number_of_trips_per_ending_location<- aggregate(uber_dataset$END_DATE~uber_dataset$STOP, data=uber_dataset, FUN=length)
View(total_number_of_trips_per_ending_location)

#Changed column names and a column value
colnames(total_number_of_trips_per_ending_location)[colnames(total_number_of_trips_per_ending_location)=="uber_dataset$STOP"]<- "Ending_Point"

colnames(total_number_of_trips_per_ending_location)[colnames(total_number_of_trips_per_ending_location)=="uber_dataset$END_DATE"]<- "Total_Number_of_Trips"

total_number_of_trips_per_ending_location[1, "Ending_Point"]<- "Unspecified"

#Plotting First Visualization
ggplot(average_number_of_miles_per_purpose, aes(x=average_number_of_miles_per_purpose$Purpose, y=average_number_of_miles_per_purpose$Average_Number_of_Miles)) + 
  geom_bar(stat="identity", fill="maroon")+
  labs(title="Average Number Of Miles Covered By Each Purpose", x= "Types of Purpose", y="Average Miles Covered")+
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Plotting Second Visualization
ggplot(total_number_of_trips_per_purpose, aes(x=total_number_of_trips_per_purpose$Purpose, y=total_number_of_trips_per_purpose$Total_Number_of_Trips)) + 
geom_bar(stat="identity", fill="maroon")+
  labs(title="Total Number Of Trips Taken For Each Purpose", x= "Types of Purpose", y="Total Number of Trips")+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Changing values into proportions for the third visualization
total_number_of_trips_per_category<- transform(total_number_of_trips_per_category, Proportion = Total_Number_of_Trips / sum(Total_Number_of_Trips))

# Plotting a third visualization
ggplot(total_number_of_trips_per_category, aes(x = "", y = Proportion, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Trip Category Pie Chart")
