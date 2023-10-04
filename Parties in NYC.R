#Installing and loading packages needed for analysis and importing the dataset to be used
library(tidyverse)
library(dplyr)
party_in_nyc<- read.csv("C:/Users/Captain Joe/Documents/Datasets/party_in_nyc.csv")
View(party_in_nyc)

#Viewing and having an idea of the dataset
party_in_nyc_2<- party_in_nyc
View(party_in_nyc_2)
head(party_in_nyc_2)
tail(party_in_nyc_2)
colnames(party_in_nyc_2)[colnames(party_in_nyc_2)=="Closed.Date.1"]<- "Closed_Time"

#Calculating counted and mean values and importing them into new data frames
total_number_of_parties_by_location <- party_in_nyc_2 %>%
  group_by(party_in_nyc_2$Location.Type) %>%
  summarise(Count = n())
View(total_number_of_parties_by_location)

total_number_of_parties_by_region <- party_in_nyc_2 %>%
  group_by(party_in_nyc_2$Borough, party_in_nyc_2$Incident.Zip) %>%
  summarise(Count = n())
View(total_number_of_parties_by_region)

total_number_of_parties_by_region_2 <- party_in_nyc_2 %>%
  group_by(party_in_nyc_2$Borough) %>%
  summarise(Count = n())
View(total_number_of_parties_by_region_2)

total_number_of_parties_by_city <- party_in_nyc_2 %>%
  group_by( party_in_nyc_2$City) %>%
  summarise(Count = n())
View(total_number_of_parties_by_city)

average_party_starting_time<- party_in_nyc_2 %>%
  group_by( party_in_nyc_2$Created.Time) %>%
  summarise(mean = n())
View(average_party_starting_time)

#Changing the column names of a dataframe
colnames(total_number_of_parties_by_location)[colnames(total_number_of_parties_by_location)=="party_in_nyc_2$Location.Type"]<- "Location_Type"

colnames(total_number_of_parties_by_location)[colnames(total_number_of_parties_by_location)=="Count"]<- "Total_Number_of_Parties"

#Changing the column names of a dataframe
colnames(average_party_starting_time)[colnames(average_party_starting_time)=="party_in_nyc_2$Created.Time"]<- "Created_Time"

colnames(average_party_starting_time)[colnames(average_party_starting_time)=="Average_Starting_Time"]<- "Average_Number_of_Parties"

#Changing the column names of a dataframe
colnames(total_number_of_parties_by_city)[colnames(total_number_of_parties_by_city)=="party_in_nyc_2$City"]<- "City"

colnames(total_number_of_parties_by_city)[colnames(total_number_of_parties_by_city)=="Count"]<- "Total_Number_of_Parties"

#Changing the column names of a dataframe
colnames(total_number_of_parties_by_region_2)[colnames(total_number_of_parties_by_region_2)== "party_in_nyc_2$Borough"] <- "Borough"

colnames(total_number_of_parties_by_region_2)[colnames(total_number_of_parties_by_region_2)== "Count"] <- "Total_Number_of_Parties"

#Changing the column names of a dataframe
colnames(total_number_of_parties_by_region)[colnames(total_number_of_parties_by_region)== "party_in_nyc_2$Borough"] <- "Borough"

colnames(total_number_of_parties_by_region)[colnames(total_number_of_parties_by_region)== "party_in_nyc_2$Incident.Zip"] <- "Incident_Zip"

colnames(total_number_of_parties_by_region)[colnames(total_number_of_parties_by_region)== "Count"] <- "Total_Number_of_Parties"


#Plotting the third visualization
ggplot(total_number_of_parties_by_location, aes(x=Location_Type ,y=Total_Number_of_Parties))+
  geom_bar(stat="identity", fill="maroon") +
  labs(title="Number of Parties held between 31/12/2015-01/01/2017", x="Location Type", y="Number of Parties held")+
  theme(axis.text.x= element_text(angle= 45, hjust= 1))

#Plotting the third visualization
ggplot(total_number_of_parties_by_region_2, aes(x=Borough ,y=Total_Number_of_Parties))+
  geom_bar(stat="identity", fill="purple") +
  labs(title="Number of Parties held between 31/12/2015-01/01/2017", x="Regions", y="Number of Parties held")

#Plotting the third visualization
ggplot(total_number_of_parties_by_region, aes(Incident_Zip,Total_Number_of_Parties))+
  geom_point(aes(colour = Borough))+
  labs(title="Number of Parties held between 31/12/2015-01/01/2017", x= "Reported Incident Zips", y="Total Number of Parties")





