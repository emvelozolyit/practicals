Merging datasets ----------
  
  #Answers to pracical problems

#Load the dataset into a data frame. 
#Call the data frame new_managers_data
managers_data <- read.csv("managers.csv")
new_managers_data <- read.csv("MoreData.csv")

getwd()
#Show the structure of this data
str(new_managers_data)

#Show the headers from both data frames
names(managers_data)
names(new_managers_data)

include_list <- new_managers_data[c("Date",
  "Country", "Gender", "Age", 
  "Q1","Q2", "Q3", "Q4", "Q5")]

#This is how we will combine the data frames
#but right now it doesnt work
rbind(managers_data, include_list)

#Create a new "AgeCat" variable in include_list
#and then calculate containing values
attach(include_list)
include_list$AgeCat[Age >= 45] <- "Elder"
include_list$AgeCat[Age >= 26 & Age <= 44] <- "Middle Aged"
include_list$AgeCat[Age <= 25] <- "Young"

#If NA is found, categorize as "Elder"
include_list$AgeCat[is.na(Age)] <- "Elder"
detach(include_list)

#Now go back up and try to rbind it ^
#Error still exists

#Drop  variable
str(managers_data)
str(include_list)

include_list <- new_managers_data[c("Date",
                                    "Country", "Gender", "Age", 
                                    "Q1","Q2", "Q3", "Q4", "Q5")]
include_list

names(managers_data)
modified_managers <- managers_data[2:11]
modified_managers

#Now fix the date field
#Update the date fields on both data frames
#so that they are in the correct format

modified_managers$Date <- as.Date(modified_managers$Date, "%m/%d/%y")
str(modified_managers)

include_list$Date <- as.Date(include_list$Date, "%m/%d/%Y")
str(include_list)


#Try rebind now
#Combine both data frames
combined_managers <- rbind(modified_managers, include_list)

#Set AgeCat with ordered factor
#so that young < middle aged < elder

combined_managers <- factor(combined_managers$AgeCat, 
                            levels = c("Young","Middle Aged","Elder"),
                            ordered = TRUE)
str(combined_managers)

