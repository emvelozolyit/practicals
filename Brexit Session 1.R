# Import the dataset.
# You'll need to copy the dataset to the working directory in RStuido
brexit <- read.csv("data_brexit_referendum.csv", na = "")

# Examine the data type and structure of the imported structued
str(brexit)
class(brexit)
head(brexit, 5)

# Count the number of values with "-1" in the 'Leave' column
nrow(brexit[brexit$Leave == -1,])
sum(brexit$Leave[brexit$Leave == -1])

# Replace these with NA
brexit$Leave[brexit$Leave == -1] <- NA

# Verify that the "-1" values have been replaced
sum(brexit$Leave[brexit$Leave == -1])

# View the no. of records with NA
# Use the complete cases command
na_records <- brexit[!complete.cases(brexit),]

# Count the no. of NA records
nrow(na_records)

# Visualise the patterns of NA data
library(mice)
md.pattern(brexit)

# VIM
library(VIM)
missing_values <- aggr(brexit, prop = FALSE, numbers = TRUE)
summary(missing_values)

# Looking at the proportion of voters
# who are in favor of leaving the EU
# Create a new variable to decide whether each area (ward)
# decided to remain or leave the EU
# Examine the proporation of leave and NVotes
brexit$Proportion <- brexit$Leave / brexit$NVotes


# Create a new variable and figure out
# whether each ward decided to leave or remain
# If proportion < 0.5 = remain 
# If proportion > 0.5 = leave
brexit$Vote[brexit$Proportion <= 0.5] <- "Remain"
brexit$Vote[brexit$Proportion > 0.5] <- "Leave"

str(brexit)
# Variable is a character, so no need to convert first

# Convert
attach(brexit)
brexit$RegionName[RegionName == "London"] <- "L" 
brexit$RegionName[RegionName == "North West"] <- "NW"
brexit$RegionName[RegionName == "North East"] <- "NE"
brexit$RegionName[RegionName == "South West"] <- "SW"
brexit$RegionName[RegionName == "South East"] <- "SE"
brexit$RegionName[RegionName == "East Midlands"] <- "EM"
brexit$RegionName[RegionName == "West Midlands"] <- "WM"
brexit$RegionName[RegionName == "East of England"] <- "EE"
brexit$RegionName[RegionName == "Yorkshire and the Humber"] <- "Y"

summary(brexit)

is.numeric(Proportion)
is.numeric(RegionName)

# Use sapply function to examine the structure
# of each variable
numeric_variable_list <-sapply(brexit, is.numeric)
class(numeric_variable_list)
numeric_variable_list

# We can use this logic to create a subset of the data
numerical_data <- brexit[numeric_variable_list]
head(numerical_data, 5)

# Remove the ID field as it is of no benefit
# Remove from the numerical_variable_list
numeric_variable_list["ID"] <- FALSE

# We can use this logic to create a subset of the data again
# now that the ID field is removed
numerical_data <- brexit[numeric_variable_list]
head(numerical_data, 5)

# We can use the apply() function to return a list
# where each list member has a corresponding name
lapply(numerical_data, summary)
