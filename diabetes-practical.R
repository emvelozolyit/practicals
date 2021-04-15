#Read in the diabetes file
diabetes_data  <- read.csv("Diabetes-md.csv", na = "")

#What type is the data?
str(diabetes_data)
class(diabetes_data)
dim(diabetes_data)
row(diabetes_data)
  
#Examine missing data
diabetes_data[!complete.cases(diabetes_data), ]
#or install mice package if not already installed
install.packages("mice")
library(mice)
md.pattern(diabetes_data)
#or
install.packages("VIM")
library(VIM)
missing_values <- aggr(diabetes_data, 
                       prop = FALSE, numbers = TRUE)

#Show the content of missing values
summary(missing_values)

#Decide what to do with the missing data
#What effect will it have on remaining data
diabetes_data$Status[diabetes_data$Status == ""] <- NA