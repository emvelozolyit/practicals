#Import the diabetes data available on Balckboard
#called diabetes-md into a new dataframe called
#diabetes_data
diabetes_data <- read.csv("diabetes.csv", na = "")

str(diabetes_data)

#Check whether there are any missing variables
#If there are, delete them
#Examine missing data
incomplete_data <- diabetes_data[!complete.cases(diabetes_data), ]
incomplete_data
#or install mice package if not already installed
install.packages("mice")
library(mice)
md.pattern(diabetes_data)
#or
install.packages("VIM")
library(VIM)
missing_values <- aggr(diabetes_data, 
                       prop = FALSE, numbers = TRUE)

# Show content of missing values
summary(missing_values)


#Create new variable called to Date that contains
#the month and year data
diabetes_data$Date <- paste(diabetes_data$Month, diabetes_data$Year, sep='/')
str(diabetes_data)

#Change the date varibale to a Date
#Date has a particular requirement as it should
#contain day, month, and year

#here is an example of the effor
converted_date <- as.Date(diabetes_data$Date, "%m/%Y")
converted_date
class(converted_date)

#This is how it is fixed
#The data should contain dd/mm/yyyy
converted_date <- paste(diabetes_data$Month, diabetes_data$Year, sep="/")
converted_date

#Add the day element into the variable
converted_date <- paste("01", diabetes_data$Month, diabetes_data$Year, sep="/")
converted_date

diabetes_data$Date <- as.Date(converted_date, "%d/%m/%Y")
str(diabetes_data)

#Plot the status variable using the plot() function
#Convert to a factor first
plot(diabetes_data$Status)
diabetes_data$Status <- factor(diabetes_data$Status)
str(diabetes_data)
plot(diabetes_data$Status)
summary(diabetes_data$Status)

#Add titles to the chart that are relevant
attach(diabetes_data)
display_settings <- par(no.readonly = TRUE)
plot(Status, main = "Number of patient recoveries", 
     xlab = "Diabetes status", 
     ylab = "No of patients")

#Save the modified diabetes dataframe
write.csv(diabetes_data, file = "diabetes-data-modified.csv")
