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
#What effect will it have on remaining data?
diabetes_data$Status[diabetes_data$Status == ""] <- NA
#or
#I've decided to keep only data with missing type and 
#health status
#and the addresses are not important
keep_this_data <- diabetes_data[!complete.cases(diabetes_data$Diabetes.type, diabetes_data$Status),]
keep_this_data
dim(keep_this_data)
#15 rows with missing data that I'd like to keep
#Reverse the logic to contain relevant data 
keep_this_data <- diabetes_data[complete.cases(diabetes_data$Diabetes.type, diabetes_data$Status),]
keep_this_data

dim(keep_this_data)
dim(diabetes_data)

#Replace the original data frame with the content of the processed data
diabetes_data <- keep_this_data
head(diabetes_data, 15)

#4- configure type to an unordered factor with 2 levels
diabetes_data$Daibetes.type<- factor(diabetes_data$Daibetes.type, order = FALSE, levels = c("Type 1","Type2"))

#Refactor status too
diabetes_data$Status <- factor(diabetes_data$Status, order = TRUE, levels = c("Poor", "Improved","Excellent"))

#5- Define the new column names
col_names <- c("Patient Name","NI Address", "Type", "Age", "Health Status")
colnames(diabetes_data) <- col_names

str(diabetes_data)

#Lets look at the class of each element
class_list <- lapply(diabetes_data, class)
class_list

class_list <- sapply(diabetes_data, class)
class_list


#Charts ------------------------------

#Build the data frame first
dose <- c(20,30,40,45,60)
drug_a <- c(16,20,27,40,60)
drug_b <- c(15,18,25,31,40)

#Create a new data frame
drugs <- data.frame(dose, drug_a, drug_b)
drugs

#General plot of data 
plot(drugs)

attach(drugs)
plot(dose, type = "o", col = "blue")
?plot

#Option type - "b" shows both lines should be plotted
plot(dose, drug_a, type = "b")
