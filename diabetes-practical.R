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

#par - customize any setting such as fonts, colors, etc
#par - parameters

#store the default content of par before we make
#any changes to them
opar <- par(no.readonly = TRUE)

#lty = line type
#lwd = line width
#lty = 2 dashed line
#pch = 17 solid triangle
par(lty = 2, pch = 17)
plot(dose, drug_a, type = "b")
par = opar

plot(dose, drug_a, type = "b", lty = 2, pch = 17)

graph_range <- range(0, drug_a, drug_b)
graph_range
plot(drug_a, type = "o", col = "blue", ylim = graph_range, axes = FALSE, ann = FALSE)
lines(drug_b, type = "o", pch = 22, lty = 2, col = "red")

#make the axes label
axis(1, at = 1:5, labels = c("20 ml", "40 ml", "60 ml", "80 ml", "100 ml"))

#make the y axis that displays ticks at
#every 5 marks
#marks = 5 *

axis(2, las = 1, at = 5 * 0: graph_range[2])
