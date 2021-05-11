# Analysing car data

# Question 1
# Hypothesis test
# H0:  car weight has no effect on fuel efficiency 
# H1: car weight has an effect on fuel efficiency  


?mtcars
str(mtcars)

# Copy the data into a data frame
cars <- mtcars
str(cars)

# Check for missing data etc
incomplete_data <- cars[!complete.cases(cars)]
incomplete_data

#Visualise the data fro missing vars
#Use VIM package to show missing values
#install.packages("VIM")
library(VIM)
missing_values <- aggr(cars, prop = FALSE, numbers = TRUE)

# Still Question 1 
# Describe the variable you will use for analysis
# vars we will need
# we'll evaluate mpg and wt 

# What type of variables are they?
# mpg is a continuous variable
# wt is a continuous variable 

# Convert either variables? Nope!


# Check correlations
install.packages("psych")
library(psych)

pairs.panels(cars, 
             smooth = TRUE, # IF TRUE, draws loess smooths
             scale = FALSE, # IF TRUE, scales the correlation text font
             density = TRUE, # IF TRUE, adds density plots and histograms
             ellipses = TRUE, #IF TRUE, draws ellipses
             method = "spearman", # Correlation method (person or kendall)
             pch = 21, #pch symbol
             lm = FALSE, #IF TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # IF TRUE, reports correlations
             jiggle = FALSE, # IF TURE, data points are jittered
             factor = 2, #Jittering factor
             hist.col = 4, #Histogram color
             stars = TRUE, # Stars after numbers, means theres some type of correlation
             ci = TRUE) 

# Question 2

# Check linearity 
attach(cars)
plot(wt, mpg, pch = "19", col = "red", main = "Comparison of car weight with mpg",
     xlab = "Weight (lbs)", ylab = "MPG (Miles/US Gallon)")


