# Use statistical methods to examine
# the relationships between our variables of interest

# Beavers dataset
# contains the body temp of 4 beavers captued every 10 mins
# We want to examine difference in average body temp
# to evaluate whether body temp is affected by activity

# Question 1
# Hypothesis test
#h0: mean beaver temperature is not affected by activity
#h1: mean beaver temperature is affected by activity

?beavers
str(beaver2)

# Copy the data into a data frame
beavers_data <- beaver2
str(beavers_data)

# Still Question 1 
# Describe the variable you will use for analysis
# vars we will need
# we'll evaluate beaver temp and activity

# What type of variables are they?
# temp is continuous variable
# activity is categorical dicotomous variable

# Convert the activity variable to 
# a categorical dichotomous variable
beavers_data$activ <- factor(beavers_data$activ, labels = c("No", "Yes"))
str(beavers_data)

install.packages("psych")
library(psych)

pairs.panels(beavers_data, 
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
# In own project, change name of titles to make look better
attach(beavers_data)
plot(activ, temp, pch = 9, col = "lightblue")
# This doesnt tell us much bc two variables are diff types

# We can split the dicotomous variable into 2
# and then examine the data
library("lattice")

#
histogram(~temp | activ,
          data = beavers_data,
          main = "Distribution of Beave Activity Data",
          xlab = "Temperature (Degrees)",
          ylab = "Activity")
# Visual analysis seems to indicate that the data is normally distributed
# Summarise the medians of the data to see if the data is normall dist. or not
tapply(temp, activ, median)
# They seem to be generally the same, this is the center point of both sides of
# the variable
# Still not sure about distribution

# Quantile-quantile plot (Q-Q) plot allows us to check
# if the data is normally distributed or not
# Compare quantiles of both samples 

# is temp normally dist?
qqnorm(temp)
# Add line that represents normal dist.
qqline(temp, col = "red")

# Temp appears not to be normally distributed

# Is activity normally distributed? 
with(beavers_data, qqplot(temp[activ == "Yes"], temp[activ == "No"],
                          main = "Comparing 2 samples of activity data",
                          xlab = "Active temp = Yes",
                          ylab = "Active temp = No"))
#qqplot and qqnorm are the same things

# We can add normality line to the plot
# to help evalute normality 
with(beavers_data, {
  qqnorm(temp[activ == "No"],
              main = "Inactive data")
              qqline(temp[activ == "No"])})

with(beavers_data, {
  qqnorm(temp[activ == "Yes"],
         main = "Active data")
  qqline(temp[activ == "Yes"])})

# Formal test of normality
# Shapiro-Wilks test
# P value tells us the chances that the sample comes from a normal distribution
# If p > 0.05 then, normally dist.
normality_test <- shapiro.test(beavers_data$temp)
normality_test$p.value
#p-value = 7.763623e-05.. this is less than 0.05, so not normally dist.

# This test does not work on dicotomous variable
with(beavers_data, tapply(temp, activ, shapiro.test))

#  Results show: 
# No p-value= 0.1231 - normally distributed
# Yes p-value= 0.5583 - normally distributed 

# activity variable is normally dist.
# temp variable is not normally dist.

# whenever we looked at correlations, some did and some didnt look
# normally dist
# we weren't really sure
# it could go either way
# but now we know whats happening 

# final part
# if we have a not normally dist. variable you are better off
# to go for a not normally dist. test

# After consulting the chart, I am amining 
# a dependent variable (temp)
# with an independent vategorical var (activity)
# temp relies on activity
# activity doesnt rely on activity 

# format wilcox.test(dependent ~ independent)
wilcox.test(temp~activ)
#cut-off = 0.05
#p-values =< 2.2e-16 (0.00000000000000022)
#Reject HO
