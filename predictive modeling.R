# Use dataset called 'women' containing
# height and weight data for 15 qwomen between 30-39
# dependent variable - weight
# independent variable - height

attach(women)
simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model
# dependent~independent

plot(height, weight, 
     main = "Scatter Plot Showing Regression Line for Weight
     Predicted by Height",
     xlab = "Height (Inches)",
     ylab = "Weight (lbs)")
abline(simple_linear_model)

# Let's look at the summary of the data
summary(simple_linear_model)
# Call - exactly what we type in 
# Residuals- shows quick view of distribution of them (min = 1.7, max = 3.1.. 
      # generally in same range of one another which is good)
# Coefficients = height is significant (***)
      # std error - accuracy of coeff - confidence interval
# t- value - known as statistics and pvalue.. stat sign.
    # high t-statistic, lower p value = more signifcant this variable is
# large t-value, small p-value and *** all show how it is significant
# Intercept - is where the line intercepts the chart (-87.1)
      # every unit of change, it will change by 3.45000 (estimate/height)
      # -87.52+3.45X HEIGHT

# Correlation co-efficient of 2 variables
confint(simple_linear_model)
# 2.5% and 97.5% -- how much down to chance the conf. int could be affected
# telling us there is a good 2 corr between 2 variables

#correlation
cor(height, weight)
# 0.99 = strong positive

# accuracy - goodness of fit
# how well does the simple_linear_model fit the dataset
summary(simple_linear_model)
# residual standard error.. smallest RSE, best fits data
# deviates by 1.525 on avg
# high value of r squared is good
# consider adjusted r-square for multiple linear regression
# larger proporativon of variablity has been explain = close to 1
# model cant exaplin a lot of variables - close to 0
# f-statistic - overall sign. of model
    # large f = statistically sign. p-value (less the 0.05)


# Predict stopping distance of a car by using the speed of the car
# MTcars dataset

# First step - check model assumptions
# a model will have certain assumptions, make sure variables meet these
# Core assumptions for linear models:
      # linearity - linear relationship?
      # normality - residuals normally dist?
      # homoscedastu=icity - residuals have a constant variance
      # no collinearity - not linear correlations of eachother 
      # independence - residuals are independent and not correlated

# Check linearity 
# dependent - y axis (dist)
# independent - x axis (speed)
detach(mtcars)
attach(cars)
str(cars)
scatter.smooth(x = speed, y = dist,
               main = "Distance with Respect to Speed",
               xlab = "Car Speed (MPH)",
               ylab = "Stopping Distance (Feet)")
# there is def a positive correaltion bw two variables
# but some outliers
# gg plot (see notes)


# Check correlation
cor(speed, dist)
# high correlation

# Check for outliers (boxplot) 
# boxplot shows variance between two variables too
# outlier = 1.5 * IQR
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # 1 row x 2 cols

boxplot(speed, 
        main = "Speed", 
        sub = paste("Outlier Rows: ",
                    boxplot.stats(speed)$out))
# sub = below boxplot
boxplot(dist, 
        main = "Distance", 
        sub = paste("Outlier Rows: ",
                    boxplot.stats(dist)$out))
par <- opar
# outlier of 120, going to remove 

# remove one outlier (subsetting) where dist = 120
attach(cars)
cars <- subset(cars, cars$dist !=120)
nrow(cars)
# != 120 --> not equal to 120
# run boxplots again to see if outlier is gone

# Check for normality 
install.packages("e1071")
library(e1071)

# Skewness will tell us if it is normally dist.
# Skewness - highly skewed <-1 or >1
          # moderately skewed = -1 to -0.5 and 0.5 to 1
          # apprrox symmetrical -0.5 to 0.5

opar <- par(no.readonly = TRUE)
plot(density(speed), 
     main = "Density Plot: Speed",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(speed), 2)))
# 2 = two decimal points
# fill in the area under the plot with red
polygon(density(speed), col = "red")
# Don't worry about bandwidth
# skewness = -0.11 -- this means approx. symmetrical
# we have proven speed is approx. normally distributed

# now do same for dist
plot(density(dist), 
     main = "Density Plot: Dist",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(dist), 2)))

polygon(density(dist), col = "red")
# Skewness = 0.5 --> this means moderately skewed

par <- opar

# the other way to do this is qqnorm()
# you choose which way you want to do this for CA2
opar <- par(no.readonly = TRUE)
par(mfrom = c(1,2))
hist(dist)
qqline(dist)
par <- opar

# Create training and testing dataset 
set.seed(1) #random pick data.. same random variablitiy
no_rows_data <- nrow(cars)
data_sample <- sample(1:no_rows_data, 
                      size = round(0.7 * no_rows_data),
                      replace = TRUE)
# size 70% of total sample
# replace = true- put record back in (if you have less records)
# replace = false- dont put record back in (if you have enough records)

training_data <- cars[data_sample, ] # 70%
testing_data <- cars[-data_sample, ] # 30%
# 34 + 15 = 49 

linear_mod <- lm(dist ~ speed, data = training_data )
# training data is the data you use to train the data
linear_mod
summary(linear_mod)
# p-value < 0.05 so coeff are stat sign. diff than 0
# *** = significant
# t-value higher the better


# Prediction
predicted_distance <- predict(linear_mod, testing_data)

actuals_predictions <- data.frame(cbind(actuals = testing_data$dist,
                                        predicted = predicted_distance))
head(actuals_predictions, 15)
            
#cbind = combine data into dataframe

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy


               