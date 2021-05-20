# Multiple Linear Regression (MLR)

# Step 1: Check the model assumptions
    # linearity - linear relationship?
    # normality - residuals normally dist?
    # homoscedastu=icity - residuals have a constant variance
    # no collinearity - not linear correlations of eachother 
    # independence - residuals are independent and not correlated

# use the state.x77 dataset - base package
help("state.x77")
head(state.x77, 15)
class(state.x77)
# convert to a data frame
states <- as.data.frame(state.x77)
class(states)
head(states, 15)
str(states)

colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

# check for NA's
# using VIM or Mice

# Check for Linearity 
# Only going to check the specific variables I will need
names(states)
# could remove a subset of the data first
# or
# choose the variables and then show them in the pairs() function
# More variables = more complex = don't just choose two 
# Simple model = simple result
variables_of_interest <- c("Murder",
                          "Population", 
                          "HS_Grad", 
                          "Illiteracy", 
                          "Income",
                          "Life_Exp",
                          "Area",
                          "Frost")

pairs(states[variables_of_interest])

# Or

library(psych)
attach(states)
pairs.panels(states, 
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

# These two charts give a general view of correlation

# Independent variable = x-axis
# Dependent variable = y-axis

attach(states)
scatter.smooth(x = Murder, 
               y = Population,
               main = "Murder ~ Population",
               xlab = "Murder (per 100,000)",
               ylab = "Population (Estimate)")
# Low or no correlation
# lets check the correlation numerical
cor(Murder, Population)
# The correlation test shows the the correlation between the 
# murder and population variables = 0.3436428, indicating medium correaltion.
# -0.2 < x < 0.2 - low corr

# What about Murder and Frost?
scatter.smooth(x = Murder, 
               y = Frost,
               main = "Murder ~ Frost",
               xlab = "Murder (per 100,000)",
               ylab = "Frost (Mean Min Temp Below Freezing")
cor(Murder, Frost)
# -0.5388834 - negative high corr


paste("correlation for murder and frost: ", cor(Murder, Frost))
paste("correlation for murder and illiteracy: ", cor(Murder, Illiteracy))
paste("correlation for murder and population: ", cor(Murder, Population))
paste("correlation for murder and HS grad: ", cor(Murder, HS_Grad))
paste("correlation for murder and income: ", cor(Murder, Income))
paste("correlation for murder and life expectancy: ", cor(Murder, Life_Exp))
paste("correlation for murder and area: ", cor(Murder, Area))

# Decided to remove the area variable because of low correlation
states <- subset(states, select = -c(Area))
head(states)

# Check for outliers
# Outliers wont be representative of the true data that it should be repping
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2)) # Charts shown in 4 rows by 2 cols

boxplot(Murder, 
        main = "Murder", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out))

boxplot(Population, 
        main = "Populationr", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out))

boxplot(HS_Grad, 
        main = "Graduation", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(HS_Grad)$out))

boxplot(Illiteracy, 
        main = "Illiteracy", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out))

boxplot(Income, 
        main = "Income", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out))

boxplot(Frost, 
        main = "Frost", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out))

boxplot(Life_Exp, 
        main = "Life Expectancy", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Life_Exp)$out))

par(opar)
# Resetting parameters
# SUB = subtitle
# Paste = lets you build up a string

# Boxplots show..
# 1 in income
# 2 in population.. not a representative of the rest of the data
# going to skew results, need to remove

# going to use boxplot.stats() function to extract the outliers
outlier_values <- boxplot.stats(Population)$out
paste("Population outliers: ", paste(outlier_values, collapse = ", "))

# repeat for the income variable
outlier_values <- boxplot.stats(Income)$out
paste("Income outliers: ", paste(outlier_values, collapse = ", "))

# Remove population outliers
states <- subset(states, 
                 Population != 21198
                 & Population != 11197
                 & Population != 18076
                 & Population != 11860
                 & Population != 12237)

# Remove outcome outliers
states <- subset(states, 
                 Income != 6315)

# Prove it worked 
# Reattach for updated new variable
attach(states)

boxplot(Population, 
        main = "Populationr", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out))

boxplot(Income, 
        main = "Income", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out))

# Decide whether to delete a few more outliers and what this results in

# Check normality
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2))

plot(density(Murder), 
     main = "Density Plot: Murder",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(Murder), 2)))
# Fill the area under the plot
polygon(density(Murder), col = "red")

# Repeat for all variables 
# See predicitve modelling practical for skewness scale

paste("Skewness for illiteracy: ", round(e1071::skewness(Illiteracy), 2))
# "Skewness for illiteracy:  0.87"
# Do this for all - skewness/normal dist.
# Create a table of the values with a decision alongside

# Check normality using QQNorm
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
hist(Murder, main = "Normality Proportion of Murder",
     xlab = "Murder Rate")
qqnorm(Murder)
qqline(Murder)
par <- opar
# seems to not be normally distributed


# MLR Model - Build the Model
attach(states)
mlr_model <- lm(Murder ~ Illiteracy + Population + HS_Grad + 
                  Income + Frost, data = states)
summary(mlr_model)

# Pr(>|t|)  = p-value
# The variables don't seem to be great
# p-values suggest that there is some sort of correlation 

# Estimate - 0.00004880 Population
# This means an increase in 1% in murder is associated with 0.0004880
# increase in the population 

# Frost, HS Grad, somewhat population.. showing relationship
# when splitting train and test, might be better


