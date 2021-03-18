# Enter data into vectors before constructing the data frame
date_col <- c("10/15/18", "01/11/18", "10/21/18", "10/28/18", "05/01/18")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99)
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2) # NA is inserted in place of the missing data for this attribute
q5_col <- c(5, 5, 2, NA, 1)

# Construct a data frame using the data from all the vectors
managers_data <- data.frame(date_col, 
                            country_col, 
                            gender_col, 
                            age_col, 
                            q1_col, 
                            q2_col, 
                            q3_col, 
                            q4_col, 
                            q5_col)

managers_data

column_names <- c("Date", 
                  "Country", 
                  "Gender", 
                  "Age", 
                  "Q1", 
                  "Q2", 
                  "Q3", 
                  "Q4", 
                  "Q5")

# Add column names to the managers_data dataframe
colnames(managers_data) <- column_names

managers_data

# Recode the incorrect 'age' to NA
managers_data$Age[managers_data$Age == 99] <- NA
managers_data

# 2 options to create a new variable
# 1 - create a new vector and store the logical check in it
# 2 - create the new var when doing the logical check

managers_data$AgeCat[managers_data$Age >= 45] <- "Elder"
managers_data$AgeCat[managers_data$Age >= 26 & managers_data$Age <= 44] <- "Middle Aged"
managers_data$AgeCat[managers_data$Age <= 25] <- "Young"
managers_data$AgeCat[is.na(managers_data$Age)] <- "Elder"
managers_data

# recode age_cat so that it is ordinal and factored
# with the order young, middle aged, elder
age_cat <- factor(managers_data$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))
age_cat

# replace mnager_data age_cat variable with
# the factored variable

managers_data$AgeCat <-age_cat
managers_data

# Look at the structure of the data frame
str(managers_data)



# Create a new column called 'summary_col' that
# contains a summary of each row
summary_col <- managers_data$Q1 + 
  managers_data$Q2 + 
  managers_data$Q3 + 
  managers_data$Q4 + 
  managers_data$Q5

summary_col

# Add summary_col to the end of the data frame
# take content of data frame, create a new data frame
# and add summary_col vector to the data frame 

managers_data <- data.frame(managers_data, summary_col)
managers_data

# Calculate mean value for each row
mean_value <-rowMeans(managers_data[5:9])
mean_value

# Add the mean_value vector to the data frame

managers_data <- data.frame(managers_data, mean_value)
managers_data

# Change variable titles to make them more readable 
names(managers_data)[11] <- "Summary"
names(managers_data)[12] <- "Mean Value"

# Show the structure of the data frame
str(managers_data)
managers_data
