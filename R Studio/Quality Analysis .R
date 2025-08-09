library(dplyr)
library(ggplot2)
setwd("/Users/liana/Desktop/MGT585/Data")

# Reading Dataset
quality <- read.csv(file = "quality.csv")

# Exploring Dataset

dim(quality)

str(quality)

colnames(quality)

head(quality)

tail(quality)

# Correct the type of R objects

# am takes two values and is a factor
table(quality$am)
quality$am<- as.factor(quality$am)

#We need dummy variables for the column 'am' because it is a factor. 
# But we do not need to manually enter them as R-studio will do that for us. 

################################################################################
# Descriptive statistics
################################################################################

# Descriptive stats for continuous variables: mean, median, min, max, sd
# defect

summary(quality$defect)
sd(quality$defect)

# temp

summary(quality$temp)
sd(quality$temp)

# density

summary(quality$density)
sd(quality$density)

# rate

summary(quality$rate)
sd(quality$rate)

# descriptive stats for categorical variable: frequency distribution

# am

table(quality$am)

################################################################################
# Scatter Plots
################################################################################

# independent variable 1 and dependent variable
## X is temp (represents temperature variability as a standard deviation 
##           during the time of measurement)
## Y is defect

ggplot(quality, mapping = aes(x=temp, y=defect)) + geom_point() + 
  geom_smooth(method = "lm",se = FALSE, colour = "#191970") + 
  ggtitle("Impact of Temperature Variability on Defects") + 
  xlab("Temperature") + ylab("Defects")

# independent variable 2 and dependent variable
## X is density (represents density of final product)
## Y is defect

ggplot(quality, mapping = aes(x=density, y=defect)) + geom_point() + 
  geom_smooth(method = "lm",se = FALSE, colour = "#3CB371") + 
  ggtitle("Impact of Density on Defects") + 
  xlab("Density") + ylab("Defects")

# independent variable 3 and dependent variable
## X is rate (represents rate of production)
## Y is defect

ggplot(quality, mapping = aes(x=rate, y=defect)) + geom_point() + 
  geom_smooth(method = "lm",se = FALSE, colour = "#DC143C") + 
  ggtitle("Impact of Rate of Production on Defects") + 
  xlab("Rate") + ylab("Defects")

# independent variable 4 and dependent variable
## X is am (1 indicates morning shift and 0 afternoon shift)
## Y is defect

ggplot(quality, aes(x=am, y=defect, fill = am)) + 
  geom_bar(stat="identity") + ggtitle("Impact of Work Shift on Defects") +
  xlab("Work Shift") +ylab("Defects") +
  scale_fill_manual(values = c("0" = "#DDA0DD", "1" = "#F4A460"))

#Conclusions:
 #1: The scatter plot "Impact of Temperature Variability of Defects" shows that as 
#    the temperature variability increases the quantity of defects increases.

 #2: The scatter plot "Impact of Density on Defects" shows that as 
#    density increases the quantity of defects decreases.

 #3: The scatter plot "Impact of Rate of Production on Defects" shows that as 
#    the production rate increases the quantity of defects increases.

 #4: The bar chart "Impact of Work Shift on Defects" shows that  
#    the morning shift had significantly more defects than the afternoon shift.

################################################################################
# Regression Summaries
################################################################################
# X is Temperature Variability
# Y is Defects

reg_temperature <- lm(defect ~ temp, data = quality)
summary(reg_temperature)

# p-value: a: p < .05, b: p < .05
# coefficients: a: -40.966, b: 30.915
# This means 1 unit increase in temperature variability leads to a 30.915 unit 
# increase in defects.
# R square: 85.83%
# f-stats: 176.6 > 1

# The analysis indicates that higher variability in temperature is associated with an 
# increase in defects, with the model explaining about 85.83% of the variation in defects.

################################################################################
# X is Density (density of the final products)
# Y is Defects

reg_density <- lm(defect ~ density, data = quality)
summary(reg_density)

# p-value: a: p < .05, b: p < .05
# coefficients: a: 161.979, b: -5.333
# This means 1 unit increase in density leads to a 5.333 unit decrease in defects.
# R square: 84.73%
# f-stats: 162 > 1

# The analysis indicates that lower density in final products is associated with a higher 
# number of defects, with the model explaining about 84.73% of the variation in defects.

################################################################################
# X is Rate
# Y is Defects

reg_rate <- lm(defect ~ rate, data = quality)
summary(reg_rate)

# p-value: a: p < .05, b: p < .05
# coefficients: a: -128.90616, b: 0.65977
# This means 1 unit increase in the rate of production leads to a .65977 unit 
# increase in defects.
# R square: 77.61%
# f-stats: 101.5 > 1


# The analysis indicates that higher rate of production is associated with an 
# increase in defects, with the model explaining about 77.61% of the variation in defects.

################################################################################
##                       Dummy variable regression
################################################################################

# X is AM (1 indicates morning shift and 0 afternoon shift)
# Y is Defects

reg_am <- lm(defect ~ am, data = quality)
summary(reg_am)

# p-value: a: p < .05, b: p < .05
# coefficients: a: 16.920 b: 20.440
# This means when production is done in the morning shift as opposed to the afternoon, 
# the number of defects increases by 16.920.
# R square: 26.12%
# f-stats: 11.25 > 1

# The analysis indicates that the morning shift is associated with the increase in 
# defects, with the model explaining about 26.12% of the variation in defects.

################################################################################
# FINAL RECOMMENDATION

# My final recommendation is to slow down the production rate to reduce 
# the number of defects.

# The analysis, including the scatter plot and dummy variable regression, indicates that 
# this adjustment is particularly important for the morning shift.
################################################################################

