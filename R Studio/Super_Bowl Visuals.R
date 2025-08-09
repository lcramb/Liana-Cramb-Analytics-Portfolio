install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("/Users/liana/Desktop/MGT585/Data")

superbowl<-read.csv("superbowl-2.csv",header=TRUE)


# tell R, new_brand, month and superbowl columns are factor, fill in the missing arguments
superbowl$new_brand<-as.factor(superbowl$new_brand)
superbowl$month<-as.factor(superbowl$month)
superbowl$superbowl<-as.factor(superbowl$superbowl)

# tell R, week_of is date of format "%d-%b-%y"
superbowl$week_of<- as.Date(superbowl$week_of,"%d-%b-%y")

# To explore the superbowl table,

dim(superbowl)
str(superbowl)
colnames(superbowl)
head(superbowl)
tail(superbowl)


#Line Plot on Adspend vs Positive Buzz

# Reshape the data to make line plot for both data metrics
superbowl_long <- superbowl %>%
  pivot_longer(cols = c(adspend, pos), names_to = "metric", values_to = "value")

# Line Plot
ggplot(superbowl_long_2, aes(x = week_of, y = value, color = new_brand, linetype = metric)) +
  geom_line() +
  labs(y = "Values", color = "Car Brand", linetype = "Metric") +
  theme_minimal()

