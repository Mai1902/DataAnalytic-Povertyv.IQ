# YourName: Mai Nguyen Dac

# Add your code below this line.

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(dplyr)

# Read the data from csv file
dat <- read_csv("IQpoverty.csv", locale = locale(encoding = "latin1"))
View(dat)

#Tidy the data:
dat$`Income ($)` = suppressWarnings(as.numeric(gsub("[\\ $]", "", dat$`Income ($)`)))
#Use cor to find correlation in orginal dat
library(psych)
cor(log1p(dat$IQ), log1p(dat$`Income ( )`), use = "complete.obs")
cor(log1p(dat$IQ), log1p(dat$`Education expenditures per capita ( )`), use = "complete.obs")
cor(log1p(dat$IQ), log1p(dat$`Daily max temperature(Celcius Degree)`), use = "complete.obs")

# Examine the relationship between IQ and GDP by using graph (both log and normal)

ggplot(data = dat) + geom_point(mapping = aes(x = `Income ( )`, y = IQ))  + geom_smooth(mapping = aes(x = dat$`Income ( )`, y = dat$IQ))

ggplot(data = dat, aes(x = log1p(dat$`Income ( )`), y=log1p(dat$IQ))) +
    geom_point() + geom_smooth() 

# Examine the relationship between IQ and GDP using normal linear regression and logarithm linear regression:
lm.model = lm(dat$IQ ~ dat$`Income ( )`, data = dat)
summary(lm.model)

lm_log.model = lm(log1p(dat$IQ) ~ log1p(dat$`Income ( )`), data = dat)
summary(lm_log.model)

# Filter the outliers

outlier <- filter(dat, dat$`Income ( )` < 1500, dat$IQ >= 94)
view(outlier)

# Examine the relationship by using linear model


#(Did you remember to add your name to this script?)