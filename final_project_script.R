## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")


##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(data$length)
sd(data$length)
min(data$length)
max(data$length)

mean(data$plays)
sd(data$plays)
min(data$plays)
max(data$plays)

table(data$album)

table(data$release_year)

table(data$enjoyability)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
lm(plays ~ album, data = data)
aov(plays ~ album, data = data)
summary(plays ~ album, data = data)
boxplot(plays ~ album, data = data)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################

linear_plot <- plot(data$length, data$plays)
print(linear_plot)
meany <- mean(data$plays)
meanx <- mean(data$length)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")
linear_relationship <- lm(plays ~ length, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(data$length, residuals(linear_relationship))

abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################

table(data$enjoyability, data$release_year)

chisq.test(table(data$enjoyability, data$release_year))
