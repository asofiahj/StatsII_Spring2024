
rm(list=ls())

install.packages("stargazer")
install.packages("eha")
install.packages("survival")
library(stargazer)
library(eha)
library(survival)

# Load the child dataset from the eha library
data(child, package = "eha")

# Check the structure of the child dataset
summary(child)

cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)

# Summarize the Cox model results
summary(cox_model)

# Create table with stargazer
stargazer(cox_model, type = "latex")

