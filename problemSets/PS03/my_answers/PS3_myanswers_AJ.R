#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)


#####################
# Question 1
#####################
########1.###########
# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

length(gdp_data)
head(gdp_data)
str(gdp_data)

# Assigning categories based on GDPWdiff values
gdp_data$gdp_category <- ifelse(gdp_data$GDPWdiff == 0, "no change", 
                                ifelse(gdp_data$GDPWdiff > 0, "positive", "negative"))


# Setting "no change" as the reference category
gdp_data$gdp_category <- relevel(factor(gdp_data$gdp_category), ref = "no change")

# Fit unordered multinomial logistic regression
model_logit_unordered <- multinom(gdp_category ~ REG + OIL, data = gdp_data)

# Print summary of the model
summary(model_logit_unordered)


########2.###########
library(MASS)
# Defining the levels of GDPWdiff in the correct order
gdp_data$gdp_category <- factor(gdp_data$gdp_category, levels = c("negative", "no change", "positive"))

# Fitting the ordered multinomial logit model
model_ordered <- polr(gdp_category ~ REG + OIL, data = gdp_data, Hess = TRUE)

# Printing the summary of the model
summary(model_ordered)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

length(mexico_elections)
head(mexico_elections)
str(mexico_elections)

# Loading required libraries
library(stats)

# Running Poisson regression
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                     data = mexico_elections, 
                     family = poisson)

# Displaying summary of the model
summary(poisson_model)

## Calculations for last question 2)c.
-3.81023 + (-0.08135*1) + (-2.08014*0) + (-0.31158*1)
-3.81023 - 0.08135 - 0.31158
-4.20316
exp(-4.20316)
