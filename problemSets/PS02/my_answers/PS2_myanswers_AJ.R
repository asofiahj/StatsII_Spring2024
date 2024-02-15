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

lapply(c("tidyverse"),  pkgTest)

# set wd for current folder
setwd("C://Users/Sofia Jesus//Desktop//PhD Political Science//Semester 2//Quantitative Methods II")
getwd()


# loaded data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))


#####################
# Question 1
#####################

# Check the class of the explanatory variables
class(climateSupport$countries)
class(climateSupport$sanctions)

# Convert "sanctions" variable into a factor
climateSupport$sanctions <- factor(climateSupport$sanctions)

# Convert "countries" variable into a factor
climateSupport$countries <- factor(climateSupport$countries)

# Transforming it into non-ordered factors
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)


# Fit logistic regression model
model1 <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial(link = "logit"))

# Print summary of the model
summary(model1)

# Fit null model (intercept-only model)
null_mod <- glm(choice ~ 1, data = climateSupport, family = binomial())

#  Run an anova test on the model compared to the null model 
anova(null_mod, model1, test = "Chisq")
anova(null_mod, model1, test = "LRT") # LRT is equivalent

########## 
# Question 2
##########
## a. 
# Identifying the coefficient: 
-0.32510

# Increasing sanctions from 5% to 15%, on average, decreases the log odds
# that an individual will support the policy by 0.33, for all number of participating
# countries. 

## b.

# Defining the conditions for the probability of interest
countries_condition <- "80 of 192"
sanctions_condition <- "None"

# Creating a data frame with the conditions
conditions_df <- data.frame(countries = factor(countries_condition, levels = levels(climateSupport$countries)),
                            sanctions = factor(sanctions_condition, levels = levels(climateSupport$sanctions)))

# Predicting the probability using model1
estimated_probability <- predict(model1, newdata = conditions_df, type = "response")

# Print the estimated probability
estimated_probability

## c.
# Fit a model with interaction term
model_interaction <- glm(choice ~ countries * sanctions, data = climateSupport, family = binomial(link = "logit"))

summary(model_interaction)
        
# Perform Likelihood Ratio Test
anova(model1, model_interaction, test = "Chisq")




