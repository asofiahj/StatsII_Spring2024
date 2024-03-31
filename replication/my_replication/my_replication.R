
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)


setwd("C:/Users/Sofia Jesus/Desktop/PhD Political Science/Semester 2/Quantitative Methods II/Welfare Regimes/Datasets")

dataround1_9 <- read.spss("ESS1-9e01_1_red.sav", to.data.frame = TRUE)
soc_exp_sub_full <- read.csv("C:/Users/Sofia Jesus/Desktop//PhD Political Science/Semester 2/Quantitative Methods II/Welfare Regimes/Datasets/DP_LIVE_12042022195739284.csv", sep = ",")

### subsetting the 18 relevant countries
soc_exp_sub_sub <- soc_exp_sub_full[soc_exp_sub_full[, 1] == "BEL" | soc_exp_sub_full[, 1] == "FRA" |
                                      soc_exp_sub_full[, 1] == "NLD" | soc_exp_sub_full[, 1] == "DEU" |
                                      soc_exp_sub_full[, 1] == "DNK" | soc_exp_sub_full[, 1] == "NOR" |
                                      soc_exp_sub_full[, 1] == "FIN" | soc_exp_sub_full[, 1] == "SWE" |
                                      soc_exp_sub_full[, 1] == "GBR" | soc_exp_sub_full[, 1] == "IRL" |
                                      soc_exp_sub_full[, 1] == "ESP" | soc_exp_sub_full[, 1] == "PRT" |
                                      soc_exp_sub_full[, 1] == "CZE" | soc_exp_sub_full[, 1] == "HUN" |
                                      soc_exp_sub_full[, 1] == "SVN" | soc_exp_sub_full[, 1] == "EST" |
                                      soc_exp_sub_full[, 1] == "POL",
                                    c(1, 6, 7)]


summary(dataround1_9$cntry)
str(dataround1_9$cntry)

unique(soc_exp_sub_sub$LOCATION)

# removing Switzerland
dataround1_9 <- dataround1_9[dataround1_9$cntry != "Switzerland", ]
dataround1_9 <- dataround1_9[, !grepl("Switzerland", names(dataround1_9))]


# Create a lookup table mapping country codes to country names
country_lookup_table <- data.frame(
  LOCATION = c("BEL", "CZE", "DEU", "DNK", "EST", "ESP", "FIN", "FRA", "GBR", "HUN", "IRL", "NLD", "NOR", "POL", "PRT", "SWE", "SVN"),
  CountryName = c("Belgium", "Czechia", "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", "United Kingdom", "Hungary", "Ireland", "Netherlands", "Norway", "Poland", "Portugal", "Sweden", "Slovenia")
)

# Merge the lookup table with soc_exp_sub_sub to replace codes with names
soc_exp_sub_sub_with_names <- merge(soc_exp_sub_sub, country_lookup_table, by.x = "LOCATION", by.y = "LOCATION", all.x = TRUE)

# Now you have the soc_exp_sub_sub dataset with country names
# You can summarize the unique country names
unique(soc_exp_sub_sub_with_names$CountryName)
summary(dataround1_9$cntry)

# Subset soc_exp_sub_sub_with_names to remove data for odd years
soc_exp_subset <- soc_exp_sub_sub_with_names[!(soc_exp_sub_sub_with_names$TIME %% 2 == 1 & soc_exp_sub_sub_with_names$TIME >= 2002 & soc_exp_sub_sub_with_names$TIME <= 2019), ]


# Create a mapping table between ESS rounds and time periods
ess_mapping <- data.frame(
  essround = c("9", "8", "7", "6", "5", "4", "3", "2", "1"),
  TIME = c(2018, 2016, 2014, 2012, 2010, 2008, 2006, 2004, 2002)
)

# Convert LOCATION to factor and set levels
soc_exp_subset$LOCATION <- factor(soc_exp_subset$LOCATION, levels = levels(dataround1_9$cntry))

# Check levels of factor variable in soc_exp_subset again
levels(soc_exp_subset$LOCATION)


######################## merging social spending values with ess rounds #########


# Create a vector of social spending values for each country and ESS round
social_spending_values <- c(
  # Belgium
  24.502, 25.444, 25.238, 26.466, 28.448, 28.804, 29.105, 28.845, 28.770, 
  # Czechia
  18.545, 17.842, 17.646, 17.719, 19.572, 19.827, 20.070, 18.888, 18.759,
  # Spain
  19.350, 20.214, 20.516, 22.326, 24.895, 25.545, 25.367, 24.236, 24.170, 
  # Estonia
  12.758, 13.306, 12.455, 15.121, 17.994, 15.636, 15.949, 17.505, 17.511, 
  # Finland
  23.192, 23.951, 23.740, 23.323, 27.360, 28.285, 30.121, 30.449, 29.275, 
  # France
  28.409, 28.862, 28.431, 28.520, 31.084, 31.211, 32.027, 31.847, 31.077, 
  # United Kingdom
  17.955, 19.323, 19.187, 21.052, 23.332, 23.194, 21.864, 20.845, 20.291, 
  # Hungary
  20.800, 21.086, 22.148, 22.659, 22.998, 22.604, 21.460, 20.296, 18.810, 
  # Ireland
  14.399, 15.151, 15.371, 19.200, 23.781, 22.566, 19.647, 15.069, 13.590, 
  # Netherlands
  19.878, 20.397, 16.434, 15.479, 17.467, 17.926, 17.932, 17.473, 16.221, 
  # Norway
  22.710, 22.283, 19.479, 19.273, 22.099, 21.504, 22.909, 26.027, 24.380, 
  # Poland
  21.854, 21.248, 20.625, 20.211, 20.746, 19.780, 20.176, 21.217, 20.601, 
  # Portugal
  20.294, 21.675, 22.020, 22.161, 24.521, 24.544, 25.076, 23.375, 22.521, 
  # Slovenia
  22.320, 21.551, 21.126, 19.880, 23.355, 23.473, 23.094, 22.291, 21.018, 
  # Sweden
  27.253, 27.458, 26.456, 25.409, 25.907, 26.330, 26.638, 26.590, 25.790,
  # Denmark
  24.804, 25.119, 24.961, 26.324, 29.637, 29.927, 29.877, 29.310, 28.684,
  # Germany
  26.163, 26.020, 25.106, 24.353, 26.046, 24.652, 24.773, 25.254, 25.342
)

# Create a vector of countries
countries <- c(
  rep("Belgium", 9),
  rep("Czechia", 9),
  rep("Spain", 9),
  rep("Estonia", 9),
  rep("Finland", 9),
  rep("France", 9),
  rep("United Kingdom", 9),
  rep("Hungary", 9),
  rep("Ireland", 9),
  rep("Netherlands", 9),
  rep("Norway", 9),
  rep("Poland", 9),
  rep("Portugal", 9),
  rep("Denmark", 9),
  rep("Germany", 9),
  rep("Slovenia", 9),
  rep("Sweden", 9)
)



# Create a vector of ESS rounds
ess_rounds <- rep(1:9, times = length(unique(countries)))

# Combine the vectors to create a data frame
social_spending_data <- data.frame(cntry = countries, essround = ess_rounds, socialspending = social_spending_values)

# Merge the data frames based on country and ESS round
dataround1_9 <- merge(dataround1_9, social_spending_data, by = c("cntry", "essround"), all.x = TRUE)

#####################################

# Merge categories and remove unwanted ones
dataround1_9 <- dataround1_9 %>%
  mutate(gincdif = recode(gincdif, 
                          "Agree strongly" = "Agree",
                          "Disagree strongly" = "Disagree",
                          "Neither agree nor disagree" = NA_character_,
                          "NA" = NA_character_)) %>%
  filter(!is.na(gincdif))


# Transform into a dummy variable
dataround1_9$gincdif_dummy <- ifelse(dataround1_9$gincdif == "Agree", 1, 0)

#Define a vector of years corresponding to each essround
year_values <- c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)

# Create a new variable "year" in dataround1_9 based on essround
dataround1_9$year <- year_values[dataround1_9$essround]

# Display the updated dataset
head(dataround1_9)




######## grouping 

# Define the country groups
conservative_countries <- c("Belgium", "Germany", "France", "Netherlands")
social_democratic_countries <- c("Denmark", "Finland", "Norway", "Sweden")
liberal_countries <- c("United Kingdom", "Ireland")
mediterranean_countries <- c("Spain", "Portugal")
post_communist_countries <- c("Hungary", "Poland", "Czechia", "Slovenia", "Estonia")

# Create a new variable 'regime' in the dataset based on the country groups
dataround1_9$regime <- NA  # Initialize the regime variable

# Assign regime labels based on country groups
dataround1_9$regime[dataround1_9$cntry %in% conservative_countries] <- "Conservative"
dataround1_9$regime[dataround1_9$cntry %in% social_democratic_countries] <- "Social_Democratic"
dataround1_9$regime[dataround1_9$cntry %in% liberal_countries] <- "Liberal"
dataround1_9$regime[dataround1_9$cntry %in% mediterranean_countries] <- "Mediterranean"
dataround1_9$regime[dataround1_9$cntry %in% post_communist_countries] <- "Post_Communist"

# Convert 'Regime' variable into dummy variables
dataround1_9 <- cbind(dataround1_9, model.matrix(~ regime - 1, data = dataround1_9))

summary(dataround1_9$gincdif_dummy)

# Compute correlation matrix
correlation_matrix <- cor(dataround1_9[, c("regimeConservative", "regimeLiberal", "regimeMediterranean", "regimePost_Communist", "regimeSocial_Democratic")])

print(correlation_matrix)

# Check for missing values 
sum(is.na(dataround1_9$gincdif))
sum(is.na(dataround1_9$socialspending))


##################################
# Regressions
#################################
library(stargazer)

regime_logit <- glm(gincdif_dummy ~ regime, 
                   family = binomial(link = "logit"), 
                   data = dataround1_9)
summary(regime_logit)
stargazer(regime_logit)


country_logit <- glm(gincdif_dummy ~ cntry, 
                   family = binomial(link = "logit"), 
                   data = dataround1_9)
summary(country_logit)


stargazer(regime_logit, type = "latex")
stargazer(country_logit, type = "latex")


fe_logit <- glm(gincdif_dummy ~ regime + as.factor(socialspending),
              family = binomial(link = "logit"), 
                   data = dataround1_9)
summary(fe_logit)

lm_model <- lm(gincdif_dummy ~ regime + year + socialspending, data = dataround1_9)
summary(lm_model)

stargazer(lm_model, type = "latex")


#### Chi-square test

# Prepare contingency table
contingency_table <- table(dataround1_9$regime, dataround1_9$gincdif_dummy)

# Perform chi-square test
chi_sq_result <- chisq.test(contingency_table)

print(chi_sq_result)

# Pairwise chi-square tests between welfare regimes
pairwise_chi_sq <- pairwise.prop.test(contingency_table, correct = FALSE)$p.value

print(pairwise_chi_sq)

######### Comparisons within regime groups
summary (dataround1_9)

# Calculate standard deviation for each regime
sd_by_regime <- aggregate(gincdif_dummy ~ regime, data = dataround1_9, FUN = sd)

# Print the standard deviation for each regime
print(sd_by_regime)

#########################
# new plots
########################
library(ggplot2)


ggplot(dataround1_9, aes(x = gincdif_dummy, y = socialspending, color = regime)) +
  geom_point() +
  labs(x = "Support for Redistribution", y = "Social Spending") +
  ggtitle("Relationship between Social Spending, Regime, and gincdif_dummy")



ggplot(dataround1_9, aes(x = regime, y = socialspending)) +
  geom_boxplot() +
  labs(x = "Regime", y = "Social Spending") +
  ggtitle("Distribution of Social Spending Across Different Regimes")

ggplot(dataround1_9, aes(x = socialspending, y = gincdif_dummy)) +
  geom_point() +
  labs(x = "Social Spending", y = "gincdif_dummy") +
  ggtitle("Relationship between Social Spending and gincdif_dummy")
