##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                       I. Introduction                       ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Group L Second Project
## Emirhan Yücel 2020302264
## Mübin Salih Sarıçiçek 2020302243
## Sayhan Yalvaçer 2019202063
#---------------------------------------------------------------#
## Dataset: https://github.com/sayhany/POLS203/raw/main/final/pols_203_final_merged.csv
## Alternative link: https://www.dropbox.com/s/e67eyagnrgepzg3/pols_203_final_merged.csv
## Our GitHub repository: https://github.com/sayhany/POLS203
## For version control: https://github.com/sayhany/POLS203/commits/main/final/Group_L_final.R
#---------------------------------------------------------------#
## All data is retrieved from "Our World in Data"
## Since the website does not allow bulk downloading, we merged multiple
## files. You can examine the R script used for that purpose from:
## https://github.com/sayhany/POLS203/blob/main/final/merge_datasets.R
#---------------------------------------------------------------#
#   Research question: What is the relationship between EU
# membership and GDP per capita growth during the period
# 2004 - 2014
#   Strategy: We first subsetted the former Eastern Bloc
# countries and divided them into two groups according to whether
# they did join the EU in 2004, as many of them did.
#             We omitted some countries either because they
# joined the union later or they underwent some major economic
# crises during the specified period. The we proceeded to compare
# their mean values of growth and applied a t-test in order to
# test the hypothesis and calculate a confidence interval.
#             In the second part of the project, we extended
# our sample to all European countries and added many new
# independent variables. We built several models with differing
# numbers of IVs, then, we described their diagnostics and 
# compared them according to their Akaike information to
# ultimately decide which model we are going to choose. The most
# important question to answer was whether the best model will 
# include EU membership as an IV.
##...............................................................
##                      Start of the code                       .
##...............................................................
# Load the required packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes) # Themes for ggplot2

if (!require(MASS)) install.packages('MASS')
library(MASS) # For stepAIC

if (!require(simputation)) install.packages('simputation')
library(simputation) # Simple linear imputation

if (!require(missForest)) install.packages('missForest')
library(missForest) # Imputation by using random forest algorithm

if (!require(car)) install.packages('car')
library(car) # For vif

if (!require(corrplot)) install.packages('corrplot')
library(corrplot) # For plotting correlation matrices

if (!require(plot3D)) install.packages('plot3D')
library(plot3D) # Three-dimensional plots

# Disable scientific notation
options(scipen = 999)

# Set seed
set.seed(203) # For the reproducibility of random forest results

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                    II. Data manipulation                    ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read the dataset using readr
pols_203_final_merged <- read_csv("pols_203_final_merged.csv")

# Explore the structure of the dataset
str(pols_203_final_merged)
dim(pols_203_final_merged) # 121078 rows and 17 columns
colnames(pols_203_final_merged) # Names of the columns

## Classes of the columns
class(pols_203_final_merged$id) # Character
class(pols_203_final_merged$Entity) # Character
class(pols_203_final_merged$Code) # Character
class(pols_203_final_merged$`Total dependency ratio - Sex: all - Age: none - Variant: estimates`) # Numeric
class(pols_203_final_merged$output_quantity) # Numeric
class(pols_203_final_merged$`Government expenditure on tertiary education as % of GDP (%)`) # Numeric
class(pols_203_final_merged$`International tourism, number of arrivals`) # Numeric
class(pols_203_final_merged$`Top marginal income tax rate (Reynolds (2008))`) # Numeric
class(pols_203_final_merged$`Oil production per capita (kWh)`) # Numeric
class(pols_203_final_merged$particip_vdem_owid) # Numeric
class(pols_203_final_merged$`Per capita electricity (kWh)`) # Numeric
class(pols_203_final_merged$`GDP per capita (output, multiple price benchmarks)`) # Numeric
class(pols_203_final_merged$`Time required to start a business (days)`) # Numeric
class(pols_203_final_merged$Population) # Numeric

## Mean values of the columns
mean(pols_203_final_merged$`Total dependency ratio - Sex: all - Age: none - Variant: estimates`,
     na.rm = TRUE) # Mean value: 70.97669
mean(pols_203_final_merged$output_quantity,
     na.rm = TRUE) # Mean value: 65420243991
mean(pols_203_final_merged$`Government expenditure on tertiary education as % of GDP (%)`,
     na.rm = TRUE) # Mean value: 0.967423
mean(pols_203_final_merged$`International tourism, number of arrivals`,
     na.rm = TRUE) # Mean value: 40150409
mean(pols_203_final_merged$`Top marginal income tax rate (Reynolds (2008))`,
     na.rm = TRUE) # Mean value: 49.35
mean(pols_203_final_merged$`Oil production per capita (kWh)`,
     na.rm = TRUE) # Mean value: 25668.78
mean(pols_203_final_merged$particip_vdem_owid,
     na.rm = TRUE) # Mean value: 0.2603911
mean(pols_203_final_merged$`Per capita electricity (kWh)`,
     na.rm = TRUE) # Mean value: 3834.949
mean(pols_203_final_merged$`GDP per capita (output, multiple price benchmarks)`,
     na.rm = TRUE) # Mean value: 14101.82
mean(pols_203_final_merged$`Time required to start a business (days)`,
     na.rm = TRUE) # Mean value: 32.9653
mean(pols_203_final_merged$Population,
     na.rm = TRUE) # Mean value: 126470437

## Standard deviations of the numeric columns
sd(pols_203_final_merged$`Total dependency ratio - Sex: all - Age: none - Variant: estimates`,
     na.rm = TRUE) # sd = 20.20166
sd(pols_203_final_merged$output_quantity,
     na.rm = TRUE) # sd = 258007206831
sd(pols_203_final_merged$`Government expenditure on tertiary education as % of GDP (%)`,
     na.rm = TRUE) # sd = 0.5594122
sd(pols_203_final_merged$`International tourism, number of arrivals`,
     na.rm = TRUE) # sd = 174571434
sd(pols_203_final_merged$`Top marginal income tax rate (Reynolds (2008))`,
     na.rm = TRUE) # sd = 16.45265
sd(pols_203_final_merged$`Oil production per capita (kWh)`,
     na.rm = TRUE) # sd = 167457.5
sd(pols_203_final_merged$particip_vdem_owid,
     na.rm = TRUE) # sd = 0.2098426
sd(pols_203_final_merged$`Per capita electricity (kWh)`,
     na.rm = TRUE) # sd = 4952.337
sd(pols_203_final_merged$`GDP per capita (output, multiple price benchmarks)`,
     na.rm = TRUE) # sd = 23746.75
sd(pols_203_final_merged$`Time required to start a business (days)`,
     na.rm = TRUE) # sd = 45.96973
sd(pols_203_final_merged$Population,
     na.rm = TRUE) # sd = 588851231

# Filter 2004 and 2014
pols_203_final_merged_2004_2014 <- pols_203_final_merged %>%
  filter(Year == 2004 | Year == 2014) # Year must be 2004 or 2014

# Create the vector of countries to be studied
country_vector <- c("Russia",
                    "Germany",
                    "United Kingdom",
                    "France",
                    "Italy",
                    "Spain",
                    "Poland",
                    "Netherlands",
                    "Belgium",
                    "Czech Republic",
                    "Greece",
                    "Portugal",
                    "Sweden",
                    "Hungary",
                    "Belarus",
                    "Austria",
                    "Serbia",
                    "Switzerland",
                    "Denmark",
                    "Finland",
                    "Slovakia",
                    "Norway",
                    "Ireland",
                    "Croatia",
                    "Moldova",
                    "Armenia",
                    "Lithuania",
                    "North Macedonia",
                    "Slovenia",
                    "Latvia",
                    "Estonia",
                    "Montenegro",
                    "Luxebourg",
                    "Malta",
                    "Iceland",
                    "Azerbaijan")

# Alphabetically rearrange
country_vector <- sort(country_vector) # Sort in ascending order

# Filter for those countries
pols_203_final_merged_2004_2014 <- pols_203_final_merged_2004_2014 %>%
  filter(Entity %in% country_vector) # Is the country in our country list?

# Move values to a single column to prepare the pols_203_joined for wrangling
## Spot the first appearance of each variable
match(unique(pols_203_final_merged_2004_2014$id), pols_203_final_merged_2004_2014$id) # Find the indices

## Move the columns
pols_203_final_merged_2004_2014$value <-c(pols_203_final_merged_2004_2014$`Total dependency ratio - Sex: all - Age: none - Variant: estimates`[1:68],
                                          pols_203_final_merged_2004_2014$output_quantity[69:134],
                                          pols_203_final_merged_2004_2014$`Government expenditure on tertiary education as % of GDP (%)`[135:165],
                                          pols_203_final_merged_2004_2014$`International tourism, number of arrivals`[166:229],
                                          pols_203_final_merged_2004_2014$`Oil production per capita (kWh)`[230:296],
                                          pols_203_final_merged_2004_2014$particip_vdem_owid[297:364],
                                          pols_203_final_merged_2004_2014$`Per capita electricity (kWh)`[365:431],
                                          pols_203_final_merged_2004_2014$`GDP per capita (output, multiple price benchmarks)`[432:499],
                                          pols_203_final_merged_2004_2014$`Time required to start a business (days)`[500:564],
                                          pols_203_final_merged_2004_2014$Population[565:632])
# Data wrangling
pols_203_final_merged_2004_2014 <- pols_203_final_merged_2004_2014 %>%
  dplyr::select("id",
         "Entity",
         "Year",
         "value") %>%
  pivot_wider(names_from = "id", # "Tame" the data
              values_from = "value")

# Split the dataframe into two by year
## Filter for 2004
pols_203_final_merged_2004 <- pols_203_final_merged_2004_2014 %>% 
  filter(Year == 2004) # Year must be equal to 2004

## Filter for 2014
pols_203_final_merged_2014 <- pols_203_final_merged_2004_2014 %>%
  filter(Year == 2014) # Year must be equal to 2014

pols_203_joined <- inner_join(pols_203_final_merged_2004,
                     pols_203_final_merged_2014,
                     by = c("Entity"),
                     suffix = c("_2004",
                                "_2014"))

# Remove the redundant "Year" column
pols_203_joined <- pols_203_joined %>%
  dplyr::select(!starts_with("Year")) # We added "dplyr::" before the function
                                      # because the package "MASS" masks
                                      # "select" function from dplyr

# Rename the "Entities" column
names(pols_203_joined)[names(pols_203_joined) == "Entity"] <- "country"

# Examine
View(pols_203_joined)

# Create a vector that lists all the EU countries
eu <- c("Austria", 
        "Belgium",
        "Bulgaria",
        "Croatia",
        "Cyprus",
        "Czech Republic",
        "Denmark",
        "Estonia",
        "Finland",
        "France",
        "Germany",
        "Greece",
        "Hungary",
        "Ireland",
        "Italy",
        "Latvia",
        "Lithuania",
        "Luxembourg",
        "Malta",
        "Netherlands",
        "Poland",
        "Portugal",
        "Romania",
        "Slovakia",
        "Slovenia",
        "Spain",
        "Sweden",
        "United Kingdom") # In 2014 the UK was a EU member 

# Whether they are members of  the EU
pols_203_joined <- pols_203_joined %>%
  mutate(eu = case_when(country %in% eu ~ TRUE, # Add TRUE if the country is in
                                                # the vector
                        !(country %in% eu) ~ FALSE )) # Add FALSE if the country
                                                      # is NOT in the vector

# Summary
summary(pols_203_joined)
# Discussion: Some columns have N/As
# We decided to remove the "gov_exp_tertiary_ed_vs_GDP" columns since they have 
# too many missing values
# However, we will fill the few other values that are missing just for 2004 
# by using imputation (from the Chapter 16 of the textbook, Statistical Methods 
#                      for the Social Science by Alan Agresti)
# Because, if we strictly avoid all the data that contains some missing values
# we will not be able to consider various IVs and if we delete the country rows
# that have missing values, our model will be rather biased since the countries
# we have complete data for are almost always developed countries

# Remove "gov_exp_tertiary_ed_vs_GDP_2004" and "gov_exp_tertiary_ed_vs_GDP_2014"
pols_203_joined <- pols_203_joined %>%
  dplyr::select(!c(gov_exp_tertiary_ed_vs_GDP_2004, # We added "dplyr::" before the function
            gov_exp_tertiary_ed_vs_GDP_2014))       # because the package "MASS" masks
                                                    # "select" function from dplyr
  
# Imputation by linear regression
## tourists_2004
plot(pols_203_joined$tourists_2014, pols_203_joined$tourists_2004) # The relationship is linear

### Imputation by linear regression is justified
pols_203_joined <- impute_lm(pols_203_joined,
                             tourists_2004 ~ tourists_2014)

## agricultural_output_2004
plot(pols_203_joined$agricultural_output_2004, pols_203_joined$agricultural_output_2014) # The relationship is non-linear

### Imputation by linear regression is not justified

## oil_production_per_cap_2004
plot(pols_203_joined$oil_production_per_cap_2014, pols_203_joined$oil_production_per_cap_2004) # The relationship is non-linear

### Imputation by linear regression is not justified

## electricity_per_cap_2004
plot(pols_203_joined$electricity_per_cap_2014, pols_203_joined$electricity_per_cap_2004) # The relationship is non-linear

### Imputation by linear regression is not justified

## time_req_to_start_business_2004
plot(pols_203_joined$time_req_to_start_business_2014, pols_203_joined$time_req_to_start_business_2004) # The is not much relationship

### Imputation by linear regression is not justified

# We decided to fill the remaining missing values by using a non-parametric
# algorithm called "randomForest"
## Prepare the dataset for imputation
pols_203_joined_4_imp <- dplyr::select(pols_203_joined, -c("country", "eu"))

### Convert it to an ordinary dataframe
pols_203_joined_4_imp <- as.data.frame(pols_203_joined_4_imp)

### Run the algorithm
forest <- missForest(pols_203_joined_4_imp)

### Convert it back to a tibble
forest_tibble <- as_tibble(forest$ximp)

### Add the "country" and "eu" columns
forest_tibble <- bind_cols(pols_203_joined$country, forest_tibble, pols_203_joined$eu)

#### Repair column names
colnames(forest_tibble)[1] <- "country"
colnames(forest_tibble)[20] <- "eu"

# Summary
summary(forest_tibble)

forest_tibble <- forest_tibble %>% 
  mutate(agricultural_output_per_cap_2004 = agricultural_output_2004 / population_2004, # Calculate the per capita value
         agricultural_output_per_cap_2014 = agricultural_output_2014 / population_2014, # Calculate the per capita value
         tourists_per_cap_2004 = tourists_2004 / population_2004, # Calculate the per capita value
         tourists_per_cap_2014 = tourists_2014 / population_2014, # Calculate the per capita value
         total_dependency_ratio_mean = (total_dependency_ratio_2004 + 
                                        total_dependency_ratio_2014) / 2, # Calculate the mean
         oil_production_per_cap_mean = (oil_production_per_cap_2004 +
                                        oil_production_per_cap_2014) / 2, # Calculate the mean
         democracy_mean = (democracy_2004 +
                                democracy_2014) / 2, # Calculate the mean
         electricity_per_cap_mean = (electricity_per_cap_2004 +
                                       electricity_per_cap_2014) / 2, # Calculate the mean
         real_GDP_per_cap_mean = (real_GDP_per_cap_2004 +
                                  real_GDP_per_cap_2014) / 2, # Calculate the mean
         time_req_to_start_business_mean = (time_req_to_start_business_2004 +
                                            time_req_to_start_business_2014) / 2) %>% # Calculate the mean
  mutate(tourists_per_cap_mean = (tourists_per_cap_2004 +
                                  tourists_per_cap_2014) / 2) %>% # Calculate the mean
  mutate(growth = (real_GDP_per_cap_2014 - real_GDP_per_cap_2004) / real_GDP_per_cap_2004) %>% # Calculate the growth

dplyr::select(-c("agricultural_output_2004", # Delete the unnecessary columns
                 "agricultural_output_2014",
                 "tourists_2004",
                 "tourists_2014")) %>%
relocate(eu, .after = growth) # Move eu to the end

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                III. First part of the project                ::
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# In our first project we tried to answer the question whether former Eastern 
# Bloc countries that joined the EU in 2004 enjoyed higher GDP PPP per capita
# growth rates? We will briefly replicate what we did back then as the first
# part of this project. However, this time we will use the data retrieved
# from Our World in Data website instead of World Bank DataBank.

#----------------------------------------------------------------#

# Treatment: Joining the EU in 2004 (nominal)
# Dependent variable: GDP PPP per capita growth (continuous numerical)

#----------------------------------------------------------------#

# Alternative hypothesis: Mean GDP PPP per capita growth between 2004 - 2014 for
# the former Eastern Bloc countries that joined the EU is lower than the mean 
# GDP PPP per capita growth for the former Eastern Bloc countries that did not
# join the union

# Null-hypothesis: It is equal or greater

#----------------------------------------------------------------#

# Strategy: Compare them and apply a t-test

##................................................................
##                      Start of the code                        .
##................................................................

# Create a vector of the countries that we compared in the first project
eastern_bloc_old <- c("Poland",
                      "Czechia",
                      "Estonia",
                      "Hungary",
                      "Latvia",
                      "Lithuania",
                      "Slovakia",
                      "Slovenia",
                      "Belarus",
                      "Russia",
                      "Moldova",
                      "Armenia")
                  

# Filter them and save as another dataframe
eastern_bloc.df <- forest_tibble[forest_tibble$country %in% eastern_bloc_old, ]

# Summarize
eastern_bloc.df %>%
  group_by(eu) %>%
  summarize(mean_growth_per_cent = mean(growth) * 100)

# Plot
ggplot(eastern_bloc.df,
       aes(x = fct_reorder(country, growth),
           y = growth * 100, fill = eu)) +
  geom_col() +
  xlab("Countries") +
  ylab("GDP per capita, real GDP growth per cent 2004-2014") +
  coord_flip() +
  theme_economist() +
  theme(axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15))) +
  scale_fill_discrete("Did join the EU?") +
  geom_hline(yintercept = mean(eastern_bloc.df$growth * 100),
             color = "grey40",
             linetype = 3) +
  annotate(
    "text",
    x = 4, y = 80,
    label = "The\nmean\ngrowth",
    vjust = 1, size = 2.5, color = "grey40") +
  annotate(
    "curve",
    x = 4.1, y = 80,
    xend = 4.6, yend = 71.5,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = "grey40") +
  labs(caption = "Source: Our World in Data")

# Significance test
t_test <- t.test(eastern_bloc.df$growth[!eastern_bloc.df$eu],
                 eastern_bloc.df$growth[eastern_bloc.df$eu],
                 paired = FALSE,
                 conf.level = 0.95)

## Is it statistically significant?
t_test$p.value < 0.05

## t-test
t_test

### t = 4.0324
### DoF = 8.3914
### p-value = 0.003421
### Confidence interval (95%): (0.2001814, 0.7249948)

##  Findings: Mean growth rate of the former Eastern Bloc countries that
## joined the EU is significantly higher for the specified period (2004-2014)
##  In the second part of the project, we will attempt to answer the question
## whether we can explain GDP PPP per capita growth by EU membership

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                IV. Second part of the project                ::
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

##................................................................
##                      Start of the code                        .
##................................................................

##----------------------------------------------------------------
##                    Descriptive statistics                    --
##----------------------------------------------------------------
# Boxplots
boxplot(forest_tibble$total_dependency_ratio_mean)
boxplot(forest_tibble$total_dependency_ratio_mean)
boxplot(forest_tibble$oil_production_per_cap_mean)
boxplot(forest_tibble$democracy_mean)
boxplot(forest_tibble$electricity_per_cap_mean)$stats
boxplot(forest_tibble$time_req_to_start_business_mean)
boxplot(forest_tibble$tourists_per_cap_mean)

## We have several outliers. However we decided not to remove them yet
## since we want to avoid overfitting at all costs

##----------------------------------------------------------------
##                        Build a model                         --
##----------------------------------------------------------------

# Correlation matrix
cor_matrix <- cor(forest_tibble[, 2:19])

## Plot the correlation matrix
corrplot(cor_matrix,
         tl.cex = 0.5)

## All possible pairs
pairs(forest_tibble[, 2:19])

# The original model
m0 <- lm(growth ~ total_dependency_ratio_mean +
          oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           electricity_per_cap_mean +
           time_req_to_start_business_mean +
           tourists_per_cap_mean +
           real_GDP_per_cap_2004 +
           eu, data = forest_tibble)

## Diagnostics
summary(m0)

### RSE = 0.5604 on 25 DoF
### Multiple R^2 = 0.5622
### Adjusted R^2 = 0.422 
### F-statistic = 4.012 on 8 and 25 DoF
### p-value = 0.003519 < 0.05


## Variance inflation factor
vif(m0) # There is moderate (VIF < 5) correlation between the IVs

# Use "Akaike information criterion" for model selection
aic <- stepAIC(m0)
aic$anova # View the steps

## Description of the steps
### Remove total_dependency_ratio_mean
m1 <- lm(growth ~ oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           electricity_per_cap_mean +
           time_req_to_start_business_mean +
           tourists_per_cap_mean +
           real_GDP_per_cap_2004 +
           eu, data = forest_tibble)

#### Diagnostics
summary(m1)

### RSE = 0.5508 on 26 DoF
### Multiple R^2 = 0.5601
### Adjusted R^2 = 0.4417 
### F-statistic = 4.73 on 7 and 26 DoF
### p-value = 0.001565 < 0.05

#### Variance inflation factor

vif(m1) # There is moderate (VIF < 5) correlation between the IVs

### Remove tourists_per_cap_mean
m2 <- lm(growth ~ oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           electricity_per_cap_mean +
           time_req_to_start_business_mean +
           real_GDP_per_cap_2004 +
           eu, data = forest_tibble)

#### Diagnostics
summary(m2)

### RSE = 0.5421 on 27 DoF
### Multiple R^2 = 0.5577
### Adjusted R^2 = 0.4594 
### F-statistic = 5.673 on 6 and 27 DoF
### p-value = 0.0006433 < 0.05

#### Variance inflation factor
vif(m2) # There is moderate (VIF < 5) correlation between the IVs

### Remove electricity_per_cap_mean
m3 <- lm(growth ~ oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           time_req_to_start_business_mean +
           real_GDP_per_cap_2004 +
           eu, data = forest_tibble)

#### Diagnostics
summary(m3)

### RSE = 0.5351 on 28 DoF
### Multiple R^2 = 0.553
### Adjusted R^2 = 0.4731 
### F-statistic = 6.927 on 5 and 28 DoF
### p-value = 0.0002538 < 0.05

#### Variance inflation factor
vif(m3) # There is moderate (VIF < 5) correlation between the IVs

### Remove eu
m4 <- lm(growth ~ oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           time_req_to_start_business_mean +
           real_GDP_per_cap_2004,
         data = forest_tibble)

#### Diagnostics
summary(m4)

### RSE = 0.531 on 29 DoF
### Multiple R^2 = 0.5441
### Adjusted R^2 = 0.4812 
### F-statistic = 8.651 on 4 and 29 DoF
### p-value = 0.0001007 < 0.05

#### Variance inflation factor
vif(m4) # There is moderate (VIF < 5) correlation between the IVs

### Apparently, we can explain the growth without using the EU membership data
### This finding refutes what we did in the first project

# Remove time_req_to_start_business_mean
m5 <- lm(growth ~ oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           real_GDP_per_cap_2004,
         data = forest_tibble)

## Diagnostics
summary(m5)

### RSE = 0.526 on 30 DoF
### Multiple R^2 = 0.5371
### Adjusted R^2 = 0.4909 
### F-statistic = 11.61 on 3 and 30 DoF
### p-value = 0.00003229 < 0.05

#### Variance inflation factor
vif(m5) # There is moderate (VIF < 5) correlation between the IVs

# Remove oil_production_per_cap_mean
m6 <- lm(growth ~ democracy_mean +
           real_GDP_per_cap_2004,
         data = forest_tibble)

## Diagnostics
summary(m6)

### RSE = 0.5323 on 31 DoF
### Multiple R^2 = 0.5102
### Adjusted R^2 = 0.4786 
### F-statistic = 16.14 on 2 and 31 DoF
### p-value = 0.00001569 < 0.05

### Variance inflation factor
vif(type= "predictor", # VIF = 1.387538
    m6) # Multicollinearity does not exist (VIF = 1.387538 < 1.5 < 5)

## Although the model with real_GDP_per_cap_2004 has more explanatory power,
## its statistical significance is low. Perhaps, real_GDP_per_cap_2004 and
## democracy_mean interact. We will try one last model.
m7 <- lm(growth ~ democracy_mean *
           real_GDP_per_cap_2004,
         data = forest_tibble)

### Diagnostics
summary(m7)

### RSE = 0.421 on 30 DoF
### Multiple R^2 = 0.7035
### Adjusted R^2 = 0.6738 
### F-statistic = 23.73 on 3 and 30 DoF
### p-value = 0.0000000458 < 0.05

### Plot the diagnostics
plot(m7)

## Q-Q plot
qqnorm(m7$residuals) + qqline(m7$residuals)

## Shapiro-Wilk test
shapiro.test(m7$residuals)$p.value < 0.05 # Null-hypothesis: distribution is normal
## p-value = 0.0003711
## Normality of residuals is rejected

## Perhaps we need to remove some variables
forest_tibble_2 <- forest_tibble[-c(3, 4, 33),] # Azerbaijan, Belarus, Switzerland are removed 


## Now call the linear model again
m8 <- lm(growth ~ democracy_mean *
           real_GDP_per_cap_2004,
         data = forest_tibble_2)

### Diagnostics
summary(m8)

### RSE = 0.2017 on 27 DoF
### Multiple R^2 = 0.6462
### Adjusted R^2 = 0.6069 
### F-statistic = 16.44 on 3 and 27 DoF
### p-value = 0.000002819 < 0.05

### Plot the diagnostics
plot(m8)

## Q-Q plot
qqnorm(m8$residuals) + qqline(m8$residuals) # Seems to be normally distributed

## Shapiro-Wilk test
shapiro.test(m8$residuals)$p.value < 0.05 # p > 0.05
                                          # We cannot reject the null-hypothesis

### We can say that the residuals are normally distributed unlike the previous
### model

### y = (-2.57155696) * democracy_mean + (-0.00005586) * real_GDP_per_cap_2004 + (0.00006662) * [democracy_mean * real_GDP_per_cap_2004] + 2.36268371

# Plot the final model
## 3-dimensional plot
scatter3D(forest_tibble_2$real_GDP_per_cap_2004,
          forest_tibble_2$democracy_mean,
          forest_tibble_2$growth,
          labels = c("GDP per cap 2004",
                     "Democracy score",
                     "Growth 2004-2014"))

## 2-dimensional plot
ggplot(forest_tibble_2,
       aes(x = real_GDP_per_cap_2004,
           y = growth,
           color = democracy_mean)) +
  scale_y_log10() +
  geom_point(size = 4.2) +
  scale_color_viridis_c(option = "inferno",
                        alpha = 0.8,
                        name = "Democracy score") +
  xlab("Real GDP per capita in 2004") +
  ylab("Real GDP per capita growth") +
  ggtitle("Our final linear model") +
  labs(caption = "Source: Our World in Data")

##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##                        V. Conclusion                        ::
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
