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
#---------------------------------------------------------------#
## All data is retrieved from "Our World in Data"
## Since the website does not allow bulk downloading, we merged different
## files. You can examine the R script used for that purpose from:
## https://github.com/sayhany/POLS203/blob/main/final/merge_datasets.R


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
# Read the dataset
pols_203_final_merged <- read_csv("pols_203_final_merged.csv")

# Examine the structure of the dataset
str(pols_203_final_merged)

# Filter 2004 and 2014
pols_203_final_merged_2004_2014 <- pols_203_final_merged %>%
  filter(Year == 2004 | Year == 2014)

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
country_vector <- sort(country_vector)

# Filter for those countries
pols_203_final_merged_2004_2014 <- pols_203_final_merged_2004_2014 %>%
  filter(Entity %in% country_vector)

# Move values to a single column to prepare the pols_203_joined for wrangling
## Spot the first appearance of each variable
match(unique(pols_203_final_merged_2004_2014$id), pols_203_final_merged_2004_2014$id)

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
  pivot_wider(names_from = "id",
              values_from = "value")

# Split the dataframe into two by year
## Filter for 2004
pols_203_final_merged_2004 <- pols_203_final_merged_2004_2014 %>% 
  filter(Year == 2004)

## Filter for 2014
pols_203_final_merged_2014 <- pols_203_final_merged_2004_2014 %>%
  filter(Year == 2014)

pols_203_joined <- inner_join(pols_203_final_merged_2004,
                     pols_203_final_merged_2014,
                     by = c("Entity"),
                     suffix = c("_2004",
                                "_2014"))

# Remove the redundant "Year" column
pols_203_joined <- pols_203_joined %>% dplyr::select(!starts_with("Year"))

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
  mutate(eu = case_when(country %in% eu ~ TRUE,
                        !(country %in% eu) ~ FALSE ))

# Summary
summary(pols_203_joined)
# Discussion: Some columns have N/As
# We decided to remove the "gov_exp_tertiary_ed_vs_GDP" columns since they have 
# many missing values
# However, we will fill the few other values that are missing just for 2004 
# by using imputation (from the Chapter 16 of the textbook)

# Remove "gov_exp_tertiary_ed_vs_GDP_2004" and "gov_exp_tertiary_ed_vs_GDP_2014"
pols_203_joined <- pols_203_joined %>%
  dplyr::select(!c(gov_exp_tertiary_ed_vs_GDP_2004,
            gov_exp_tertiary_ed_vs_GDP_2014))
  
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

forest_tibble <- forest_tibble %>% # Calculate the per capita value if needed
  mutate(agricultural_output_per_cap_2004 = agricultural_output_2004 / population_2004,
         agricultural_output_per_cap_2014 = agricultural_output_2014 / population_2014,
         tourists_per_cap_2004 = tourists_2004 / population_2004,
         tourists_per_cap_2014 = tourists_2014 / population_2014,
         total_dependency_ratio_mean = (total_dependency_ratio_2004 + 
                                        total_dependency_ratio_2014) / 2,
         oil_production_per_cap_mean = (oil_production_per_cap_2004 +
                                        oil_production_per_cap_2014) / 2,
         democracy_mean = (democracy_2004 +
                                democracy_2014) / 2,
         electricity_per_cap_mean = (electricity_per_cap_2004 +
                                       electricity_per_cap_2014) / 2,
         real_GDP_per_cap_mean = (real_GDP_per_cap_2004 +
                                  real_GDP_per_cap_2014) / 2,
         time_req_to_start_business_mean = (time_req_to_start_business_2004 +
                                            time_req_to_start_business_2014) / 2) %>%
  mutate(tourists_per_cap_mean = (tourists_per_cap_2004 +
                                  tourists_per_cap_2014) / 2) %>%
  mutate(growth = (real_GDP_per_cap_2014 - real_GDP_per_cap_2004) / real_GDP_per_cap_2004) %>%

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
# part of this project

#----------------------------------------------------------------#

# Independent variable: Joining the EU in 2004 (nominal)
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
                 conf.level = 0.05)

## Is it statistically significant?
t_test$p.value < 0.05

## t-test
t_test

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

## We have several outliers. However we decided not to remove them
## since we want to avoid overfitting at all costs

##----------------------------------------------------------------
##                        Build a model                         --
##----------------------------------------------------------------

# Correlation matrix
cor_matrix <- cor(forest_tibble[, 2:19])

## Plot the correlation matrix
corrplot(cor_matrix,
         tl.cex = 0.5)


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

### Remove electricity_per_cap_mean
m3 <- lm(growth ~ oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           time_req_to_start_business_mean +
           real_GDP_per_cap_2004 +
           eu, data = forest_tibble)

#### Diagnostics
summary(m3)

### Remove eu
m4 <- lm(growth ~ oil_production_per_cap_mean + 
           democracy_mean +
           oil_production_per_cap_mean +
           time_req_to_start_business_mean +
           real_GDP_per_cap_2004,
         data = forest_tibble)

#### Diagnostics
summary(m4)

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

# Remove oil_production_per_cap_mean
m6 <- lm(growth ~ democracy_mean +
           real_GDP_per_cap_2004,
         data = forest_tibble)

## Diagnostics
summary(m6)

### Variance inflation factor
vif(type= "predictor", # VIF = 1.387538
    m6) # Multicollinearity does not exist

## Although the model with real_GDP_per_cap_2004 has more explanatory power,
## its statistical significance is low. Perhaps, real_GDP_per_cap_2004 and
## democracy_mean interact. We will try one last model.
m7 <- lm(growth ~ democracy_mean *
           real_GDP_per_cap_2004,
         data = forest_tibble)

### Diagnostics
summary(m7)

### Plot the diagnostics
plot(m7)

## Q-Q plot
qqnorm(m7$residuals) + qqline(m7$residuals)

## Shapiro-Wilk test
shapiro.test(m7$residuals)$p.value < 0.05 # Null-hypothesis: distribution is normal
stdres(m7)
## p-value = 0.0003711
## Normality of residuals is rejected

## Perhaps we need to remove some variables
forest_tibble_2 <- forest_tibble[-c(3, 4, 33),]
# Azerbaijan, Belarus, Switzerland are removed 

## Now call the linear model again
m8 <- lm(growth ~ democracy_mean *
           real_GDP_per_cap_2004,
         data = forest_tibble_2)

### Diagnostics
summary(m8)

### Plot the diagnostics
plot(m8)

## Q-Q plot
qqnorm(m8$residuals) + qqline(m8$residuals) # Seems to be normally distributed

## Shapiro-Wilk test
shapiro.test(m8$residuals)$p.value < 0.05 # We cannot reject the null-hypothesis

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

