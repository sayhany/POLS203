# Load the required packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(simputation)) install.packages('simputation')
library(simputation)
# Disable scientific notation
options(scipen = 999)

# Read the dataset
pols_203_final_merged <- read_csv("pols_203_final_merged.csv")

# Examine the structure of the dataset
str(pols_203_final_merged)

# View the dataset
View(pols_203_final_merged)

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
  select("id",
         "Entity",
         "Year",
         "value") %>%
  pivot_wider(names_from = "id",
              values_from = "value")

# Split the dataframe into two by year
pols_203_final_merged_2004 <- pols_203_final_merged_2004_2014 %>% # Filter for 2004
  filter(Year == 2004)

pols_203_final_merged_2014 <- pols_203_final_merged_2004_2014 %>% # Filter for 2014
  filter(Year == 2014)

pols_203_joined <- inner_join(pols_203_final_merged_2004,
                     pols_203_final_merged_2014,
                     by = c("Entity"),
                     suffix = c("_2004",
                                "_2014"))

pols_203_joined <- pols_203_joined %>% select(!starts_with("Year")) # Remove the redundant "Year" column

# Rename the "Entities" column
names(pols_203_joined)[names(pols_203_joined) == "Entity"] <- "country" # Change with "country"

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
# We decided to remove the "gov_exp_tertiary_ed_vs_GDP" columns since they have many missing values
# However, we will fill the missing values for other columns using imputation

# Remove "gov_exp_tertiary_ed_vs_GDP_2004" and "gov_exp_tertiary_ed_vs_GDP_2014"
pols_203_joined <- pols_203_joined %>%
  select(!c(gov_exp_tertiary_ed_vs_GDP_2004,
            gov_exp_tertiary_ed_vs_GDP_2014))
  
# Imputation




