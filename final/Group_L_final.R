# Load the required packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

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
pols_203_final_merged_2004_2014$value <-c(pols_203_final_merged_2004_2014$`Total dependency ratio - Sex: all - Age: none - Variant: estimates`[1:68],
                                          pols_203_final_merged_2004_2014$output_quantity[69:134],
                                          pols_203_final_merged_2004_2014$`Share of agriculture in GDP at current prices (Herrendorf et al. and GGDC-10 data)`[135:202],
                                          pols_203_final_merged_2004_2014$`Government expenditure on tertiary education as % of GDP (%)`[203:233],
                                          pols_203_final_merged_2004_2014$`Book titles per capita (Fink-Jensen 2015)`[234:241],
                                          pols_203_final_merged_2004_2014$`Number of Internet users`[242:307],
                                          pols_203_final_merged_2004_2014$`Oil production per capita (kWh)`[308:374],
                                          pols_203_final_merged_2004_2014$particip_vdem_owid[375:442],
                                          pols_203_final_merged_2004_2014$`Per capita electricity (kWh)`[443:509],
                                          pols_203_final_merged_2004_2014$`GDP per capita (output, multiple price benchmarks)`[510:577],
                                          pols_203_final_merged_2004_2014$`Time required to start a business (days)`[578:642])
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

# "share_of_agriculture_in_GDP", "expenditure_in_tertiary_educatio_2004" and "book_titles_Per_cap" columns
# have missing values for many countries
## Omitting them is the most reasonable choice
## Omit "share_of_agriculture_in_GDP", "expenditure_in_tertiary_educatio_2004" and "book_titles_Per_cap"
pols_203_joined <- pols_203_joined %>%
  select(-c("share_of_agriculture_in_GDP_2004",
           "share_of_agriculture_in_GDP_2014",
           "book_titles_Per_cap_2004",
           "book_titles_Per_cap_2014",
           "expenditure_in_tertiary_educatio_2004",
           "expenditure_in_tertiary_educatio_2014"))

# Omit countries that still have missing values
pols_203_joined <- na.omit(pols_203_joined)

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
        "Sweden")
