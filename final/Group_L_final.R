# Load the required packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# Read the dataset
pols_203_final_merged <- read_csv("pols_203_final_merged.csv")

# View the datset
View(pols_203_final_merged)

# Filter 2004 and 2014
pols_203_final_merged_2004_2014 <- pols_203_final_merged %>%
  filter(Year == 2004 | Year == 2014)
View(pols_203_final_merged_2004_2014)

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
country_vector <- sort(country_vector) # Alphabetically rearrange

# Filter for those countries
pols_203_final_merged_2004_2014 <- pols_203_final_merged_2004_2014 %>%
  filter(Entity %in% country_vector)

# colnames(pols_203_final_merged_2004_2014) <- c("id",
#                                               "Entity",
#                                               "Code",
#                                               "Year",
#                                               "total_dependency_ratio",
#                                               "")

# Move values to a single column to prepare the df for wrangling
pols_203_final_merged_2004_2014$Value <-c(pols_203_final_merged_2004_2014$`Total dependency ratio - Sex: all - Age: none - Variant: estimates`[1:68],
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
         "Value") %>%
  pivot_wider(names_from = "id",
              values_from = "Value")

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
pols_203_joined <- pols_203_joined %>% select(!starts_with("Year"))
