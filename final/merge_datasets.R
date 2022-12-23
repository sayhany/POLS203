# Load the required package
if (!require(tidyverse)) install.packages('tidyverse') # Install tidyverse if it is not already installed
library(tidyverse) # Load tidyverse

# Merge the datasets
pols203_final_merged <- list.files(path = getwd(), # Access the working directory
                                   pattern = "^pre_merge_" ) %>%  # Find the datasets to be merged
                        lapply(read_csv) %>% # Read them
                        bind_rows(.id = "id") # Give them IDs

# Replace the IDs with actual titles
pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 1, "total_dependency_ratio"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 2, "agricultural_output"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 3, "gov_exp_tertiary_ed_vs_GDP"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 4, "tourists"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 5, "income_tax"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 6, "oil_production_per_cap"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 7, "democracy"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 8, "electricity_per_cap"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 9, "real_GDP_per_cap"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 10, "time_req_to_start_business"))

# Add populationd data
## Read the file
population_and_demography <- read_csv("population-and-demography.csv")

# Change the "country name" column
colnames(population_and_demography)[1] <- "Entity"

# Select only the necessary columns
population_and_demography <- population_and_demography %>%
  select("Entity",
         "Year",
         "Population")

# Bind
pols203_final_merged <- bind_rows(pols203_final_merged,
          population_and_demography)

# Give it an ID too
pols203_final_merged <- replace_na(data = pols203_final_merged,
           list(id ="population"))

# Examine
View(pols203_final_merged)
