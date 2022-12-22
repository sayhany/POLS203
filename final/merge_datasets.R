# Load the required package
if (!require(tidyverse)) install.packages('tidyverse') # Install tidyverse if it is not already installed
library(tidyverse) # Load tidyverse

# Merge the datasets
pols203_final_merged <- list.files(path = getwd(), # Access the working directory
                                   pattern = "^pre_merge_" ) %>%  # Find the datasets to be merged
                        lapply(read_csv) %>% # Read them
                        bind_rows(.id = "id") # Give them IDs
# Examine
View(pols203_final_merged)

# Replace the IDs with actual titles
pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 1, "total_dependency_ratio"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 2, "agricultural_output"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 3, "share_of_agriculture_in_GDP"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 4, "expenditure_in_tertiary_educatio"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 5, "marginal_income_tax"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 6, "book_titles_Per_cap"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 7, "internet_users"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 8, "participation_vdem"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 9, "electricity_Per_cap"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 10, "Real_GDP_Per_cap"))

pols203_final_merged <- pols203_final_merged %>%
  mutate(id = replace(id, id == 11, "time_req_to_start_a_business"))