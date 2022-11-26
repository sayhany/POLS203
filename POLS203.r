# Group L First Project
## Emirhan Yücel 2020302264
## Mübin Salih Sarıçiçek 2020302243
## Sayhan Yalvaçer 2019202063

# Research Question: Did former Eastern Bloc countries that joined the EU in 2004 enjoy higher GDP PPP per capita growth rates?
# Introduction of the Dataset: Data from the database: World Development Indicators, Last Updated: 09/16/2022
# Link for the dataset: https://www.dropbox.com/s/fuuagwlepwu0gvh/8ec8a9c3-0191-4773-b494-c992bbc85257_Data.csv?dl=0
# Independent Variables: Joining the EU in 2004
# Dependent Variables: GDP PPP per capita Growth
# Hypothesis: Eastern Bloc countries that did not join the EU in 2004 had higher GDP PPP per capita growth rates between 2004-2013
# Strategy: Subset EU and non-EU countries, compute the mean GDP PPP per capita growth rates between 2004-2013 for both the EU and non-EU former Eastern Bloc countries, and compare their GDP to see which one is higher
# Interpretation: Former Eastern Bloc countries that did not join the EU in 2004 enjoyed higher GDP growth rates throughout the following decade

# Load the required packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes)
if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)

# Read the dataset by using readr
polsdata <- read_csv("8ec8a9c3-0191-4773-b494-c992bbc85257_Data.csv")

# Omit N/A values from the dataset
polsdata <- na.omit(polsdata)

# Explore the dataset
head(polsdata)
nrow(polsdata)

# Rename the columns
colnames(polsdata) <- c("country",
                        "country_code",
                        "series",
                        "series_code",
                        "2004",
                        "2013")

# Extract the countries from the dataset
Countries <- polsdata$country

# Extract the 2004 GDP per capita PPP data
GDP_Per_cap_PPP_2004 <- polsdata$`2004`

# Extract the 2013 GDP per capita PPP data
GDP_Per_cap_PPP_2013 <- polsdata$`2013`

# Create a new dataframe for research
polsdata_new <- data.frame(Countries, 
                           GDP_Per_cap_PPP_2004,
                           GDP_Per_cap_PPP_2013)
# Examine
polsdata_new
# Growth rates
Growth <- ((polsdata_new$GDP_Per_cap_PPP_2013 - polsdata_new$GDP_Per_cap_PPP_2004) / polsdata_new$GDP_Per_cap_PPP_2004) * 100
# Examine
Growth

# Add as a new column to the data frame
polsdata_new$Growth <- Growth

# Which countries joined the EU in 2004?
Joined_EU <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)

# Add to the dataframe
polsdata_new$Joined_EU <- Joined_EU

# Control and Treatment Groups
## Control group: Former Eastern Bloc countries that did not join the EU
## Treatment group: Former Eastern Bloc countries that joined EU in 2004
# Treatment: Joining the EU in 2004

# Treatment Group:
polsdata_new_EU <- polsdata_new %>%
  filter(Joined_EU)

# Control Group:
polsdata_new_NON_EU <- polsdata_new %>%
  filter(!Joined_EU)

# Boxplots
boxplot(polsdata_new_EU$Growth, 
        outline = TRUE)
boxplot(polsdata_new_NON_EU$Growth,
        outline = TRUE)
# Find mean growth rates
EU_Growth_mean <- mean(polsdata_new_EU$Growth)
NON_EU_Growth_mean <- mean(polsdata_new_NON_EU$Growth)

# Find the difference of means
difference <- NON_EU_Growth_mean - EU_Growth_mean

# Examine
difference

# Calculate the standard deviations
n1 <- nrow(polsdata_new_EU)
n2 <- nrow(polsdata_new_NON_EU)

sd1 <- sd(polsdata_new_EU$Growth)
sd2 <- sd(polsdata_new_NON_EU$Growth)
# Calculate the standard error
se <- sqrt(( sd1 ^ 2 / n1 ) + (sd2 ^ 2 / n2))

# Maximum growth: Russian Federation, ~154.95%
max(polsdata_new$Growth)
# Minimum growth: Slovenia, ~31.68%
min(polsdata_new$Growth)
# Median growth: ~86.65%
median(polsdata_new$Growth)
# Quartiles
quantile(polsdata_new$Growth,
         probs = seq(0, 1, 0.25),
         digits = 2)
# Inter-quartile range
IQR(polsdata_new$Growth,)
# Summarize two groups
polsdata_new %>%
  group_by(Joined_EU) %>%
    summarize("2004 per capita PPP mean" = mean(GDP_Per_cap_PPP_2004),
              "2004 per capita PPP mean" = mean(GDP_Per_cap_PPP_2013),
              "Mean growth" = mean(Growth),
              "Maximum growth" = max(Growth),
              "Minimum growth" = min(Growth))
# Significance Test
# HA = Mean GDP per capia PPP growth rates between 2004 and 2013 for former Eastern Bloc countries that joined and did not join the EU are different
# H0 = Mean GDP per capita PPP growth rates 2004 and 2013 for former Eastern Bloc countries that joined and did not join the EU are equal
t <- difference / se
pt(t, n1 + n2 - 1, lower.tail = FALSE)
t_test <- t.test(polsdata_new_NON_EU$Growth, 
                 polsdata_new_EU$Growth, 
                 paired = FALSE,
                 conf.level = 0.95)
# Is it statistically significant?
t_test$p.value < 0.05
# We are able to reject the null-hypothesis

# Confidence interval
## (y1 - y2) ± (t * se)
## Calculate degrees of freedom using Welch–Satterthwaite equation
degrees_of_freedom <- (sd2 ^ 2 / n2 + sd1 ^ 2 / n1) ^ 2 / (sd1 ^ 4 / ((n1 ^ 2 * (n1 - 1))) + sd2 ^ 4/((n2 ^ 2 * (n2 - 1))))
## Calculate the t-score
t_score <- qt(0.975, degrees_of_freedom)
## Calculate the boundaries
lower_boundary <- difference - t_score * se
upper_boundary <- difference + t_score * se
## Concatenate
ci <- c(lower_boundary, upper_boundary)

# Preparation for plotting
## 1st plot
### Extract 2004 data
pols_4_plot_2004 <- data.frame(polsdata$country, polsdata$`2004`)
pols_4_plot_2004$Year <- rep(c(2004), each = n1 + n2)
colnames(pols_4_plot_2004) <- c("Country",
                                "GDP PPP Per capita",
                                "Year")

pols_4_plot_2004$"Did Join the EU?" <- Joined_EU
### Extract 2013 data
pols_4_plot_2013 <- data.frame(polsdata$country, polsdata$`2013`)
pols_4_plot_2013$Year <- rep(c(2013), each = n1 + n2)

colnames(pols_4_plot_2013) <- c("Country",
                                "GDP PPP Per capita",
                                "Year")
pols_4_plot_2013$"Did Join the EU?" <- Joined_EU
## Bind
pols_4_plot <- rbind(pols_4_plot_2004, pols_4_plot_2013)
### Replace the logical values with strings i. e. "Yes" or "No"
pols_4_plot$`Did Join the EU?` <- gsub(FALSE,
                                       "No",
                                       pols_4_plot$`Did Join the EU?`)

pols_4_plot$`Did Join the EU?` <- gsub(TRUE,
                                       "Yes",
                                       pols_4_plot$`Did Join the EU?`)

## 2nd plot
### Select the required data from the dataframe
growth_4_plot <- select(polsdata_new,
                        Growth,
                        Countries,
                        Joined_EU)

growth_4_plot$Joined_EU <- gsub(FALSE,
                                "No",
                                growth_4_plot$Joined_EU)

growth_4_plot$Joined_EU <- gsub(TRUE,
                                "Yes",
                                growth_4_plot$Joined_EU)
### Sort by growth rate
growth_4_plot <- growth_4_plot %>%
  arrange(desc(Growth))
# Plotting
ggplot(pols_4_plot,
       aes(x = factor(Year),
          y = `GDP PPP Per capita`,
          color = `Did Join the EU?`,
          label = Country)) +
          geom_point() +
          theme_economist() +
          xlab("Year") +
          ylab("GDP per capita, PPP (current international $)") +
          geom_text_repel(show.legend = FALSE)

ggplot(growth_4_plot,
       aes(x = fct_reorder(Countries, Growth),
           y = Growth, fill = Joined_EU)) +
          geom_col() +
          xlab("Year") +
          ylab("GDP per capita, PPP Growth per cent 2004-2013") +
          coord_flip() +
          theme_economist() +
          scale_fill_discrete("Did join the EU?")                                                                                                                                                                 
                                                                                                                                                                                                         