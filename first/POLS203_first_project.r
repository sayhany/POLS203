# Group L First Project
## Emirhan Yücel 2020302264
## Mübin Salih Sarıçiçek 2020302243
## Sayhan Yalvaçer 2019202063

# =============================================================== #
#### WARNING: ENLARGE YOUR "PLOT" PANE TO VIEW THE PLOTS PROPERLY
# =============================================================== #


# Research Question: Did former Eastern Bloc countries that joined the EU in 2004 
# enjoy higher GDP PPP per capita growth rates?

# Introduction of the Dataset: Data from the database: World Development Indicators, Last Updated: 09/16/2022
# Link for the dataset: https://www.dropbox.com/s/fuuagwlepwu0gvh/8ec8a9c3-0191-4773-b494-c992bbc85257_Data.csv?dl=0

# Independent Variables: Joining the EU in 2004 (nominal)
# Dependent Variables: GDP PPP per capita growth (numerical)
# Number of observations: 24

# We did not include some former Eastern Bloc countries in the dataset to avoid interference 
# with the results because they joined the EU either after 2004 or before 2013

# Hypothesis: Former Eastern Bloc countries that did not join the EU in 2004 had higher GDP  per capita PPP growth rates between 2004-2013

# Strategy: Subset EU and non-EU countries, compute the mean GDP PPP per capita growth rates 
# between 2004-2013 for both the EU and non-EU former Eastern Bloc countries, 
# and compare their GDP per capita PPP growth rates to see which one is higher

# Load the required packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes)

###DATA STRUCTURE###
# We chose the years 2004 and 2013 for our analysis,
# and we limited the data by utilizing World Bank Data Bank's built-in tools

polsdata <- read_csv("8ec8a9c3-0191-4773-b494-c992bbc85257_Data.csv") # Read the dataset by using readr

polsdata <- na.omit(polsdata) # Omit N/A values from the dataset

# Explore the dataset
head(polsdata)
str(polsdata)
nrow(polsdata) # number of rows = 12
ncol(polsdata) # number of columns = 6
summary(polsdata)
sd(polsdata$`2004 [YR2004]`)
sd(polsdata$`2013 [YR2013]`)

## For Year 2004,

# The Minimum value is 4429 and the Maximum value is 22761
# The Median is 13192 and the mean is 13013
# The Standart deviation is 5629.63
# The 1st quartile is 9793 and the 3rd quartile is 15488

## For Year 2013,

# The Minimum value is 8382 and the maximum value is 30818
# The Median is 25321 and the mean is 23162
# The standart deviation is 7291.851
# The 1st quartile is 21728 and the 3rd quartile is 27577


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

#We want to examine if 2004 EU entrance had an effect on Eastern Bloc countries, 
#therefore we decided to compare the economic development of former Eastern Bloc countries that 
#joined EU and those that didn't through the years between 2004 and 2013

# Treatment Group:
polsdata_new_EU <- polsdata_new %>%
  filter(Joined_EU) #Filtered EU countries to be set as our treatment group

# Control Group:
polsdata_new_NON_EU <- polsdata_new %>%
  filter(!Joined_EU) #Filtered non-EU countries to be set as our control group

## Boxplots for spotting outliers
if(dev.cur() > 1) dev.off() # Reset par()

boxplot(polsdata_new_EU$Growth,
        main = "Former Eastern Bloc countries that joined the EU",
        ylab = "GDP PPP per capita growth per cent",
        outline = TRUE) 

boxplot(polsdata_new_NON_EU$Growth,
        main = "Former Eastern Bloc countries that did not join the EU",
        ylab = "GDP PPP per capita growth per cent",
        outline = TRUE) 

# By examining the boxplots we can infer that:

# There are no outliers in either samples.
# Right and Left whiskers of the boxplot each represent 25% of the data 
# and the box in the middle represents 50% of the data
# The bold line in the middle of our boxplot is our median
# Countries that joined the EU have a right skewed distribution. 

# Descriptive statistics

## Measures of central tendency

median(polsdata_new$Growth) # Median growth: ~86.65%
mean(polsdata_new$Growth) # Mean growth: ~88.03%

## Measures of spread
## Maximum growth: Russian Federation, ~154.95%
max(polsdata_new$Growth) # What is the maxium growth rate?
polsdata_new$Countries[which.max(polsdata_new$Growth)] # Which country has the maximum growth rate?

## Minimum growth: Slovenia, ~31.68%
min(polsdata_new$Growth) # What is the minimum growth rate?
polsdata_new$Countries[which.min(polsdata_new$Growth)] # Which country has the minimum growth rate?

## Quartiles
quantile(polsdata_new$Growth,
         probs = seq(0, 1, 0.25),
         digits = 2)
# When the data is sorted in ascending order,
# the First Quartile represents the lower 25% of the data. 
# The Second Quartile represents the  lower 50%, it is also known as the median
# The Third Quartile represents the lower 75% of the data

# The 1st Quartile is 75.23083
# The 2nd Quartile is 86.65767, this is our median
# The 3rd Quartile is 107.07184


## Inter-quartile range
IQR(polsdata_new$Growth) # the difference between the 3rd and 1st quartiles

## Summarize two groups
polsdata_new %>%
  group_by(Joined_EU) %>% # Groups the data into two groups: EU and non-EU
  summarize("2004 per capita PPP mean" = mean(GDP_Per_cap_PPP_2004),
            "2013 per capita PPP mean" = mean(GDP_Per_cap_PPP_2013),
            "Mean growth" = mean(Growth),
            "Maximum growth" = max(Growth),
            "Minimum growth" = min(Growth))

# Both groups enjoyed GDP per capita PPP Growth, but on different scales. 
# Countries that joined the EU in 2004 grew on average by 72%, 
# whereas countries that didn't join the EU grew on average by 120%


## Calculate the sample sizes
n1 <- nrow(polsdata_new_EU)
n2 <- nrow(polsdata_new_NON_EU)

## Calculate the standard deviations
sd1 <- sd(polsdata_new_EU$Growth) #Standard deviation of Growth for the countries that joined the EU
sd2 <- sd(polsdata_new_NON_EU$Growth) #Standard deviation of Growth for the countries that didn not join the EU

## Calculate the standard error
se <- sqrt(( sd1 ^ 2 / n1 ) + (sd2 ^ 2 / n2))

## Plot barplots to visually describe our data
#### Warning: Enlarge your "Plots" pane to view the plots properly

### Plot GDP per capita PPP
par(mai = c(1.75, 0.8, 0.9, 0.9),
    mfrow = c(2, 2),
    cex.axis = 0.7,
    cex.lab = 0.5,
    cex.main = 0.9)

#### EU
barplot(polsdata_new_EU$GDP_Per_cap_PPP_2004,
        ylim = c(0, 30000),
        names = polsdata_new_EU$Countries,
        col = "#004494",
        main = "EU 2004",
        ylab = "GDP per capita, PPP (current international $)",
        las = 2)

barplot(polsdata_new_EU$GDP_Per_cap_PPP_2013,
        names = polsdata_new_EU$Countries,
        ylim = c(0, 30000),
        col = "#004494",
        main = "EU 2013",
        ylab = "GDP per capita, PPP (current international $)",
        las = 2)



#### Non-EU
barplot(polsdata_new_NON_EU$GDP_Per_cap_PPP_2004,
        ylim = c(0, 30000),
        names = polsdata_new_NON_EU$Countries,
        col = "#CC0000",
        main = "Non-EU 2004",
        ylab = "GDP per capita, PPP (current international $)",
        las = 2)

barplot(polsdata_new_NON_EU$GDP_Per_cap_PPP_2013,
        names = polsdata_new_NON_EU$Countries,
        ylim = c(0, 30000),
        col = "#CC0000",
        main = "Non-EU 2013",
        ylab = "GDP per capita, PPP (current international $)",
        las = 2)


# Preparation for plotting growth rates
### Select the required data from the dataframe
growth_4_plot <- select(polsdata_new,
                        Growth,
                        Countries,
                        Joined_EU)

### Replace the logical values with strings i. e. "Yes" or "No"
growth_4_plot$Joined_EU <- gsub(FALSE,
                                "No",
                                growth_4_plot$Joined_EU)

growth_4_plot$Joined_EU <- gsub(TRUE,
                                "Yes",
                                growth_4_plot$Joined_EU)
### Sort by growth rate
growth_4_plot <- growth_4_plot %>%
  arrange(desc(Growth))

# Plotting growth rates
## Plot growth between 2004-2013 for all countries in the dataset
ggplot(growth_4_plot,
       aes(x = fct_reorder(Countries, Growth),
           y = Growth, fill = Joined_EU)) +
  geom_col() +
  xlab("Countries") +
  ylab("GDP per capita, PPP Growth per cent 2004-2013") +
  coord_flip() +
  theme_economist() +
  theme(axis.title.x = element_text(margin=margin(t = 15)),
        axis.title.y = element_text(margin=margin(r = 15))) +
  scale_fill_discrete("Did join the EU?") +
  geom_hline(yintercept = mean(growth_4_plot$Growth), color = "grey40", linetype = 3) +
  annotate(
    "text",
    x = 4, y = 98,
    label = "The\nmean\ngrowth",
    vjust = 1, size = 2.5, color = "grey40") +
  annotate(
    "curve",
    x = 4.1, y = 98,
    xend = 5, yend = 90,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
    color = "grey40") +
  labs(caption = "Source: World Bank")

# In the plot, GDP per capita PPP Growth of countries between 2003 and 2004 have been listed in descending order. 
# Russian Federation enjoyed the highest GDP growth rate while Slovenia had the lowest. 
# Russian Federation, Belarus, Armenia, Lithuania, Moldova, Estonia's GDP per capita PPP growth
# was higher than the mean growth.

## Find the mean growth rates
EU_Growth_mean <- mean(polsdata_new_EU$Growth)
NON_EU_Growth_mean <- mean(polsdata_new_NON_EU$Growth)

## Find the difference of means
difference <- NON_EU_Growth_mean - EU_Growth_mean

## Examine
difference
### Since the difference is positive we can say that former Eastern Bloc countries
#who did not join the EU in 2004 enjoyed higher GDP per capita PPP growth between 2004-2013


# Significance Test
## H0 = Mean GDP per capita PPP growth rates between 2004 and 2013 for former Eastern Bloc countries that joined and did not join the EU are equal
## HA = Mean GDP per capia PPP growth rates between 2004 and 2013 for former Eastern Bloc countries that joined and did not join the EU are different
## We chose the t-test over the z-test since n<30
t_test <- t.test(polsdata_new_NON_EU$Growth, 
                 polsdata_new_EU$Growth, 
                 paired = FALSE,
                 conf.level = 0.95)
## Examine the results of the t-test 
t_test
# t-test findings: t = 2.961, df = 5.6924, p-value = 0.02689

## Is it statistically significant?
t_test$p.value < 0.05
### Since our p-value is smaller than 0.05, we are able to reject the null-hypothesis

# Confidence interval
## (y1 - y2) ± (t * se)
## Calculate degrees of freedom using Welch–Satterthwaite approximation from the textbook
degrees_of_freedom <- (sd2 ^ 2 / n2 + sd1 ^ 2 / n1) ^ 2 / (sd1 ^ 4 / ((n1 ^ 2 * (n1 - 1))) + sd2 ^ 4 / ((n2 ^ 2 * (n2 - 1)))) # Welch–Satterthwaite equation

## Calculate the t-score
t_score <- qt(0.975, degrees_of_freedom)

## Calculate the boundaries
lower_boundary <- difference - t_score * se
upper_boundary <- difference + t_score * se

## Concatenate
ci <- c(lower_boundary, upper_boundary)

# Examine the confidence interval
ci

### CONCLUSION ###

# We wanted to explore the effects of the former Eastern Bloc countries' 2004 EU accession on their economic growth
# We categorized countries according to whether they joined the EU in 2004 or not
# We calculated the mean growth rates between the years 2004 and 2013 for each group
# After looking at the difference between the two means, we concluded by saying that,
# countries that joined the EU in 2004 EU did have lower GDP per capita PPP growth rates compared to those that did not join
