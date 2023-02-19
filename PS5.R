# Load necessary packages
library(tidyverse)

# Load data
gapminder_data <- read.delim("C:/Users/User1/Downloads/gapminder.csv.bz2")

# Check number of rows and columns
nrow(gapminder_data)
ncol(gapminder_data)

# Print a small sample of data
head(gapminder_data)

# Number of countries in dataset
n_distinct(gapminder_data$iso2)
n_distinct(gapminder_data$iso3)
n_distinct(gapminder_data$name)

# Number of names for each iso-2 code
gapminder_data %>% 
  group_by(iso2, name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 1)

# Number of iso3-codes for each name
gapminder_data %>% 
  group_by(iso3, name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 1)

# Minimum and maximum year in data
min(gapminder_data$time)
max(gapminder_data$time)

# Missing CO2 emissions by year
gapminder_data %>% 
  group_by(time) %>% 
  summarize(missing_co2 = sum(is.na(co2)), missing_co2_pc = sum(is.na(co2_PC)))

# Plot of total CO2 emissions over time for selected countries
gapminder_data %>% 
  filter(name %in% c("United States", "China", "India", "Canada", "Brazil")) %>% 
  ggplot(aes(x = time, y = co2, color = name)) +
  geom_line() +
  labs(title = "Total CO2 emissions over time for selected countries")

# Plot of CO2 emissions per capita over time for selected countries
gapminder_data %>% 
  filter(name %in% c("United States", "China", "India", "Canada", "Brazil")) %>% 
  ggplot(aes(x = time, y = co2_PC, color = name)) +
  geom_line() +
  labs(title = "CO2 emissions per capita over time for selected countries")

# Average CO2 emissions per capita by continent
gapminder_data %>% 
  group_by(region) %>% 
  summarize(avg_co2_pc = mean(co2_PC, na.rm = TRUE)) %>% 
  arrange(desc(avg_co2_pc))
