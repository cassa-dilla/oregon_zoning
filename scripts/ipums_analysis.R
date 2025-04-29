# ===================================================
# IPUMS Housing Market Analysis
# Purpose: Analyze the impact of HB 2001 using IPUMS data
# ===================================================

# --- Set Working Directory ---
setwd("~/Documents/GitHub/oregon_zoning/")  

# --- Load Libraries ---
library(haven)   # For reading .dta files
library(dplyr)   # For data wrangling
library(readr)   # For reading compressed files if needed

# --- Load Data ---
data_path <- "data/usa_00002 (1).dta.gz"
ipums_data <- read_dta(gzfile(data_path))

# --- Quick Explore ---
head(ipums_data)
names(ipums_data)
str(ipums_data)

# --- Filter to Oregon ---
# Use 'stateicp == 72' for Oregon
oregon_data <- ipums_data %>%
  filter(stateicp == 72)

# --- Basic Analysis ---
# 1. Look at housing structure types
table(oregon_data$unitsstr)

# 2. Average rent over time
oregon_rent_by_year <- oregon_data %>%
  group_by(year) %>%
  summarize(mean_rent = mean(rentgrs, na.rm = TRUE))

print(oregon_rent_by_year)

# 3. Average home value over time
oregon_home_value_by_year <- oregon_data %>%
  group_by(year) %>%
  summarize(mean_home_value = mean(valueh, na.rm = TRUE))

print(oregon_home_value_by_year)

# --- Save Summaries to Output Folder ---
if (!dir.exists("output")) dir.create("output")

write_csv(oregon_rent_by_year, "output/oregon_rent_by_year.csv")
write_csv(oregon_home_value_by_year, "output/oregon_home_value_by_year.csv")

# --- Plotting Rent Trends ---
library(ggplot2)

ggplot(oregon_rent_by_year, aes(x = year, y = mean_rent)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Rent in Oregon (2017–2023)",
    x = "Year",
    y = "Average Monthly Rent ($)"
  ) +
  theme_minimal()

# ===================================================
# End of Script
# ===================================================

