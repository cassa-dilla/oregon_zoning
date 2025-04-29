# ===================================================
# Re-Zoning Oregon: Effects of HB 2001 on Housing Markets
# ===================================================

# --- Load necessary libraries ---
library(tidyverse)
library(sf)
library(ggplot2)

# --- Set up directories ---
data_dir <- file.path(getwd(), "data")
output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) dir.create(output_dir)

# --- Define target cities ---
target_cities <- c("Baker City", "Eagle Point", "North Bend", "Sweet Home", "Ashland",
                   "Cottage Grove", "Klamath Falls", "Newberg", "Newport", "Ontario",
                   "Prineville", "Roseburg", "Sandy", "Silverton", "Forest Grove",
                   "Tualatin", "West Linn")

# --- Load spatial datasets ---
# Load census tract geometries
tracts_geo <- st_read(file.path(data_dir, "oregon_census_tracts/CensusTracts.shp"))

# Load city limit geometries
cities_geo <- st_read(file.path(data_dir, "oregon_city_limits/City_Limits.shp"))

# --- Filter to target cities ---
cities_geo <- cities_geo %>%
  filter(CITYNAME %in% target_cities)

# Check how many cities we found
target_cities_found <- cities_geo %>% 
  st_drop_geometry() %>%
  select(CITYNAME) %>%
  distinct()
print(target_cities_found)

# --- Create census tract to city mapping ---
# Transform to ensure both datasets use the same coordinate system
tracts_geo <- st_transform(tracts_geo, st_crs(cities_geo))

# Spatial join to determine which tracts fall within our target cities
tract_city_mapping <- st_join(tracts_geo, 
                              cities_geo %>% select(CITYNAME),
                              join = st_intersects)

# Create a non-spatial dataframe linking tract IDs to cities
tract_city_map_df <- tract_city_mapping %>%
  st_drop_geometry() %>%
  select(GEOID, CITYNAME) %>%
  filter(!is.na(CITYNAME))

# Check how many tracts we have for each city
city_tract_counts <- tract_city_map_df %>%
  group_by(CITYNAME) %>%
  summarize(tract_count = n())
print(city_tract_counts)

# --- Load yearly housing data ---
# Set up years to analyze
years <- 2017:2023

# Function to load and standardize yearly data
load_yearly_data <- function(year) {
  file_path <- file.path(data_dir, paste0("oregon_", year, ".csv"))
  if(file.exists(file_path)) {
    df <- read_csv(file_path)
    df$year <- year
    return(df)
  } else {
    warning(paste("File", file_path, "not found."))
    return(NULL)
  }
}

# Load all available yearly data
yearly_data_list <- lapply(years, load_yearly_data)
yearly_data_list <- yearly_data_list[!sapply(yearly_data_list, is.null)]

# Check which years we have data for
years_available <- sapply(yearly_data_list, function(df) unique(df$year))
print("Years with available data:")
print(years_available)

# --- Identify relevant variables ---
# Check the structure of the first yearly dataset
if(length(yearly_data_list) > 0) {
  yearly_data_sample <- yearly_data_list[[1]]
  
  # Look at column names
  cols <- colnames(yearly_data_sample)
  print("Sample of column names:")
  print(head(cols, 20))  # Print first 20 column names
  
  # Look for tract identifier to match with our mapping
  id_cols <- cols[grep("geoid|tract|fips|id", cols, ignore.case = TRUE)]
  print("Potential ID columns for mapping:")
  print(id_cols)
  
  # Looking at the data dictionary for housing value and rent variables
  potential_housing_vars <- c(
    # Home value variables
    "SE_A10035", # House Value for All Owner-Occupied Housing Units
    "SE_A10036", # Median House Value for All Owner-Occupied Housing Units
    # Rent variables
    "SE_A18001", # Gross Rent (Housing Units with Cash Rent)
    "SE_A18009"  # Median Gross Rent
  )
  
  # Check if these variables exist in our data
  home_value_cols <- grep("SE_A10035|SE_A10036", cols, value = TRUE)
  print("Home value columns found:")
  print(head(home_value_cols))
  
  rent_cols <- grep("SE_A18001|SE_A18009", cols, value = TRUE)
  print("Rent columns found:")
  print(head(rent_cols))
}

# --- Process yearly data and join with city mapping ---
# Initialize lists to store processed data
all_city_housing_data <- list()
all_city_rent_data <- list()

# Process each year's data
for(i in 1:length(yearly_data_list)) {
  year_data <- yearly_data_list[[i]]
  year <- unique(year_data$year)
  
  # Get the GEOID column (assuming it's Geo_GEOID)
  if("Geo_GEOID" %in% colnames(year_data)) {
    id_col <- "Geo_GEOID"
  } else {
    # Try to find alternative GEOID column
    potential_id_cols <- grep("GEOID|TRACT|FIPS", colnames(year_data), value = TRUE, ignore.case = TRUE)
    if(length(potential_id_cols) > 0) {
      id_col <- potential_id_cols[1]
      print(paste("Using", id_col, "as ID column for year", year))
    } else {
      warning(paste("No ID column found for year", year))
      next
    }
  }
  
  # Find median home value column
  home_val_col <- grep("SE_A10036_001", colnames(year_data), value = TRUE)
  if(length(home_val_col) == 0) {
    warning(paste("No median home value column found for year", year))
    home_val_data <- NULL
  } else {
    # Join with tract-city mapping and summarize home values
    home_val_data <- year_data %>%
      select(!!sym(id_col), !!sym(home_val_col)) %>%
      rename(GEOID = !!sym(id_col), median_home_value = !!sym(home_val_col)) %>%
      inner_join(tract_city_map_df, by = "GEOID") %>%
      mutate(year = year)
    
    all_city_housing_data[[i]] <- home_val_data
  }
  
  # Find median rent column
  rent_col <- grep("SE_A18009_001", colnames(year_data), value = TRUE)
  if(length(rent_col) == 0) {
    warning(paste("No median rent column found for year", year))
    rent_data <- NULL
  } else {
    # Join with tract-city mapping and summarize rent values
    rent_data <- year_data %>%
      select(!!sym(id_col), !!sym(rent_col)) %>%
      rename(GEOID = !!sym(id_col), median_rent = !!sym(rent_col)) %>%
      inner_join(tract_city_map_df, by = "GEOID") %>%
      mutate(year = year)
    
    all_city_rent_data[[i]] <- rent_data
  }
}

# Combine all years' data
city_housing_data <- bind_rows(all_city_housing_data)
city_rent_data <- bind_rows(all_city_rent_data)

# --- Calculate city-level statistics by year ---
# Housing values by city and year
city_housing_by_year <- city_housing_data %>%
  group_by(CITYNAME, year) %>%
  summarize(
    mean_home_value = mean(median_home_value, na.rm = TRUE),
    median_home_value = median(median_home_value, na.rm = TRUE),
    min_home_value = min(median_home_value, na.rm = TRUE),
    max_home_value = max(median_home_value, na.rm = TRUE),
    tract_count = n(),
    .groups = "drop"
  )

# Rent values by city and year
city_rent_by_year <- city_rent_data %>%
  group_by(CITYNAME, year) %>%
  summarize(
    mean_rent = mean(median_rent, na.rm = TRUE),
    median_rent = median(median_rent, na.rm = TRUE),
    min_rent = min(median_rent, na.rm = TRUE),
    max_rent = max(median_rent, na.rm = TRUE),
    tract_count = n(),
    .groups = "drop"
  )

# Print samples of the processed data
print("Sample of housing data by city and year:")
print(head(city_housing_by_year))

print("Sample of rent data by city and year:")
print(head(city_rent_by_year))

# --- Define pre and post HB 2001 periods ---
# Define implementation periods
# HB 2001 was enacted in 2019, so pre is 2017-2018 and post is 2020-2023
city_housing_by_year <- city_housing_by_year %>%
  mutate(period = case_when(
    year < 2019 ~ "Pre-HB2001",
    year == 2019 ~ "Implementation",
    year > 2019 ~ "Post-HB2001"
  ))

city_rent_by_year <- city_rent_by_year %>%
  mutate(period = case_when(
    year < 2019 ~ "Pre-HB2001",
    year == 2019 ~ "Implementation",
    year > 2019 ~ "Post-HB2001"
  ))

# --- Pre/post analysis ---
# Calculate average values for pre and post periods by city
housing_period_summary <- city_housing_by_year %>%
  filter(period != "Implementation") %>%
  group_by(CITYNAME, period) %>%
  summarize(
    avg_median_home_value = mean(median_home_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_median_home_value
  ) %>%
  mutate(
    percent_change = ((Post.HB2001 / Pre.HB2001) - 1) * 100
  ) %>%
  arrange(desc(percent_change))

rent_period_summary <- city_rent_by_year %>%
  filter(period != "Implementation") %>%
  group_by(CITYNAME, period) %>%
  summarize(
    avg_median_rent = mean(median_rent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_median_rent
  ) %>%
  mutate(
    percent_change = ((Post.HB2001 / Pre.HB2001) - 1) * 100
  ) %>%
  arrange(desc(percent_change))

# View the results
print("Home value percent change by city (pre vs post HB 2001):")
print(housing_period_summary)

print("Rent percent change by city (pre vs post HB 2001):")
print(rent_period_summary)

# --- Save results to output directory ---
write_csv(city_housing_by_year, file.path(output_dir, "oregon_home_value_by_year.csv"))
write_csv(city_rent_by_year, file.path(output_dir, "oregon_rent_by_year.csv"))
write_csv(housing_period_summary, file.path(output_dir, "housing_change_summary.csv"))
write_csv(rent_period_summary, file.path(output_dir, "rent_change_summary.csv"))

# --- Create visualizations ---
# Visualize home value changes over time
home_value_plot <- ggplot(city_housing_by_year, 
                          aes(x = year, y = median_home_value, color = CITYNAME, group = CITYNAME)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Median Home Values by City (2017-2023)",
       subtitle = "Vertical line shows HB 2001 implementation",
       x = "Year", 
       y = "Median Home Value ($)") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black") +
  theme(legend.position = "right")
print(home_value_plot)
ggsave(file.path(output_dir, "home_value_trends.png"), home_value_plot, width = 12, height = 8)

# Visualize rent changes over time
rent_plot <- ggplot(city_rent_by_year, 
                    aes(x = year, y = median_rent, color = CITYNAME, group = CITYNAME)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Median Rent by City (2017-2023)",
       subtitle = "Vertical line shows HB 2001 implementation",
       x = "Year", 
       y = "Median Rent ($)") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "black") +
  theme(legend.position = "right")
print(rent_plot)
ggsave(file.path(output_dir, "rent_trends.png"), rent_plot, width = 12, height = 8)

# Create bar charts showing percent changes
home_value_change_plot <- ggplot(housing_period_summary, 
                                 aes(x = reorder(CITYNAME, percent_change), y = percent_change)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Home Value % Change After HB 2001",
       subtitle = "Pre (2017-2018) vs Post (2020-2023)",
       x = "City", 
       y = "Percent Change in Median Home Value") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
print(home_value_change_plot)
ggsave(file.path(output_dir, "home_value_percent_change.png"), home_value_change_plot, width = 10, height = 8)

rent_change_plot <- ggplot(rent_period_summary, 
                           aes(x = reorder(CITYNAME, percent_change), y = percent_change)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Rent % Change After HB 2001",
       subtitle = "Pre (2017-2018) vs Post (2020-2023)",
       x = "City", 
       y = "Percent Change in Median Rent") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
print(rent_change_plot)
ggsave(file.path(output_dir, "rent_percent_change.png"), rent_change_plot, width = 10, height = 8)