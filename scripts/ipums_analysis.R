# Load required libraries
library(sf)
library(dplyr)
library(readr)
library(here)
library(tibble)

# Define paths
data_dir   <- here::here("data")
output_dir <- here::here("output")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Define target cities
target_cities <- tibble(
  city = c(
    "Baker City", "Eagle Point", "North Bend", "Sweet Home",
    "Ashland", "Cottage Grove", "Klamath Falls", "Newberg",
    "Newport", "Ontario", "Prineville", "Roseburg",
    "Sandy", "Silverton", "Forest Grove", "Tualatin", "West Linn"
  )
)

# Read in shapefiles
tracts <- st_read(file.path(data_dir, "oregon_census_tracts", "CensusTracts.shp"), quiet = TRUE)
cities <- st_read(file.path(data_dir, "oregon_city_limits",   "City_Limits.shp"),   quiet = TRUE)

# Ensure both layers share the same CRS
cities  <- st_transform(cities, st_crs(tracts))

# Spatial join: assign each tract to a city if their geometries intersect
tract_city <- st_join(
  tracts,
  cities %>% select(CITYNAME),
  join = st_intersects,
  left = FALSE   # keep only tracts that intersect a city
) %>%
  st_drop_geometry() %>%
  transmute(
    GEOID = as.character(GEOID),
    city  = CITYNAME
  )

# Filter to our target cities (some county‐level tracts may intersect multiple city polygons; duplicates removed)
tract_city <- tract_city %>%
  filter(city %in% target_cities$city) %>%
  distinct()

# Summarize: list all tract GEOIDs per city
tracts_by_city <- tract_city %>%
  group_by(city) %>%
  summarise(
    tracts = paste(GEOID, collapse = ", ")
  ) %>%
  arrange(city)

# Output results
print(tracts_by_city)
write_csv(tracts_by_city, file.path(output_dir, "tracts_by_city.csv"))
