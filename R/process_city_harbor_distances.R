# Copyright 2025 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# This script processes and cleans city geographic data, then calculates
# the distance from each city to the nearest major harbors in China.
# The processed data is then saved for further analysis.

# It includes the following main sections:
# 1. Environment setup and library loading.
# 2. Definition of a custom function for calculating nearest harbor distances.
# 3. Preparation and cleaning of raw port (harbor) information.
# 4. Loading, cleaning, and standardizing China administrative map data.
# 5. Calculation of distances to all and big harbors.
# 6. Final data cleaning, variable creation, and saving.

# Custom helper functions like 'chinese_to_standard_chinese_map', 'province_name_map',
# and 'geneAbbr' are assumed to be defined in the 'R/funs' directory.

# Clean Environment -------------------------------------------------------
# Clear all objects from the global environment to ensure a clean slate.
rm(list = ls())
# Perform garbage collection to free up memory.
gc()


# Load Packages and Custom Functions --------------------------------------
# Load the tidyverse suite of packages for data manipulation and visualization.
library(tidyverse)
# Load the here package for easy and reproducible file path management.
library(here)
# Load the sf package for handling spatial vector data (Simple Features).
library(sf)
# The dplyr package is part of tidyverse, explicitly loading might be redundant but harmless.
library(dplyr)
# Load ggplot2 for creating high-quality data visualizations (though not used directly for plotting here).
library(ggplot2)
# Load stringr for string manipulation functions.
library(stringr)
# Load purrr for functional programming tools.
library(purrr)
# Load janitor for data cleaning and examination functions.
library(janitor)
# Load scales for tools to control how data values are mapped to visual properties.
library(scales)
# Load units for handling and converting units (e.g., kilometers).
library(units)
# Load terra for raster and vector spatial data manipulation (often used with sf).
library(terra)

# Source custom functions and lookup tables from 'R/funs' directory.
source(here("R/funs", "00_funs.R"))        # General utility functions
source(here("R/funs", "province_map.R"))   # Province name mappings
source(here("R/funs", "geneAbbr.R"))       # Function to generate city abbreviations


# Define Custom Function: calculate_nearest_harbor_distance ----------------
#' Calculates the distance from each city to the nearest harbor.
#'
#' This function efficiently calculates the shortest distance from each city
#' to any given harbor in a set of harbors, and identifies the nearest harbor.
#' It handles cases where a city itself is a harbor.
#'
#' @param cities_sf An sf data frame of city centroids (or other geometries).
#'                  Must have 'citychn' column for name matching.
#' @param harbors_sf An sf data frame of harbor centroids (or other geometries).
#'                   Must have 'citychn' column for name matching.
#' @param dist_col_name The desired name for the new column storing the distance
#'                      to the nearest harbor (e.g., "distance_to_harbor").
#' @param port_col_name The desired name for the new column storing the name
#'                      of the nearest port (e.g., "nearest_port").
#'
#' @return The input `cities_sf` sf data frame with two new columns:
#'         `dist_col_name` (distance in kilometers) and `port_col_name` (nearest port name).
#'         The distances are of class 'units'.
calculate_nearest_harbor_distance <- function(cities_sf, harbors_sf, dist_col_name, port_col_name) {
  # Initialize new columns in the cities_sf data frame with default NA values.
  # Distances are initialized as Inf with "km" units.
  cities_sf[[dist_col_name]] <- set_units(NA_real_, "km")
  cities_sf[[port_col_name]] <- NA_character_

  # Identify cities that are themselves present in the harbors_sf list.
  # For these, the distance to the nearest harbor is 0.
  is_harbor_city <- cities_sf$citychn %in% harbors_sf$citychn

  # For cities that are harbors, set their distance to 0 and their nearest port to themselves.
  cities_sf[[dist_col_name]][is_harbor_city] <- units::set_units(0, "km")
  cities_sf[[port_col_name]][is_harbor_city] <- cities_sf$citychn[is_harbor_city]

  # Process non-harbor cities separately for distance calculation.
  non_harbor_cities <- cities_sf[!is_harbor_city, ]

  # Only proceed if there are cities that are not themselves harbors.
  if (nrow(non_harbor_cities) > 0) {
    # Calculate the distance matrix between all non-harbor cities and all harbors.
    # st_distance returns a matrix where rows are from non_harbor_cities and columns are from harbors_sf.
    distances_matrix <- st_distance(non_harbor_cities$geometry, harbors_sf$geometry)

    # For each row (city) in the distances_matrix, find the minimum distance.
    min_distances_values <- apply(distances_matrix, 1, min)
    # For each row (city), find the column index of the minimum distance (i.e., the nearest harbor).
    nearest_harbor_indices <- apply(distances_matrix, 1, which.min)
    # Use these indices to get the names of the nearest harbors from the harbors_sf data frame.
    nearest_harbor_names <- harbors_sf$citychn[nearest_harbor_indices]

    # Assign the calculated minimum distances and nearest port names back to the
    # corresponding non-harbor rows in the original cities_sf data frame.
    cities_sf[[dist_col_name]][!is_harbor_city] <- min_distances_values
    cities_sf[[port_col_name]][!is_harbor_city] <- nearest_harbor_names
  }

  return(cities_sf)
}


# Prepare Port Information ------------------------------------------------
message("\n--- Preparing Port Information ---\n")

# Port info source: https://www.port-m.com/mobile/news/detail/200806/1119.html
data_string <- "1.沿海合计	万吨	      6,142	     64,530	      118.3
大连	万吨	        379	      3,484	      123.5
营口	万吨	         56	      1,022	      109.5
秦皇岛	万吨	        388	      4,596	      102.1
天津	万吨	        613	      6,444	      112.5
烟台	万吨	         95	      1,253	      126.7

青岛	万吨	        706	      7,587	      116.8
日照	万吨	        175	      1,775	      114.6
上海	万吨	        990	      9,619	      126.9
连云港	万吨	        120	      1,861	      103.5
宁波	万吨	        489	      5,578	      112.6

舟山	万吨	        146	        754	      135.4
福州	万吨	        140	      1,256	      133.7
泉州	万吨	         44	        443	       86.2
厦门	万吨	        150	      1,618	      126.7
深圳	万吨	        468	      4,620	      142.5

广州	万吨	        305	      3,780	      121.0
湛江	万吨	        186	      1,615	      129.3"

# Read port data from string and clean it
tb_port <- read_tsv(data_string,
                    col_names = FALSE,
                    comment = "#",
                    col_types = "ccnnn",
                    skip_empty_rows = TRUE) %>%
    mutate(X1 = gsub("^1\\.", "", X1)) %>%
    rename(
        port = X1,
        unit = X2,
        amount_nov = X3,
        amount_year = X4,
        growth = X5
    ) |>
    mutate(citychn = if_else(port == "沿海合计", port, paste0(port, "市"))) |> 
    filter(citychn != "沿海合计") |> 
    mutate(amount_share = amount_year/sum(amount_year))


message("Port data loaded and cleaned successfully.\n")


# Load and Process Map Data -----------------------------------------------
message("--- Loading and Processing China Map Data ---\n")

# Load city code data (likely for specific year and city names).
# show_col_types = FALSE suppresses messages about column specifications.
city_code <-
    read_csv(
        here("data-raw", "tidy_citycode_1990_2019.csv"),
        col_select = c("cityid", "year", "citychn", "province"),
        show_col_types = FALSE
    ) |>
    # Filter for data specific to year 2002.
    filter(year == 2002)

# Read China map sf data from an RDS file.
# terra::unwrap() is used to convert a wrapped SpatVector to a plain SpatVector.
# sf::st_as_sf() converts a SpatVector to an sf object.
china_map <-
  readRDS(here("data-raw", "gadm", "gadm41_CHN_2_pk.rds")) |> # Standardize path with here()
  terra::unwrap() |>
  sf::st_as_sf()

# Standardize province names using a predefined mapping (from province_map.R).
# Filter out any regions where province name standardization failed (NA province).
china_map <- china_map |>
    mutate(
        province = dplyr::recode(NL_NAME_1, !!!chinese_to_standard_chinese_map, .missing = NA_character_),
        province_en = dplyr::recode(province, !!!province_name_map, .missing = NA_character_)
    ) |>
    filter(!is.na(province))

# Clean and standardize city names (NL_NAME_2) into 'citychn'.
# This involves complex string manipulations and conditional remapping
# to ensure consistency with other datasets.
china_map <- china_map |>
    mutate(
        # Extract the primary city name, handling cases with "|".
        temp_city_name = purrr::map_chr(NL_NAME_2, ~ {
            if (is.na(.x)) return(NA_character_)
            parts <- str_split(.x, pattern = "\\|", simplify = TRUE)
            if (ncol(parts) > 1 && parts[2] != "") {
                return(str_trim(parts[2]))
            } else {
                return(str_trim(parts[1]))
            }
        }),
        # Apply extensive case-based cleaning and standardization for city names.
        citychn = case_when(
            temp_city_name == "巴音郭愣蒙古自治州" ~ "巴音郭楞蒙古自治州",
            str_detect(temp_city_name, "婁底市")~ "娄底市",
            str_detect(temp_city_name, "岳陽市")~ "岳阳市",
            str_detect(temp_city_name, "張家界市")~ "张家界市",
            str_detect(temp_city_name, "懷化市")~ "怀化市",
            str_detect(temp_city_name, "益陽市")~ "益阳市",
            str_detect(temp_city_name, "衡陽市")~ "衡阳市",
            str_detect(temp_city_name, "邵陽市")~ "邵阳市",
            str_detect(temp_city_name, "長沙市")~ "长沙市",
            temp_city_name == "滨州" ~ "滨州市",
            temp_city_name == "运城县" ~ "运城市",
            temp_city_name == "海南" ~ "海南省直辖县级行政区划", # Special case for Hainan
            temp_city_name %in% c("仙桃市", "天门市", "潜江市", "神农架林区") ~ temp_city_name, # Direct admin counties
            # Handle specific "地区" conversions to "市"
            temp_city_name %in% c("毕节地区", "铜仁地区", "吐鲁番地区", "哈密地区", "林芝地区",
                               "昌都地区", "日喀则地区", "山南地区", "那曲地区", "海东地区") ~ paste0(str_remove(temp_city_name, "地区"), "市"),
            temp_city_name == "襄樊市" ~ "襄阳市", # Historical name change
            # Direct municipalities
            temp_city_name %in% c("上海", "北京", "天津", "重庆") ~ paste0(temp_city_name, "市"),
            # Append "市", "州", "盟", "区" if not already present, based on common patterns
            str_detect(temp_city_name, "市$|州$|盟$|区$") ~ temp_city_name,
            # Default: append "市" if not detected in name and not NA
            !is.na(temp_city_name) ~ paste0(temp_city_name, "市"),
            TRUE ~ NA_character_ # Catch any remaining unhandled cases as NA
        )
    ) |>
    # Remove the temporary city name column.
    select(-temp_city_name)

message("China map data loaded and city names standardized.\n")

# --- Check City Name Cleaning Results ---
message("--- Comparison of NL_NAME_2 (Original) and citychn (Standardized) (Sample) ---\n")
china_map |>
    select(NL_NAME_2, citychn) |>
    slice_sample(n = 20) # Sample 20 rows for manual inspection

message("\n--- Duplicate citychn entries and their counts ---\n")
china_map |>
    get_dupes(citychn) # Check for duplicates in the standardized city names


# Identify any original `NL_NAME_2` values that failed to standardize (resulted in NA `citychn`).
unstandardized_values <- china_map |>
    filter(is.na(citychn) & !is.na(NL_NAME_2)) |>
    pull(NL_NAME_2) |>
    unique()

if (length(unstandardized_values) > 0) {
    message("\n--- Warning: The following NL_NAME_2 original values failed to standardize (resulted in NA citychn) ---\n")
    print(unstandardized_values)
} else {
    message("\nAll NL_NAME_2 values have been successfully standardized or were initially NA.\n")
}

# Merge geographic regions to create unified city geometries.
# Group by province and standardized city name, then union their geometries.
merged_sf_city <- china_map |>
    group_by(province, citychn) |>
    select(province, citychn, geometry) |>
    summarise(geometry = st_union(geometry), .groups = "drop")

message("Geographic regions merged to unified city geometries.\n")


# Calculate Distances to Nearest Harbor -----------------------------------
message("--- Calculating Distances to Nearest Harbors ---\n")

# Get city centroids (point representation for distance calculations).
city_centroids <- st_centroid(merged_sf_city)

# Manually add Yingkou's centroid if it's not correctly represented in the map data.
# This ensures its inclusion for harbor distance calculations.
yingkou_centroid <- st_sf(
  province = "辽宁省",
  citychn = "营口市",
  geometry = st_sfc(st_point(c(122.25, 40.67)), crs = 4326) # WGS 84 CRS
)

# Combine the main city centroids with the manually added Yingkou centroid.
city_centroids <- rbind(city_centroids, yingkou_centroid)

# Convert filtered port data to sf object and get their centroids for distance calculation.
# Join with city_centroids to get the spatial data for the ports.
ports_sf <- tb_port %>%
    filter(citychn != "沿海合计") %>% # Ensure "沿海合计" is excluded
    inner_join(city_centroids, by = "citychn") %>%
    select(port, citychn, geometry)

# Select "big ports" based on their annual amount share (>= 5%).
# Join with city_centroids to get the spatial data for these big ports.
big_ports_sf <- tb_port %>%
    filter(amount_share >= 0.05) %>%
    inner_join(city_centroids, by = "citychn") %>%
    select(port, citychn, geometry)

# Calculate distance to all harbors using the optimized custom function.
all_cities_with_distance <- calculate_nearest_harbor_distance(
  cities_sf = city_centroids,
  harbors_sf = ports_sf,
  dist_col_name = "distance_to_harbor",
  port_col_name = "nearest_port"
) %>%
  # Convert distance units to numeric for log transformation and remove 'units' class.
  # Apply log transformation (log(x+1)) to distance for skewed distributions.
  mutate(
    distance_to_harbor = as.numeric(distance_to_harbor),
    lndis_to_harbor = log(distance_to_harbor + 1)
  ) %>%
  # Convert sf object to a regular tibble for simpler data manipulation if spatial features are no longer needed.
  tibble()

# Calculate distance to big harbors using the optimized custom function.
# This reuses the 'all_cities_with_distance' data frame and adds new distance columns.
all_cities_with_distance <- calculate_nearest_harbor_distance(
  cities_sf = all_cities_with_distance,
  harbors_sf = big_ports_sf,
  dist_col_name = "distance_to_bharbor", # Name for distance to big harbor
  port_col_name = "nearest_bport"        # Name for nearest big port
) %>%
  # Convert distance units to numeric and apply log transformation.
  mutate(
    distance_to_bharbor = as.numeric(distance_to_bharbor),
    lndis_to_bharbor = log(distance_to_bharbor + 1)
  ) %>%
  # Ensure it remains a tibble.
  tibble()

message("Distances to all and big harbors calculated successfully.\n")


# Final Data Cleaning and Preparation -------------------------------------
# Generate city abbreviations using the 'geneAbbr' function.
# Remove original city name and geometry columns as they are no longer needed.
# Reorder columns to place 'cityabbr' at the very beginning.
all_cities_with_distance <-
  all_cities_with_distance |>
  mutate(cityabbr = geneAbbr(citychn)) |>
  select(-citychn, -geometry) |> # Remove original city name and geometry
  select(cityabbr, everything())   # Reorder columns to put cityabbr first

all_cities_with_distance <- all_cities_with_distance |> 
  mutate(distance_to_harbor = units::set_units(distance_to_harbor/1000, "km"))
# Save Final Data ---------------------------------------------------------
message("--- Saving Final Processed Data ---\n")
# Write the final cleaned and processed data to a CSV file.
write_excel_csv(all_cities_with_distance,
                file = here("data", "all_cities_with_distance.csv"))

message(paste0("Final processed data saved to: ", here("data", "all_cities_with_distance.csv"), "\n"))

save(merged_sf_city, file = here("data/merged_sf_city.Rdata"))
