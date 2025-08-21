# Load necessary packages -------------------------------------------------------------------
# If you haven't installed these packages, uncomment the following lines and run the install commands:
# install.packages("sfnetworks")
# install.packages("sf")
# install.packages("dplyr")
# install.packages("tidygraph")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("readr")
# install.packages("here")
# install.packages("purrr")
# install.packages("units")
# install.packages("future") # Added: underlying framework for parallel computing
# install.packages("furrr")  # Added: furrr package provides parallel versions of purrr functions

library(sfnetworks)
library(sf)
library(dplyr)
library(tidygraph)
library(ggplot2)
library(readxl)
library(readr)
library(here)
library(purrr)
library(units) # Used for handling and displaying distance units
library(future) # Introduced future package for setting parallel strategy
library(furrr)  # Introduced furrr package, provides parallel versions of purrr functions
library(janitor)
library(skimr)

# funs -------------------------------------------------------------------

source(here("R/funs/calculate_network_distance.R"))

source(here("R/funs/create_transfer_edges.R"))

source(here("R/funs/assign_points_to_cities.R"))

source(here("R/funs/create_bimodal_network.R"))
# import data ------------------------------------------------------------
load("data/city_port_combinations.Rdata")

set.seed(1234)

test_city <- unique(city_port_combinations$Pftn)[sample(length(unique(city_port_combinations$Pftn)), 10)]

test_combinations <- 
  city_port_combinations |> 
  filter(Pftn %in% test_city)


## prefecture level area `POLYGON` or `MULTIPOLYGON`
load("data/merged_sf_city.Rdata")

## maps in different type ------------------------------------------------

# Read the new 2000 map files (with provincial roads)
shapefile_paths_new <- here::here("data-raw/2000-map", c("hiway.shp", "rail.shp", "gd.shp", "sd.shp"))

# Read all four transportation network types
highway_map_new <- st_read(shapefile_paths_new[1]) |> mutate(type = "高速公路")
railway_map_new <- st_read(shapefile_paths_new[2]) |> mutate(type = "铁路") 
national_road_map <- st_read(shapefile_paths_new[3]) |> mutate(type = "国道")
provincial_road_map <- st_read(shapefile_paths_new[4]) |> mutate(type = "省道")


waterway_map <- st_read(here("data-raw/2020-Waterway/Waterwaysingleline.shp"))


# Standardize column names across all datasets
# First inspect what columns each dataset has
cat("--- Highway columns: --- \n", names(highway_map_new))
cat("--- Railway columns: --- \n", names(railway_map_new))
cat("--- National road columns: --- \n", names(national_road_map))
cat("--- Provincial road columns: --- \n", names(provincial_road_map))


# unified_railway_highway <- 
#   create_bimodal_network(
#     map1 = railway_map_new,
#     map2 = highway_map_new,
#     mode1_name = "railway",
#     mode2_name = "highway",
#     cities_sf = merged_sf_city,
#     max_transfer_distance = 100000
#   )


## Traffic Data (2000 Railway, Highway, National Road Maps) ---------------------------------------------------


# Set the directory path containing the Shapefile files
# Please adjust the path according to your actual project structure
shapefile_dir <- "data-raw/2000年道路交通图"

# Define the main Shapefile file names (usually .shp files)
# Assuming main file names are '铁路_polyline.shp', '高速_polyline.shp', '国道_polyline.shp'
shapefile_names <- c("铁路_polyline.shp", "高速_polyline.shp", "国道_polyline.shp")

# Build the full Shapefile file paths
full_shapefile_paths <- here::here(shapefile_dir, shapefile_names)

# Check if files exist
if (!all(file.exists(full_shapefile_paths))) {
  stop(paste("Error: One or more Shapefile files not found. Please ensure all .shp files and their accompanying files (.dbf, .shx, etc.) are located in the specified directory.", 
             "\nMissing files:", paste(full_shapefile_paths[!file.exists(full_shapefile_paths)], collapse = ", ")))
}

# Resolve Chinese garbled characters in Shapefile: Set SHAPE_ENCODING environment variable before reading
# For Chinese Shapefiles, common encodings are GBK or GB2312
Sys.setenv("SHAPE_ENCODING" = "GBK") 

# Read individual Shapefile files
# st_read() automatically identifies accompanying files like .dbf, .shx in the same directory
railway_map_2000 <- st_read(full_shapefile_paths[[1]]) |> mutate(type = "铁路")

highway_map_2000 <- st_read(full_shapefile_paths[[2]]) |> mutate(type = "高速")

nationroad_map_2000 <- st_read(full_shapefile_paths[[3]]) |> mutate(type = "国道")


# Ensure all maps have consistent columns before combining. 
# We'll select common columns and geometry, and let sf handle the binding.
# Adjust column selection based on your specific needs; here, 'NAME', 'CODE', 'type', and geometry are common.
common_cols <- c("NAME", "CODE", "type") # Assuming NAME and CODE are present or can be made consistent

# Select and rename columns if necessary for each map
# For `nationroad_map_2000`, if 'QUES' column is not needed, it will be dropped by select.
railway_map_2000_processed <- railway_map_2000 %>% select(all_of(common_cols), geometry)
highway_map_2000_processed <- highway_map_2000 %>% select(all_of(common_cols), geometry)
nationroad_map_2000_processed <- nationroad_map_2000 %>% select(all_of(common_cols), geometry)


# Combine all three traffic maps into a single sf object
# st_crs(railway_map_2000) will be used as the CRS for the combined map.
# It's crucial that all input layers have the same CRS or are transformed to a common one beforehand if different.
traffic_map_2000 <- rbind(railway_map_2000_processed, highway_map_2000_processed)
# Round coordinates of traffic_map_2000 to 5 decimal places to improve network connectivity
# This helps in snapping slightly misaligned endpoints to common nodes
# st_geometry(traffic_map_2000) <- st_geometry(traffic_map_2000) %>%
#   lapply(function(x) round(x, 1)) %>% # Round to 5 decimal places (adjust as needed)
#   st_sfc(crs = st_crs(traffic_map_2000))

st_geometry(highway_map_2000_processed) <- st_geometry(highway_map_2000_processed) %>%
  lapply(function(x) round(x, 2)) %>% # Round to 5 decimal places (adjust as needed)
  st_sfc(crs = st_crs(highway_map_2000_processed))


sf_network <- as_sfnetwork(traffic_map_2000, directed = FALSE) %>%
      activate("edges") %>%
      mutate(edge_length = st_length(geometry))

# Print data overview
print("Combined Railway Map Overview:")
print(head(railway_map_2000))

print("Combined Highway Map Overview:")
print(head(highway_map_2000))

print("Combined National Road Map Overview:")
print(head(nationroad_map_2000))

print("Combined Traffic Map (traffic_map_2000) Overview:")
print(head(traffic_map_2000))


# Print CRS information for the combined data
print("CRS information for the combined traffic map (traffic_map_2000):")
print(st_crs(traffic_map_2000))


calculate_shortest_distance <- function(city_port_combinations, map){
  # 2. Apply distance calculation function for each city-port pair (parallel computing)
# Use future_pmap from furrr package to replace rowwise() and mutate() for parallelization
# plan(multisession) will create multiple R sessions on your macOS to execute tasks in parallel
plan(multisession) 

calculated_distances <- city_port_combinations %>% 
  # filter(Pftn == "安庆") %>% # For testing, uncomment or delete if not needed
  # Use future_pmap to apply calculate_network_distance function to each row in parallel
  mutate(
    distance_results = future_pmap(
      .l = list(.$city_geometry, .$port_geometry), # Pass the columns to iterate over
      .f = ~ calculate_network_distance(map, ..1, ..2), # Define the function to apply and its arguments
      .options = furrr_options(seed = TRUE) # Ensure reproducibility if the function has random operations internally
    )
  ) %>%
  # Expand distance_results (a list-column where each element is a tibble) into new columns
  # This requires manually extracting each component from the tibble
  mutate(
    p1_to_node = map_dbl(distance_results, ~ as.numeric(.$p1_to_node)),
    node_to_node = map_dbl(distance_results, ~ as.numeric(.$node_to_node)),
    node_to_p2 = map_dbl(distance_results, ~ as.numeric(.$node_to_p2)),
    # Calculate total distance
    total_distance_km = p1_to_node + node_to_node + node_to_p2 
  ) %>%
  # Clean up intermediate columns, keep only the required final results
  select(-distance_results, -p1_to_node, -node_to_node, -node_to_p2)



# 3. Find the shortest distance for each city and its corresponding port
result_summary <- calculated_distances %>%
  group_by(Ctnb, Ubclssz, Prvn, Pftn, Cont) %>% # Group by city
  # Find the row with the minimum total_distance_km within each city group
  filter(total_distance_km == min(total_distance_km, na.rm = TRUE)) %>%
  # If multiple ports have the same minimum distance, keep only the first one
  distinct(Ctnb, Ubclssz, Prvn, Pftn, Cont, .keep_all = TRUE) %>%
  select(Ctnb, Ubclssz, Prvn, Pftn, Cont, min_total_distance_km = total_distance_km, nearest_port_name)
  # Summary statistics
  success_rate <- sum(!is.infinite(result_summary$min_total_distance_km) & !is.na(result_summary$min_total_distance_km)) / nrow(result_summary)
  avg_distance <- mean(result_summary$min_total_distance_km[!is.infinite(result_summary$min_total_distance_km)], na.rm = TRUE)
    
  cat("✓ Success! Success rate:", round(success_rate * 100, 1), "%, Avg distance:", round(avg_distance, 2), "km\n")
    
}


calculate_shortest_distance(test_combinations, railway_map_2000)

calculate_shortest_distance(test_combinations, railway_map_new)

calculate_shortest_distance(
  test_combinations,
  create_bimodal_network(
    railway_map_new,
    highway_map_new,
    mode1_name = "railway",
    mode2_name = "highway",
    cities_sf = merged_sf_city
  )
)





# Generate all possible map combinations with constraints
generate_map_combinations <- function() {
  
  # Define all available maps with their corresponding mode names
  all_maps <- list(
    # 2000-map source (new)
    railway_map_new = list(map = railway_map_new, mode = "railway", source = "new"),
    highway_map_new = list(map = highway_map_new, mode = "highway", source = "new"), 
    national_road_map = list(map = national_road_map, mode = "national_road", source = "new"),
    provincial_road_map = list(map = provincial_road_map, mode = "provincial_road", source = "new"),
    
    # 2000年道路交通图 source (2000)
    railway_map_2000 = list(map = railway_map_2000, mode = "railway", source = "2000"),
    highway_map_2000 = list(map = highway_map_2000_processed, mode = "highway", source = "2000"),
    national_map_2000 = list(map = nationroad_map_2000_processed, mode = "national_road", source = "2000"),
    
    # Unique source
    waterway_map = list(map = waterway_map, mode = "waterway", source = "unique")
  )
  
  # Generate all possible combinations
  map_names <- names(all_maps)
  combinations <- expand.grid(
    map1_name = map_names,
    map2_name = map_names,
    stringsAsFactors = FALSE
  ) %>%
    # Remove same map combinations
    filter(map1_name != map2_name) %>%
    # Add map and mode information
    mutate(
      map1_source = map_chr(map1_name, ~ all_maps[[.]]$source),
      map2_source = map_chr(map2_name, ~ all_maps[[.]]$source),
      map1_mode = map_chr(map1_name, ~ all_maps[[.]]$mode),
      map2_mode = map_chr(map2_name, ~ all_maps[[.]]$mode)
    )
  
  # Apply constraints
  valid_combinations <- combinations %>%
    filter(
      # Constraint 1: Same transport type from different sources can't be combined
      !(map1_mode == "railway" & map2_mode == "railway" & map1_source != map2_source),
      !(map1_mode == "highway" & map2_mode == "highway" & map1_source != map2_source),
      !(map1_mode == "national_road" & map2_mode == "national_road" & map1_source != map2_source),
      
      # Constraint 2: 2000 source maps can be combined with provincial_road_map
      # This is automatically allowed by the structure
      
      # Constraint 3: All other maps can be combined with waterway_map
      # This is automatically allowed by the structure
    ) %>%
    # Add actual map objects
    mutate(
      map1 = map(map1_name, ~ all_maps[[.]]$map),
      map2 = map(map2_name, ~ all_maps[[.]]$map),
      mode1_name = map1_mode,
      mode2_name = map2_mode,
      combination_id = paste(map1_name, map2_name, sep = "_vs_")
    ) %>%
    select(combination_id, map1_name, map2_name, mode1_name, mode2_name, map1, map2)
  
  return(valid_combinations)
}

# Generate all valid combinations
map_combinations <- generate_map_combinations()

cat("Generated", nrow(map_combinations), "valid map combinations:\n")
print(map_combinations %>% select(combination_id, map1_name, map2_name, mode1_name, mode2_name))


# Function to test a single map combination
test_single_combination <- function(combination_row, test_data, cities_sf, max_transfer_distance = 100000) {
  
  cat("\n=== Testing combination:", combination_row$combination_id, "===\n")
  
  tryCatch({
    # Create bimodal network
    bimodal_network <- create_bimodal_network(
      map1 = combination_row$map1[[1]], 
      map2 = combination_row$map2[[1]], 
      mode1_name = combination_row$mode1_name, 
      mode2_name = combination_row$mode2_name, 
      cities_sf = cities_sf, 
      max_transfer_distance = max_transfer_distance
    )
    
    # Calculate distances using the bimodal network
    plan(multisession)
    
    calculated_distances <- test_data %>%
      slice_head(n = 5) %>%  # Test with first 5 rows for speed
      mutate(
        distance_results = future_pmap(
          .l = list(city_geometry, port_geometry),
          .f = ~ calculate_network_distance(bimodal_network, ..1, ..2),
          .options = furrr_options(seed = TRUE)
        )
      ) %>%
      mutate(
        p1_to_node = map_dbl(distance_results, ~ as.numeric(.$p1_to_node)),
        node_to_node = map_dbl(distance_results, ~ as.numeric(.$node_to_node)),
        node_to_p2 = map_dbl(distance_results, ~ as.numeric(.$node_to_p2)),
        total_distance_km = p1_to_node + node_to_node + node_to_p2
      ) %>%
      select(-distance_results, -p1_to_node, -node_to_node, -node_to_p2)
    
    # Get results similar to result_df_vectorized format
    result_summary <- calculated_distances %>%
      group_by(Ctnb, Ubclssz, Prvn, Pftn, Cont) %>%
      filter(total_distance_km == min(total_distance_km, na.rm = TRUE)) %>%
      distinct(Ctnb, Ubclssz, Prvn, Pftn, Cont, .keep_all = TRUE) %>%
      select(Ctnb, Ubclssz, Prvn, Pftn, Cont, min_total_distance_km = total_distance_km, nearest_port_name) %>%
      mutate(combination_id = combination_row$combination_id)
    
    # Summary statistics
    success_rate <- sum(!is.infinite(result_summary$min_total_distance_km) & !is.na(result_summary$min_total_distance_km)) / nrow(result_summary)
    avg_distance <- mean(result_summary$min_total_distance_km[!is.infinite(result_summary$min_total_distance_km)], na.rm = TRUE)
    
    cat("✓ Success! Success rate:", round(success_rate * 100, 1), "%, Avg distance:", round(avg_distance, 2), "km\n")
    
    return(list(
      success = TRUE,
      combination_id = combination_row$combination_id,
      success_rate = success_rate,
      avg_distance = avg_distance,
      result_data = result_summary
    ))
    
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n")
    return(list(
      success = FALSE,
      combination_id = combination_row$combination_id,
      error_message = e$message
    ))
  })
}

# Test all combinations
cat("Testing all combinations with test dataset...\n")
cat("Test dataset has", nrow(test_combinations), "city-port pairs\n")

# Run tests for all combinations
all_test_results <- map_dfr(1:nrow(map_combinations), function(i) {
  result <- test_single_combination(
    map_combinations[i, ], 
    test_combinations, 
    merged_sf_city, 
    max_transfer_distance = 100000
  )
  
  # Convert to data frame for binding
  data.frame(
    combination_id = result$combination_id,
    success = result$success,
    success_rate = if(result$success) result$success_rate else NA,
    avg_distance = if(result$success) result$avg_distance else NA,
    error_message = if(!result$success) result$error_message else NA,
    stringsAsFactors = FALSE
  )
})

# Summary of all test results
cat("\n=== OVERALL TEST SUMMARY ===\n")
print("Success rate by combination:")
print(all_test_results %>% 
  arrange(desc(success_rate)) %>%
  select(combination_id, success, success_rate, avg_distance))

# Best performing combinations
successful_combinations <- all_test_results %>%
  filter(success == TRUE) %>%
  arrange(desc(success_rate), avg_distance)

if (nrow(successful_combinations) > 0) {
  cat("\n=== TOP 5 BEST PERFORMING COMBINATIONS ===\n")
  print(head(successful_combinations, 5))
} else {
  cat("\n❌ No combinations were successful\n")
}

# Failed combinations
failed_combinations <- all_test_results %>%
  filter(success == FALSE)

if (nrow(failed_combinations) > 0) {
  cat("\n=== FAILED COMBINATIONS ===\n")
  print(failed_combinations %>% select(combination_id, error_message))
}

# Detailed analysis of the best combination
if (nrow(successful_combinations) > 0) {
  best_combination_id <- successful_combinations$combination_id[1]
  cat("\n=== DETAILED ANALYSIS OF BEST COMBINATION:", best_combination_id, "===\n")
  
  # Get the best combination details
  best_combo <- map_combinations %>% 
    filter(combination_id == best_combination_id)
  
  # Run full test on best combination
  detailed_result <- test_single_combination(
    best_combo, 
    test_combinations,  # Use full test dataset
    merged_sf_city,
    max_transfer_distance = 100000
  )
  
  if (detailed_result$success) {
    print("Detailed results for best combination:")
    print(detailed_result$result_data)
  }
}