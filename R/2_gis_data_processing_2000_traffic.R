# Load necessary packages
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


# parameters -------------------------------------------------------------

# Define heuristic mapping between provinces and ports (for optimization)
# This mapping table aims to reduce computation by initially judging which ports might be associated with specific provinces.
# If a province is not in the explicit list, all export_port will be considered by default.
province_port_heuristic_map <- list(
  # North China
  "北京" = c("天津", "秦皇岛"),
  "天津" = c("天津", "秦皇岛"),
  "河北" = c("秦皇岛", "天津"),
  "山西" = c("天津", "秦皇岛", "青岛"),
  "内蒙古" = c("天津", "秦皇岛", "大连"),
  
  # Northeast China
  "辽宁" = c("大连", "营口", "秦皇岛"),
  "吉林" = c("大连", "营口"),
  "黑龙江" = c("大连", "营口"),
  
  # East China
  "上海" = c("上海", "宁波", "舟山"),
  "江苏" = c("连云港", "上海"),
  "浙江" = c("宁波", "舟山", "上海", "福州"),
  "安徽" = c("上海", "宁波", "连云港"),
  "福建" = c("福州", "泉州", "厦门", "宁波"),
  "山东" = c("烟台", "青岛", "日照"),
  
  # Central China
  "河南" = c("青岛", "上海", "连云港", "天津"),
  "湖北" = c("上海", "宁波", "广州", "湛江"),
  "湖南" = c("广州", "深圳", "上海", "湛江"),
  "江西" = c("福州", "泉州", "厦门", "宁波", "上海"),
  
  # South China
  "广东" = c("深圳", "广州", "湛江"),
  "广西" = c("湛江", "广州"),
  "海南" = c("湛江", "深圳", "广州"),
  
  # Southwest China
  "重庆" = c("上海", "广州", "深圳", "湛江"),
  "四川" = c("上海", "广州", "深圳", "湛江", "重庆"),
  "贵州" = c("广州", "深圳", "湛江"),
  "云南" = c("湛江", "广州", "深圳"),
  "西藏" = c("湛江", "广州", "深圳"),
  
  # Northwest China
  "陕西" = c("天津", "青岛", "上海", "广州"),
  "甘肃" = c("天津", "青岛", "上海", "广州"),
  "青海" = c("天津", "青岛", "上海", "广州"),
  "宁夏" = c("天津", "青岛", "上海", "广州"),
  "新疆" = c("天津", "青岛", "上海", "广州"),
  
  # Hong Kong, Macao, Taiwan
  "台湾" = c("福州", "泉州", "厦门"),
  "香港" = c("深圳", "广州"),
  "澳门" = c("广州", "深圳", "湛江")
)

# import data ------------------------------------------------------------

## prefecture level area `POLYGON` or `MULTIPOLYGON`
load("data/merged_sf_city.Rdata")

## Longitude and Latitude - Cities -----------------------------------------------------------------

location_city <- 
  read_excel(here::here("data-raw/城市经纬度/城市经纬度.xlsx")) |> 
  slice(-1) 

# Convert Ltd and Latd columns to numeric type, which is required by st_as_sf
location_city <- location_city %>%
  mutate(
    Ltd = as.numeric(Ltd),
    Latd = as.numeric(Latd)
  )

# Filter to keep only city rows where Pftn (prefecture/city) is the same as Cont (county/district), 
# effectively keeping only primary city records if needed.
location_city <- location_city |> 
  filter(Pftn == Cont) |> 
  filter(!Prvn %in% c("海南", "西藏", "澳门", "香港", "台湾"))

# Use st_as_sf() to generate an sf object
# coords parameter specifies longitude and latitude columns
# crs parameter sets the coordinate reference system to WGS84 (EPSG:4326)
sf_cities <- st_as_sf(location_city, coords = c("Ltd", "Latd"), crs = 4326)

# Print the generated sf object
print("City Data Overview:")
print(sf_cities)

## Longitude and Latitude - Ports -----------------------------------------------------------------

location_port <- 
  read_csv(here::here("data/all_ports_data.csv")) %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  ) 

# Define export port list
export_port <- c("大连", "营口", "秦皇岛",
                "天津", "烟台", "青岛", "日照", "上海", "连云港", 
                "宁波", "舟山", "福州", "泉州", "厦门", 
                "深圳", "广州", "湛江")

# Construct regular expression to match if region contains any port name from export_port (not exact match)
# Use paste(..., collapse = "|") to join port names with "|" for "OR" logic
# iconv(..., to = "UTF-8") ensures correct character encoding handling, avoiding Chinese garbled characters
# fixed = TRUE if port names contain regex special characters, needs to be set to TRUE
export_port_pattern <- paste(export_port, collapse = "|")
export_port_pattern <- iconv(export_port_pattern, to = "UTF-8") # Ensure correct encoding

# Filter location_port, keep only rows where region contains port names from export_port
# Note: Here it is assumed you want to create sf_port after filtering
location_port_filtered <- location_port %>%
  filter(grepl(export_port_pattern, region, ignore.case = TRUE)) %>% # ignore.case = TRUE ignores case
  filter(main_port) # Continue filtering for main ports; remove this line if not needed

# Use the filtered data to create sf_port object
sf_port <- st_as_sf(location_port_filtered, coords = c("longitude", "latitude"), crs = 4326)

print("Filtered Port Data Overview:")
print(sf_port)


## maps in different type ------------------------------------------------

# Read the new 2000 map files (with provincial roads)
shapefile_paths_new <- here::here("data-raw/2000-map", c("hiway.shp", "rail.shp", "gd.shp", "sd.shp"))

# Read all four transportation network types
highway_map_new <- st_read(shapefile_paths_new[1]) |> mutate(type = "高速公路")
railway_map_new <- st_read(shapefile_paths_new[2]) |> mutate(type = "铁路") 
national_road_map <- st_read(shapefile_paths_new[3]) |> mutate(type = "国道")
provincial_road_map <- st_read(shapefile_paths_new[4]) |> mutate(type = "省道")

# Read water way network types
waterway_map <- st_read(here("data-raw/2020-Waterway/Waterwaysingleline.shp"))


# Standardize column names across all datasets
# First inspect what columns each dataset has
cat("--- Highway columns: --- \n", names(highway_map_new))
cat("--- Railway columns: --- \n", names(railway_map_new))
cat("--- National road columns: --- \n", names(national_road_map))
cat("--- Provincial road columns: --- \n", names(provincial_road_map))


unified_railway_highway <- 
  create_bimodal_network(
    map1 = railway_map_new,
    map2 = highway_map_new,
    mode1_name = "railway",
    mode2_name = "highway",
    cities_sf = merged_sf_city,
    max_transfer_distance = 100000
  )


unified_railway_highway <- 
  create_bimodal_network(
    map1 = railway_map_new,
    map2 = waterway_map,
    mode1_name = "railway",
    mode2_name = "waterway",
    cities_sf = merged_sf_city,
    max_transfer_distance = 100000
  )


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



# generate city-port combinations ----------------------------------------


#' Helper function: Get allowed port list based on province name
#' @param province_name Province name (character)
#' @return Vector of allowed port names (character)
get_allowed_ports <- function(province_name) {
  # If province is in the mapping table, return its corresponding port list
  if (province_name %in% names(province_port_heuristic_map)) {
    return(province_port_heuristic_map[[province_name]])
  } else {
    # If province is not explicitly mapped, return all export_port to ensure no omission (but increases computation)
    # Alternatively, a default subset containing major ports can be returned to further limit computation
    # For safety, all export_port are returned here
    return(export_port) 
  }
}


# Calculate the shortest distance from each city to all ports (vectorized/tidyverse style) ----------------------------

# 1. Create combinations of all cities and ports
city_port_combinations <- tidyr::crossing(
  city_row = 1:nrow(sf_cities),
  port_row = 1:nrow(sf_port)
) %>%
  # Join city and port sf objects to directly access their geometry and attributes
  left_join(sf_cities %>% 
              tibble::rowid_to_column("city_row") %>% 
              select(city_row, Ctnb, Ubclssz, Prvn, Pftn, Cont, city_geometry = geometry), 
            by = "city_row") %>%
  left_join(sf_port %>% 
              tibble::rowid_to_column("port_row") %>% 
              select(port_row, nearest_port_name = chinese_name, port_geometry = geometry), 
            by = "port_row") %>%
  # Add new filtering step: filter based on heuristically judged nearest ports by province, to reduce subsequent expensive network distance calculations
  rowwise() %>% # Requires rowwise to access Prvn and nearest_port_name for the current row
  filter(
    nearest_port_name %in% get_allowed_ports(Prvn)
  ) %>%
  ungroup() # Exit rowwise mode

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
      .f = ~ calculate_network_distance(railway_map_new, ..1, ..2), # Define the function to apply and its arguments
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
result_df_vectorized <- calculated_distances %>%
  group_by(Ctnb, Ubclssz, Prvn, Pftn, Cont) %>% # Group by city
  # Find the row with the minimum total_distance_km within each city group
  filter(total_distance_km == min(total_distance_km, na.rm = TRUE)) %>%
  # If multiple ports have the same minimum distance, keep only the first one
  distinct(Ctnb, Ubclssz, Prvn, Pftn, Cont, .keep_all = TRUE) %>%
  select(Ctnb, Ubclssz, Prvn, Pftn, Cont, min_total_distance_km = total_distance_km, nearest_port_name)

# Print final result data frame
print("Calculation complete, results are as follows:")
print(result_df_vectorized)



