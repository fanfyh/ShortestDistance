# Load necessary packages -------------------------------------------------------------------

library(tidyverse)      # For dplyr, tidyr, ggplot2, etc.
library(sf)             # For spatial data operations (st_as_sf, st_join, etc.)
library(readxl)         # For read_excel()
library(here)           # For here() function for file paths
library(tibble)         # For rowid_to_column() (part of tidyverse but explicitly used)
library(skimr)          # For data summary (though it seems to have issues in your session)

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


## Longitude and Latitude - Cities -----------------------------------------------------------------

location_city <- 
  read_excel(here("data-raw/城市经纬度/城市经纬度.xlsx")) |> 
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


# save data --------------------------------------------------------------

save(city_port_combinations, file = here("data/city_port_combinations.Rdata"))
