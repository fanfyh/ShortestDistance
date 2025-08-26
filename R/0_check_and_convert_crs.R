# Load necessary packages -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)
# funs -------------------------------------------------------------------

# source(here("R/funs/calculate_network_distance.R"))

# source(here("R/funs/create_transfer_edges.R"))

# source(here("R/funs/assign_points_to_cities.R"))

source(here("R/funs/create_bimodal_network.R"))

# import data ------------------------------------------------------------

## maps in different type ------------------------------------------------

# Read the new 2000 map files (with provincial roads)
shapefile_paths_new <- here::here("data-raw/2000-map", c("hiway.shp", "rail.shp", "gd.shp", "sd.shp"))

# Read all four transportation network types
highway_map_taobao <- st_read(shapefile_paths_new[1]) |> 
  mutate(type = "高速公路") |> check_and_convert_crs()

railway_map_taobao <- st_read(shapefile_paths_new[2]) |>
  mutate(type = "铁路") |> check_and_convert_crs()

national_map_taobao <- st_read(shapefile_paths_new[3]) |> 
  mutate(type = "国道") |> check_and_convert_crs()

provincial_map_taobao <- st_read(shapefile_paths_new[4]) |> 
  mutate(type = "省道") |> check_and_convert_crs()

waterway_map_taobao <- 
  st_read(here("data-raw/2020-Waterway/Waterwaysingleline.shp")) |> 
  mutate(type = "水运") |> 
  check_and_convert_crs()


# Standardize column names across all datasets
# First inspect what columns each dataset has
cat("--- Highway columns: --- \n", names(highway_map_taobao))
cat("--- Railway columns: --- \n", names(railway_map_taobao))
cat("--- National road columns: --- \n", names(national_road_map))
cat("--- Provincial road columns: --- \n", names(provincial_road_map))




## Traffic Data (2000 Railway, Highway, National Road Maps) ---------------------------------------------------


# Set the directory path containing the Shapefile files

# Build the full Shapefile file paths
full_shapefile_paths <- here::here("data-raw/2000年道路交通图", c("铁路_polyline.shp", "高速_polyline.shp", "国道_polyline.shp"))

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
railway_map_pku <- st_read(full_shapefile_paths[[1]]) |>
  mutate(type = "铁路") |> check_and_convert_crs()

highway_map_pku <- st_read(full_shapefile_paths[[2]]) |>
  mutate(type = "高速") |> check_and_convert_crs()

nationroad_map_pku <- st_read(full_shapefile_paths[[3]]) |> 
  mutate(type = "国道") |> check_and_convert_crs()



# save data --------------------------------------------------------------

save(
  highway_map_taobao,
  railway_map_taobao,
  national_map_taobao,
  provincial_map_taobao,
  waterway_map_taobao,
  railway_map_pku,
  highway_map_pku,
  nationroad_map_pku,
  file = here::here("data/all_maps.Rdata")
)



# plot -------------------------------------------------------------------

# Create a more visually appealing highway comparison plot
bind_rows(
  highway_map_taobao |> mutate(source = "Taobao"),
  highway_map_pku |> mutate(source = "PKU")
) |> 
  ggplot() +
  geom_sf(aes(color = source), linewidth = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("PKU" = "#E41A1C", "Taobao" = "#377EB8")) +
  labs(
    title = "Comparison of Highway Networks from Different Sources",
    subtitle = "PKU (北京大学) vs Taobao (淘宝)",
    caption = "Data sources: PKU and Taobao transportation map collections",
    color = "Data Source"
  ) +
  theme_minimal(base_family = "Songti SC") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Save with appropriate dimensions
ggsave(
  filename = here::here("output/figure", "compare_highway.jpg"),
  width = 10, 
  height = 8,
  dpi = 300
)


# Create a more visually appealing railway comparison plot
# Create small spatial offsets
railway_map_taobao_offset <- railway_map_taobao |> 
  st_buffer(dist = 500) |> st_boundary()  # Adjust distance as needed

railway_map_pku_offset <- railway_map_pku |> 
  st_buffer(dist = -500) |> st_boundary()
bind_rows(
  railway_map_taobao_offset |> mutate(source = "Taobao"),
  railway_map_pku_offset |> mutate(source = "PKU")
) |> 
  ggplot() +
  geom_sf(aes(color = source), linewidth = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("PKU" = "#E41A1C", "Taobao" = "#377EB8")) +
  labs(
    title = "Comparison of Railway Networks from Different Sources",
    subtitle = "PKU (北京大学) vs Taobao (淘宝)",
    caption = "Data sources: PKU and Taobao transportation map collections",
    color = "Data Source"
  ) +
  theme_minimal(base_family = "Songti SC") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# Save with appropriate dimensions
ggsave(
  filename = here::here("output/figure", "compare_railway.jpg"),
  width = 10, 
  height = 8,
  dpi = 300
)


# Create a more visually appealing nationroad comparison plot


bind_rows(
  national_map_taobao |> mutate(source = "Taobao"),
  nationroad_map_pku |> mutate(source = "PKU")
) |> 
  ggplot() +
  geom_sf(aes(color = source), linewidth = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("PKU" = "#E41A1C", "Taobao" = "#377EB8")) +
  labs(
    title = "Comparison of Nation Road Networks from Different Sources",
    subtitle = "PKU (北京大学) vs Taobao (淘宝)",
    caption = "Data sources: PKU and Taobao transportation map collections",
    color = "Data Source"
  ) +
  theme_minimal(base_family = "Songti SC") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  ) +
  facet_wrap(~ source)

# Save with appropriate dimensions
ggsave(
  filename = here::here("output/figure", "compare_nationroad.jpg"),
  width = 10, 
  height = 8,
  dpi = 300
)

