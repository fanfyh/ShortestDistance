#' Create Bimodal Transportation Network
#'
#' This function creates a unified bimodal transportation network by combining
#' two transport modes and establishing transfer connections between them within cities.
#' The resulting network enables realistic multimodal routing that considers mode 
#' transfers and associated costs.
#'
#' @param map1 An sf object containing the first transportation network data with 
#'   LINESTRING geometries representing transport segments.
#' @param map2 An sf object containing the second transportation network data with 
#'   LINESTRING geometries representing transport segments.
#' @param mode1_name A character string identifying the first transport mode
#'   (e.g., "railway", "highway", "national_road", "provincial_road").
#' @param mode2_name A character string identifying the second transport mode
#'   (e.g., "railway", "highway", "national_road", "provincial_road").
#' @param cities_sf An sf object containing city administrative boundaries with
#'   POLYGON or MULTIPOLYGON geometries for node assignment.
#' @param max_transfer_distance Maximum distance (in meters) for transfer connections
#'   between different transport modes within the same city. Default is 10000 meters.
#'
#' @return A sfnetwork object representing the complete bimodal transportation
#'   network with the following characteristics:
#'   \describe{
#'     \item{Nodes}{Combined nodes from both transport networks with city assignments}
#'     \item{Edges}{Transport edges from both networks plus transfer edges}
#'     \item{Edge Attributes}{
#'       \itemize{
#'         \item \code{transport_mode}: Type of transport (mode1_name, mode2_name, or "transfer")
#'         \item \code{speed_kmh}: Travel speed in km/h for each mode
#'         \item \code{cost_per_km}: Cost factor per kilometer for each mode
#'         \item \code{edge_length}: Physical length of the edge in meters
#'         \item \code{travel_time}: Calculated travel time in seconds
#'         \item \code{total_cost}: Distance-based cost for routing
#'         \item \code{weight}: Combined weight (travel_time + total_cost) for optimization
#'       }
#'     }
#'     \item{Node Attributes}{
#'       \itemize{
#'         \item \code{network_type}: Original network type for each node
#'         \item \code{node_id}: Unique identifier across the combined network
#'         \item \code{province}: Province assignment for transfer generation
#'         \item \code{citychn}: City assignment for transfer generation
#'       }
#'     }
#'   }
#'
#' @details
#' The function performs the following steps to create a realistic bimodal network:
#' 
#' \strong{1. Data Cleaning and Node Extraction:}
#' \itemize{
#'   \item Removes empty and invalid geometries from both input networks
#'   \item Extracts nodes (endpoints) from LINESTRING geometries
#'   \item Assigns nodes to cities using spatial operations
#'   \item Assigns mode-specific travel parameters (speed, cost)
#' }
#' 
#' \strong{2. Individual Network Creation:}
#' \itemize{
#'   \item Creates separate sfnetwork objects for each transport mode
#'   \item Extracts nodes and edges with proper indexing and attributes
#' }
#' 
#' \strong{3. Transfer Edge Generation:}
#' \itemize{
#'   \item Uses create_transfer_edges() to find transfer connections within cities
#'   \item Creates LINESTRING geometries for transfer paths
#'   \item Applies transfer penalties (low speed, high cost) to reflect real-world constraints
#' }
#' 
#' \strong{4. Network Combination:}
#' \itemize{
#'   \item Combines nodes from both networks with proper ID offsetting
#'   \item Merges edges from both networks with transfer connections
#'   \item Calculates comprehensive routing weights
#' }
#'
#' @examples
#' \dontrun{
#' # Create a railway-highway bimodal network
#' railway_highway_net <- create_bimodal_network(
#'   railway_map_new, 
#'   highway_map_new,
#'   "railway",
#'   "highway", 
#'   merged_sf_city,
#'   max_transfer_distance = 10000
#' )
#' 
#' # Create a highway-national road network  
#' highway_national_net <- create_bimodal_network(
#'   highway_map_new,
#'   national_road_map,
#'   "highway",
#'   "national_road",
#'   merged_sf_city
#' )
#' 
#' # Use for routing
#' shortest_path <- st_network_paths(
#'   railway_highway_net, 
#'   from = origin_point, 
#'   to = destination_point,
#'   weights = "weight"
#' )
#' }
#'
#' @seealso 
#' \code{\link{create_transfer_edges}} for transfer edge creation,
#' \code{\link{assign_points_to_cities}} for city assignment,
#' \code{\link{extract_nodes}} for node extraction,
#' \code{\link[sfnetworks]{sfnetwork}} for network construction
#'
#' @note
#' \itemize{
#'   \item Input networks should use the same coordinate reference system (CRS)
#'   \item Large networks may require significant memory and processing time
#'   \item The function only handles two transport modes at a time for flexibility
#'   \item Cities_sf must contain 'province' and 'citychn' columns for proper assignment
#' }
#'
#' @export
create_bimodal_network <- function(map1, map2, mode1_name, mode2_name, cities_sf, max_transfer_distance = 10000) {
  
  # Step 1: Clean and prepare the input maps
  map1_clean <- map1 |> 
    filter(!st_is_empty(geometry)) |>
    st_cast("LINESTRING") |>
    filter(st_is_valid(geometry))
  
  map2_clean <- map2 |> 
    filter(!st_is_empty(geometry)) |>
    st_cast("LINESTRING") |>
    filter(st_is_valid(geometry))
  
  cat("Cleaned networks:", nrow(map1_clean), "edges in", mode1_name, "and", nrow(map2_clean), "edges in", mode2_name, "\n")
  
  # Step 2: Create individual networks FIRST with attributes
  mode_params <- list(
    railway = list(speed_kmh = 200, cost_per_km = 0.8),
    highway = list(speed_kmh = 120, cost_per_km = 1.0),
    national_road = list(speed_kmh = 80, cost_per_km = 1.2),
    provincial_road = list(speed_kmh = 60, cost_per_km = 1.5)
  )
  
  params1 <- mode_params[[mode1_name]] %||% list(speed_kmh = 80, cost_per_km = 1.0)
  params2 <- mode_params[[mode2_name]] %||% list(speed_kmh = 80, cost_per_km = 1.0)
  
  map1_with_attrs <- map1_clean |>
    mutate(
      transport_mode = mode1_name,
      speed_kmh = params1$speed_kmh,
      cost_per_km = params1$cost_per_km
    )
  
  map2_with_attrs <- map2_clean |>
    mutate(
      transport_mode = mode2_name, 
      speed_kmh = params2$speed_kmh,
      cost_per_km = params2$cost_per_km
    )
  
  network1 <- as_sfnetwork(map1_with_attrs, directed = FALSE)
  network2 <- as_sfnetwork(map2_with_attrs, directed = FALSE)
  
  # Step 3: Extract nodes from networks and assign to cities
  network1_nodes <- network1 |> activate("nodes") |> st_as_sf() |>
    mutate(network_type = mode1_name, original_node_id = paste0("A", row_number()))
  
  network2_nodes <- network2 |> activate("nodes") |> st_as_sf() |>
    mutate(network_type = mode2_name, original_node_id = paste0("B", row_number()))
  
  # Extract edges and adjust indices
  edges1 <- network1 |> activate("edges") |> st_as_sf() |> 
    mutate(
      edge_length = st_length(geometry),
      from = paste0("A", from),  # Offset indices
      to = paste0("A", to)
    )
  
  edges2 <- network2 |> activate("edges") |> st_as_sf() |> 
    mutate(
      edge_length = st_length(geometry),
      from = paste0("B", from),  # Offset indices
      to = paste0("B", to)
    )

  nodes1_with_cities <- assign_points_to_cities(network1_nodes, cities_sf)
  nodes2_with_cities <- assign_points_to_cities(network2_nodes, cities_sf)
  
  # Step 4: Create transfer edges
  transfer_edges <- create_transfer_edges(
    nodes1_with_cities, 
    nodes2_with_cities, 
    mode1_name, 
    mode2_name, 
    max_distance = max_transfer_distance, 
    city_filter = TRUE
  )
  
  # Step 5: Create the final combined network
  # Combine all nodes with global IDs
  all_nodes <- bind_rows(
    nodes1_with_cities ,
    nodes2_with_cities 
  )
  

  
  # Combine edges
  all_edges <- bind_rows(
    edges1 |> select(from, to, transport_mode, speed_kmh, cost_per_km, edge_length, geometry),
    edges2 |> select(from, to, transport_mode, speed_kmh, cost_per_km, edge_length, geometry)
  )
  
  # Add transfer edges if they exist
  if (!is.null(transfer_edges) && nrow(transfer_edges) > 0) {
    transfer_edges_formatted <- transfer_edges |>
      select(from = from_node, to = to_node, transport_mode, speed_kmh, cost_per_km, edge_length, geometry)
    
    all_edges <- bind_rows(all_edges, transfer_edges_formatted)
    cat("Added", nrow(transfer_edges), "transfer edges\n")
  }
  
  # Step 6: Create final network - this should work now
  bimodal_network <- sfnetwork(all_nodes, all_edges, directed = FALSE, node_key = "original_node_id") |>
    activate("edges") |>
    mutate(
      travel_time = as.numeric(edge_length) / (speed_kmh * 1000 / 3600),
      total_cost = as.numeric(edge_length) * cost_per_km / 1000,
      weight = travel_time + total_cost
    ) |>
    activate("nodes")
  
  cat("Created bimodal network with", 
      nrow(st_as_sf(bimodal_network, "nodes")), "nodes and", 
      nrow(st_as_sf(bimodal_network, "edges")), "edges\n")
  
  return(bimodal_network)
}