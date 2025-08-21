#' Create Multimodal Transportation Network
#'
#' This function creates a unified multimodal transportation network by combining
#' different transport modes (highway, railway, national roads, provincial roads)
#' and establishing transfer connections between them. The resulting network enables
#' realistic multimodal routing that considers mode transfers and associated costs.
#'
#' @param highway_map An sf object containing highway network data with LINESTRING
#'   geometries representing highway segments and toll stations.
#' @param railway_map An sf object containing railway network data with LINESTRING
#'   geometries representing railway tracks and stations.
#' @param national_road_map An sf object containing national road network data
#'   with LINESTRING geometries representing national road segments.
#' @param provincial_road_map An sf object containing provincial road network data
#'   with LINESTRING geometries representing provincial road segments.
#'
#' @return A sfnetwork object representing the complete multimodal transportation
#'   network with the following characteristics:
#'   \describe{
#'     \item{Nodes}{Combined nodes from all four transport networks with unique IDs}
#'     \item{Edges}{Transport edges with mode-specific attributes plus transfer edges}
#'     \item{Edge Attributes}{
#'       \itemize{
#'         \item \code{transport_mode}: Type of transport ("highway", "railway", "national_road", "provincial_road", "transfer")
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
#'       }
#'     }
#'   }
#'
#' @details
#' The function performs the following steps to create a realistic multimodal network:
#' 
#' \strong{1. Data Cleaning and Standardization:}
#' \itemize{
#'   \item Removes empty and invalid geometries from all input networks
#'   \item Standardizes column structure across transport modes
#'   \item Assigns mode-specific travel parameters (speed, cost)
#' }
#' 
#' \strong{2. Individual Network Creation:}
#' \itemize{
#'   \item Creates separate sfnetwork objects for each transport mode
#'   \item Extracts nodes and edges with proper indexing
#' }
#' 
#' \strong{3. Transfer Point Identification:}
#' \itemize{
#'   \item Finds nodes within 2km of each other across different networks
#'   \item Creates transfer edges representing mode-switching capability
#'   \item Applies transfer penalties (low speed, high cost) to reflect real-world constraints
#' }
#' 
#' \strong{4. Network Combination:}
#' \itemize{
#'   \item Combines all nodes with proper ID offsetting
#'   \item Merges edges from all networks with transfer connections
#'   \item Calculates comprehensive routing weights
#' }
#' 
#' \strong{Transport Mode Characteristics:}
#' \tabular{lrrr}{
#'   Mode \tab Speed (km/h) \tab Cost Factor \tab Usage \cr
#'   Highway \tab 120 \tab 1.0 \tab Fast, flexible, moderate cost \cr
#'   Railway \tab 200 \tab 0.8 \tab Fastest, cheapest per km, less flexible \cr
#'   National Road \tab 80 \tab 1.2 \tab Moderate speed, higher cost \cr
#'   Provincial Road \tab 60 \tab 1.5 \tab Slowest, highest cost, good coverage \cr
#'   Transfer \tab 5 \tab 10.0 \tab Walking speed, high penalty
#' }
#' 
#' The high transfer costs ensure that the routing algorithm prefers direct routes
#' when possible but can find multimodal solutions when necessary for connectivity.
#'
#' @examples
#' \dontrun{
#' # Load transportation network data
#' highway_data <- st_read("highway.shp")
#' railway_data <- st_read("railway.shp") 
#' national_data <- st_read("national_roads.shp")
#' provincial_data <- st_read("provincial_roads.shp")
#' 
#' # Create multimodal network
#' multimodal_net <- create_multimodal_network(
#'   highway_data, railway_data, national_data, provincial_data
#' )
#' 
#' # Use for routing
#' shortest_path <- st_network_paths(
#'   multimodal_net, 
#'   from = origin_point, 
#'   to = destination_point,
#'   weights = "weight"
#' )
#' }
#'
#' @seealso 
#' \code{\link{create_transfer_edges}} for transfer edge creation,
#' \code{\link[sfnetworks]{sfnetwork}} for network construction,
#' \code{\link[sfnetworks]{st_network_paths}} for routing on the resulting network
#'
#' @note
#' \itemize{
#'   \item Input networks should use the same coordinate reference system (CRS)
#'   \item Large networks may require significant memory and processing time
#'   \item Transfer distance (2km) can be adjusted within the function if needed
#'   \item The resulting network may have multiple disconnected components
#' }
#'
#' @export
create_multimodal_network <- function(highway_map, railway_map, national_road_map, provincial_road_map) {
  
  # Step 1: Create individual networks with mode identifiers
  highway_clean <- highway_map |> 
    select(type, geometry) |> 
    filter(!st_is_empty(geometry)) |>
    st_cast("LINESTRING") |>
    filter(st_is_valid(geometry)) |>
    mutate(
      transport_mode = "highway",
      speed_kmh = 120,
      cost_per_km = 1.0  # Base cost
    )
  
  railway_clean <- railway_map |> 
    select(type, geometry) |> 
    filter(!st_is_empty(geometry)) |>
    st_cast("LINESTRING") |>
    filter(st_is_valid(geometry)) |>
    mutate(
      transport_mode = "railway",
      speed_kmh = 200,
      cost_per_km = 0.8  # Cheaper per km but less flexible
    )
  
  national_road_clean <- national_road_map |> 
    select(type, geometry) |> 
    filter(!st_is_empty(geometry)) |>
    st_cast("LINESTRING") |>
    filter(st_is_valid(geometry)) |>
    mutate(
      transport_mode = "national_road", 
      speed_kmh = 80,
      cost_per_km = 1.2
    )
  
  provincial_road_clean <- provincial_road_map |> 
    select(type, geometry) |> 
    filter(!st_is_empty(geometry)) |>
    st_cast("LINESTRING") |>
    filter(st_is_valid(geometry)) |>
    mutate(
      transport_mode = "provincial_road",
      speed_kmh = 60, 
      cost_per_km = 1.5
    )
  
  # Step 2: Create individual networks
  highway_network <- as_sfnetwork(highway_clean, directed = FALSE)
  railway_network <- as_sfnetwork(railway_clean, directed = FALSE)
  national_network <- as_sfnetwork(national_road_clean, directed = FALSE)
  provincial_network <- as_sfnetwork(provincial_road_clean, directed = FALSE)
  
  # Step 3: Extract nodes from each network
  highway_nodes <- highway_network |> activate("nodes") |> st_as_sf() |> mutate(network_type = "highway")
  railway_nodes <- railway_network |> activate("nodes") |> st_as_sf() |> mutate(network_type = "railway")
  national_nodes <- national_network |> activate("nodes") |> st_as_sf() |> mutate(network_type = "national_road")
  provincial_nodes <- provincial_network |> activate("nodes") |> st_as_sf() |> mutate(network_type = "provincial_road")
  
  # Step 4: Find transfer points (nodes within transfer_distance of each other)
  transfer_distance <- 2000  # 2km transfer radius
  
  # Step 5: Combine all nodes
  all_nodes <- bind_rows(
    highway_nodes |> mutate(node_id = row_number()),
    railway_nodes |> mutate(node_id = row_number() + nrow(highway_nodes)),
    national_nodes |> mutate(node_id = row_number() + nrow(highway_nodes) + nrow(railway_nodes)),
    provincial_nodes |> mutate(node_id = row_number() + nrow(highway_nodes) + nrow(railway_nodes) + nrow(national_nodes))
  )
  
  # Step 6: Combine all edges from original networks
  highway_edges <- highway_network |> activate("edges") |> st_as_sf() |> 
    mutate(edge_length = st_length(geometry), from_offset = 0)
  railway_edges <- railway_network |> activate("edges") |> st_as_sf() |> 
    mutate(edge_length = st_length(geometry), from_offset = nrow(highway_nodes))
  national_edges <- national_network |> activate("edges") |> st_as_sf() |> 
    mutate(edge_length = st_length(geometry), from_offset = nrow(highway_nodes) + nrow(railway_nodes))
  provincial_edges <- provincial_network |> activate("edges") |> st_as_sf() |> 
    mutate(edge_length = st_length(geometry), from_offset = nrow(highway_nodes) + nrow(railway_nodes) + nrow(national_nodes))
  
  # Step 7: Create transfer edges between modes
  hw_rail_transfers <- create_transfer_edges(highway_nodes, railway_nodes, "highway", "railway")
  hw_nat_transfers <- create_transfer_edges(highway_nodes, national_nodes, "highway", "national_road")
  hw_prov_transfers <- create_transfer_edges(highway_nodes, provincial_nodes, "highway", "provincial_road")
  rail_nat_transfers <- create_transfer_edges(railway_nodes, national_nodes, "railway", "national_road")
  rail_prov_transfers <- create_transfer_edges(railway_nodes, provincial_nodes, "railway", "provincial_road")
  nat_prov_transfers <- create_transfer_edges(national_nodes, provincial_nodes, "national_road", "provincial_road")
  
  # Step 8: Create the final multimodal network with proper edge connectivity
  all_edges <- bind_rows(
    highway_edges |> 
      mutate(from = from + 0, to = to + 0) |> # Highway nodes start at index 1
      select(from, to, transport_mode, speed_kmh, cost_per_km, edge_length, geometry),
    railway_edges |> 
      mutate(from = from + nrow(highway_nodes), to = to + nrow(highway_nodes)) |> # Offset by highway nodes
      select(from, to, transport_mode, speed_kmh, cost_per_km, edge_length, geometry),
    national_edges |> 
      mutate(from = from + nrow(highway_nodes) + nrow(railway_nodes), 
             to = to + nrow(highway_nodes) + nrow(railway_nodes)) |> # Offset by highway + railway nodes
      select(from, to, transport_mode, speed_kmh, cost_per_km, edge_length, geometry),
    provincial_edges |> 
      mutate(from = from + nrow(highway_nodes) + nrow(railway_nodes) + nrow(national_nodes), 
             to = to + nrow(highway_nodes) + nrow(railway_nodes) + nrow(national_nodes)) |> # Offset by all previous nodes
      select(from, to, transport_mode, speed_kmh, cost_per_km, edge_length, geometry)
  )
  
  # Add transfer edges with proper from/to indices
  if (!is.null(hw_rail_transfers)) {
    transfer_edges_sf <- hw_rail_transfers |> 
      select(from_node, to_node, transport_mode, speed_kmh, cost_per_km, edge_length) |>
      rename(from = from_node, to = to_node) |>
      mutate(geometry = transfer_geometries) |>
      st_as_sf()
    
    all_edges <- bind_rows(all_edges, transfer_edges_sf)
  }
  
  # Step 9: Create final sfnetwork with travel time weights
  multimodal_network <- sfnetwork(all_nodes, all_edges, directed = FALSE) |>
    activate("edges") |>
    mutate(
      travel_time = as.numeric(edge_length) / (speed_kmh * 1000 / 3600),  # seconds
      total_cost = as.numeric(edge_length) * cost_per_km / 1000,  # cost based on distance and mode
      weight = travel_time + total_cost  # Combined weight for routing
    ) |>
    activate("nodes")
  
  return(multimodal_network)
}