#' Create Transfer Edges Between Different Transport Networks (City-Optimized)
#'
#' This function identifies nearby nodes between different transportation networks
#' and creates transfer edges to enable multimodal routing. The function is optimized
#' to only consider node pairs within the same city, dramatically reducing computation
#' time while maintaining logical transfer connections.
#'
#' @param nodes1 An sf object containing nodes from the first transport network
#'   with city assignment information (must contain `province` and `citychn` columns).
#'   Each node should be a POINT geometry representing a location in the network
#'   (e.g., highway toll stations, railway stations).
#' @param nodes2 An sf object containing nodes from the second transport network
#'   with city assignment information (must contain `province` and `citychn` columns).
#'   Each node should be a POINT geometry representing a location in the network.
#' @param mode1 A character string identifying the first transport mode
#'   (e.g., "highway", "railway", "national_road").
#' @param mode2 A character string identifying the second transport mode
#'   (e.g., "highway", "railway", "provincial_road").
#' @param max_distance Maximum distance (in meters) within which nodes from different
#'   networks are considered connected for transfers. Default is 2000 meters.
#'   Only applied to node pairs within the same city.
#' @param city_filter Logical, whether to pre-filter nodes to same cities before
#'   distance calculation. Default is TRUE for performance optimization.
#'
#' @return An sf object containing transfer edge information with LINESTRING geometries,
#'   or NULL if no transfer connections are found. The returned sf object includes:
#'   \describe{
#'     \item{from_node}{Index of the source node in the first network}
#'     \item{to_node}{Index of the destination node in the combined network (offset by nrow(nodes1))}
#'     \item{from_mode}{Transport mode of the source node}
#'     \item{to_mode}{Transport mode of the destination node}
#'     \item{province}{Province where both nodes are located}
#'     \item{citychn}{City where both nodes are located}
#'     \item{transfer_distance}{Actual distance between the connected nodes (in meters)}
#'     \item{transport_mode}{Set to "transfer" to identify these as transfer edges}
#'     \item{speed_kmh}{Transfer speed in km/h (typically walking speed, 5 km/h)}
#'     \item{cost_per_km}{Cost penalty per kilometer for transfers (typically high, 10)}
#'     \item{edge_length}{Transfer distance as a units object with meters}
#'     \item{geometry}{LINESTRING geometry connecting the from_node and to_node}
#'   }
#'
#' @details
#' \strong{Optimization Strategy:}
#' The function uses a city-based filtering approach to reduce computational complexity:
#' 
#' 1. \strong{City-based Pre-filtering:} Only considers node pairs within the same city
#' 2. \strong{Chunked Processing:} Processes cities individually to manage memory
#' 3. \strong{Distance Filtering:} Applies distance threshold only to relevant pairs
#' 4. \strong{Geometry Creation:} Creates LINESTRING geometries connecting transfer endpoints
#' 5. \strong{Early Termination:} Skips cities with no viable transfer pairs
#' 
#' \strong{Performance Benefits:}
#' \itemize{
#'   \item Reduces distance matrix calculations from O(n*m) to O(sum(ni*mi)) per city
#'   \item Eliminates meaningless inter-city transfers at large distances
#'   \item Focuses computational resources on realistic transfer scenarios
#'   \item Creates spatial geometries for network visualization and analysis
#'   \item Scales much better with large national networks
#' }
#' 
#' \strong{Logical Rationale:}
#' Transfer connections between transport modes are most relevant within the same
#' administrative area where travelers would realistically switch modes. The LINESTRING
#' geometries represent walking or short-distance transfer paths between different
#' transport terminals within the same city.
#'
#' @examples
#' \dontrun{
#' # Create transfer edges between railway and highway networks (optimized)
#' railway_stations_with_cities <- assign_points_to_cities(railway_stations, merged_sf_city)
#' highway_tolls_with_cities <- assign_points_to_cities(highway_tolls, merged_sf_city)
#' 
#' transfers <- create_transfer_edges(
#'   railway_stations_with_cities, 
#'   highway_tolls_with_cities, 
#'   "railway", 
#'   "highway", 
#'   max_distance = 10000,
#'   city_filter = TRUE
#' )
#' 
#' # Plot transfer edges for a specific city
#' beijing_transfers <- transfers |> filter(citychn == "北京市")
#' plot(beijing_transfers$geometry, col = "red", lwd = 2)
#' }
#'
#' @seealso 
#' \code{\link{assign_points_to_cities}} for city assignment preprocessing,
#' \code{\link[sf]{st_distance}} for distance calculations,
#' \code{\link[sf]{st_linestring}} for geometry creation,
#' \code{\link[sfnetworks]{sfnetwork}} for network creation
#'
#' @export
create_transfer_edges <- function(nodes1, nodes2, mode1, mode2, max_distance = 2000, city_filter = TRUE) {
  
  if (city_filter) {
    # Pre-filter to only consider nodes within the same cities
    # First, identify cities that have both types of nodes
    cities_with_both <- inner_join(
      nodes1 |> st_drop_geometry() |> distinct(province, citychn),
      nodes2 |> st_drop_geometry() |> distinct(province, citychn),
      by = c("province", "citychn")
    )
    
    cat("Processing transfers for", nrow(cities_with_both), "cities with both transport modes\n")
    
    if (nrow(cities_with_both) == 0) {
      cat("No cities found with both transport modes\n")
      return(NULL)
    }
    
    # Process each city separately to create transfer edges
    transfer_edges_list <- map2_dfr(
      cities_with_both$province,
      cities_with_both$citychn,
      function(prov, city) {
        
        # Get nodes for this specific city
        city_nodes1 <- nodes1 |> filter(province == prov, citychn == city)
        city_nodes2 <- nodes2 |> filter(province == prov, citychn == city)
        
        if (nrow(city_nodes1) == 0 || nrow(city_nodes2) == 0) {
          return(NULL)
        }
        
        # Calculate distances only within this city
        distances <- st_distance(city_nodes1, city_nodes2)
        transfer_pairs <- which(distances <= units::set_units(max_distance, "m"), arr.ind = TRUE)
        
        if (nrow(transfer_pairs) > 0) {
          # Get original node indices from the full datasets
          city1_original_indices <- nodes1$original_node_id[which(nodes1$province == prov & nodes1$citychn == city)]
          city2_original_indices <- nodes2$original_node_id[which(nodes2$province == prov & nodes2$citychn == city)]
          
          # Create LINESTRING geometries for each transfer pair
          transfer_geometries <- map2(
            transfer_pairs[,1], 
            transfer_pairs[,2],
            function(idx1, idx2) {
              # Get coordinates from the city-filtered nodes
              coord1 <- st_coordinates(city_nodes1$geometry[idx1])
              coord2 <- st_coordinates(city_nodes2$geometry[idx2])
              
              # Create LINESTRING connecting the two points
              st_linestring(rbind(coord1, coord2))
            }
          ) |> st_sfc(crs = st_crs(nodes1))
          
          transfer_edges_city <- tibble(
            from_node = city1_original_indices[transfer_pairs[,1]],
            to_node = city2_original_indices[transfer_pairs[,2]], # Offset for combined network
            from_mode = mode1,
            to_mode = mode2,
            province = prov,
            citychn = city,
            transfer_distance = as.numeric(distances[transfer_pairs]),
            transport_mode = "transfer",
            speed_kmh = 5,  # Walking speed for transfers
            cost_per_km = 10,  # High cost to represent inconvenience of transfer
            edge_length = units::set_units(as.numeric(distances[transfer_pairs]), "m")
          ) |>
          # Add geometry column and convert to sf
          mutate(geometry = transfer_geometries) |>
          st_as_sf()
          
          return(transfer_edges_city)
        }
        return(NULL)
      }
    )
    
    if (!is.null(transfer_edges_list) && nrow(transfer_edges_list) > 0) {
      cat("Created", nrow(transfer_edges_list), "transfer edges with geometries across", 
          n_distinct(transfer_edges_list$citychn), "cities\n")
      return(transfer_edges_list)
    } else {
      cat("No transfer edges created within distance threshold\n")
      return(NULL)
    }
    
  } else {
    # Original implementation with geometry creation
    cat("Using original non-optimized approach (not recommended for large datasets)\n")
    distances <- st_distance(nodes1, nodes2)
    transfer_pairs <- which(distances <= units::set_units(max_distance, "m"), arr.ind = TRUE)
    
    if (nrow(transfer_pairs) > 0) {
      # Create LINESTRING geometries
      transfer_geometries <- map2(
        transfer_pairs[,1], 
        transfer_pairs[,2],
        function(idx1, idx2) {
          coord1 <- st_coordinates(nodes1$geometry[idx1])
          coord2 <- st_coordinates(nodes2$geometry[idx2])
          st_linestring(rbind(coord1, coord2))
        }
      ) |> st_sfc(crs = st_crs(nodes1))
      
      transfer_edges <- tibble(
        # Use row indices directly, not original_node_id which may not exist
        from_node = if ("original_node_id" %in% names(nodes1)) {
          nodes1$original_node_id[transfer_pairs[,1]]
        } else {
          transfer_pairs[,1]  # Use row indices if original_node_id doesn't exist
        },
        to_node = if ("original_node_id" %in% names(nodes2)) {
          nodes2$original_node_id[transfer_pairs[,2]]
        } else {
          transfer_pairs[,2] + nrow(nodes1)  # Offset for combined network
        },
        from_mode = mode1,
        to_mode = mode2,
        transfer_distance = as.numeric(distances[transfer_pairs]),
        transport_mode = "transfer",
        speed_kmh = 5,  # Walking speed for transfers
        cost_per_km = 10,  # High cost to represent inconvenience of transfer
        edge_length = units::set_units(transfer_distance, "m")
      ) |>
      mutate(geometry = transfer_geometries) |>
      st_as_sf()
      
      return(transfer_edges)
    }
    return(NULL)
  }
}
