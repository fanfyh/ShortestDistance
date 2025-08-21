#' Create Transfer Edges Between Different Transport Networks
#'
#' This function identifies nearby nodes between different transportation networks
#' and creates transfer edges to enable multimodal routing. Transfer edges represent
#' the ability to switch between different transport modes (e.g., from highway to railway)
#' at locations where the networks are within a specified distance of each other.
#'
#' @param nodes1 An sf object containing nodes from the first transport network.
#'   Each node should be a POINT geometry representing a location in the network
#'   (e.g., highway toll stations, railway stations).
#' @param nodes2 An sf object containing nodes from the second transport network.
#'   Each node should be a POINT geometry representing a location in the network.
#' @param mode1 A character string identifying the first transport mode
#'   (e.g., "highway", "railway", "national_road").
#' @param mode2 A character string identifying the second transport mode
#'   (e.g., "highway", "railway", "provincial_road").
#' @param max_distance Maximum distance (in meters) within which nodes from different
#'   networks are considered connected for transfers. Default is `transfer_distance`
#'   (typically 2000 meters).
#'
#' @return A tibble containing transfer edge information, or NULL if no transfer
#'   connections are found. The returned tibble includes:
#'   \describe{
#'     \item{from_node}{Index of the source node in the first network}
#'     \item{to_node}{Index of the destination node in the combined network (offset by nrow(nodes1))}
#'     \item{from_mode}{Transport mode of the source node}
#'     \item{to_mode}{Transport mode of the destination node}
#'     \item{transfer_distance}{Actual distance between the connected nodes (in meters)}
#'     \item{transport_mode}{Set to "transfer" to identify these as transfer edges}
#'     \item{speed_kmh}{Transfer speed in km/h (typically walking speed, 5 km/h)}
#'     \item{cost_per_km}{Cost penalty per kilometer for transfers (typically high, 10)}
#'     \item{edge_length}{Transfer distance as a units object with meters}
#'   }
#'
#' @details
#' The function works by:
#' 1. Calculating the distance matrix between all nodes in the two networks
#' 2. Identifying node pairs within the maximum transfer distance
#' 3. Creating transfer edges with appropriate costs and speeds
#' 4. Applying node index offsets to ensure proper connectivity in the combined network
#'
#' Transfer edges are assigned a low speed (walking pace) and high cost per kilometer
#' to reflect the inconvenience and time cost of switching between transport modes.
#' This ensures that the routing algorithm will prefer direct routes when possible
#' but can still find multimodal solutions when necessary.
#'
#' @examples
#' \dontrun{
#' # Create transfer edges between highway and railway networks
#' highway_nodes <- highway_network |> activate("nodes") |> st_as_sf()
#' railway_nodes <- railway_network |> activate("nodes") |> st_as_sf()
#' 
#' transfers <- create_transfer_edges(
#'   highway_nodes, 
#'   railway_nodes, 
#'   "highway", 
#'   "railway", 
#'   max_distance = 2000
#' )
#' }
#'
#' @seealso \code{\link[sf]{st_distance}} for distance calculations,
#'   \code{\link[sfnetworks]{sfnetwork}} for network creation
create_transfer_edges <- function(nodes1, nodes2, mode1, mode2, max_distance = transfer_distance) {
  # Find nearby nodes between different networks
  distances <- st_distance(nodes1, nodes2)
  transfer_pairs <- which(distances <= units::set_units(max_distance, "m"), arr.ind = TRUE)
  
  if (nrow(transfer_pairs) > 0) {
    transfer_edges <- tibble(
      from_node = transfer_pairs[,1],
      to_node = transfer_pairs[,2] + nrow(nodes1),  # Offset for combined node index
      from_mode = mode1,
      to_mode = mode2,
      transfer_distance = as.numeric(distances[transfer_pairs]),
      transport_mode = "transfer",
      speed_kmh = 5,  # Walking speed for transfers
      cost_per_km = 10,  # High cost to represent inconvenience of transfer
      edge_length = units::set_units(transfer_distance, "m")
    )
    return(transfer_edges)
  }
  return(NULL)
}