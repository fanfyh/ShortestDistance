#' Calculate the shortest distance between two points on the traffic network
#'
#' @param network_map An sf object containing traffic network data, geometry type should be LINESTRING.
#' @param p1 The first point, can be an sf_point object, or a vector c(lon, lat) containing longitude and latitude.
#' @param p2 The second point, in the same format as p1.
#'
#' @return A tibble containing the straight-line distance from p1 to the nearest node,
#'         the network distance between the two nodes, and the straight-line distance from p2 to the nearest node.
#'         All distances will be converted to kilometers (km).
#'         If distance cannot be calculated (e.g., points are not within network range or not connected), returns Inf.
#'
#' @examples
#' # Assuming traffic_map_2000 is already loaded
#' # p1 <- st_point(c(108.6328, 19.097))
#' # p2 <- st_point(c(109.4752, 18.293))
#' # distance <- calculate_network_distance(traffic_map_2000, p1, p2)
calculate_network_distance <- function(network_map, p1, p2, path_type = "shortest") {
  # Wrap the entire function logic with tryCatch to ensure a tibble is always returned
  tryCatch({
    # Ensure input is an sf object
    if (!inherits(network_map, "sf")) {
      stop("`network_map` must be an sf object.")
    }

    # --- Standardize input point p1 ---
    # p1_geom will store the final sfg (simple feature geometry) object for calculation
    if (inherits(p1, "sfc") && length(p1) == 1) {
      # If it's an sfc object of length 1 (common when extracted from the geometry column of an sf tibble)
      p1_geom <- p1[[1]] # Extract the internal sfg object
    } else if (inherits(p1, "sfg")) {
      # If it's already an sfg object
      p1_geom <- p1
    } else if (is.numeric(p1) && length(p1) == 2) {
      # If it's a numeric vector c(lon, lat)
      p1_geom <- st_point(p1)
    } else {
      stop("`p1` must be an sf point geometry object (sfg), an sf geometry column (sfc) of length 1, or a numeric vector c(lon, lat).")
    }

    # --- Standardize input point p2 ---
    # p2_geom will store the final sfg (simple feature geometry) object for calculation
    if (inherits(p2, "sfc") && length(p2) == 1) {
      # If it's an sfc object of length 1
      p2_geom <- p2[[1]] # Extract the internal sfg object
    } else if (inherits(p2, "sfg")) {
      # If it's already an sfg object
      p2_geom <- p2
    } else if (is.numeric(p2) && length(p2) == 2) {
      # If it's a numeric vector c(lon, lat)
      p2_geom <- st_point(p2)
    } else {
      stop("`p2` must be an sf point geometry object (sfg), an sf geometry column (sfc) of length 1, or a numeric vector c(lon, lat).")
    }
    
    # Repackage the processed sfg objects into sfc and set the correct CRS for subsequent sf operations
    p1_sfc_final <- st_sfc(p1_geom, crs = st_crs(network_map))
    p2_sfc_final <- st_sfc(p2_geom, crs = st_crs(network_map))
    
    # Set CRS to a projected coordinate system for distance calculation, e.g., EPSG:3857 (Web Mercator)
    # or a more suitable local projected coordinate system
    # Note: WGS84 (EPSG:4326) is a geographic coordinate system, direct distance calculation might be inaccurate here, this is for example only
    # It is recommended to choose a more suitable projected coordinate system based on your data range
    
    # Attempt to transform the network and points to a common projected CRS (e.g., for China, a more specific CRS might be needed)
    # Here, a global projected CRS is used as an example; in actual applications, a regional projection should be chosen.
    # Transform to a projected coordinate system that allows for accurate distance calculation, e.g., UTM zone or a China-specific projection
    # Simple example, transform to a commonly used projected CRS, e.g., EPSG:3857
    # But for large-scale accurate distance calculation, this might not be the optimal choice.
    tryCatch({
      network_map_proj <- st_transform(network_map, crs = 3857)
      p1_proj <- st_transform(p1_sfc_final, crs = 3857)
      p2_proj <- st_transform(p2_sfc_final, crs = 3857)
    }, error = function(e) {
      warning("CRS transformation failed or suitable CRS not found. Distance calculation may be inaccurate or fail. Attempting approximate distance calculation using geographic CRS.")
      network_map_proj <- network_map
      p1_proj <- p1_sfc_final # If transformation fails, use the original sfc object
      p2_proj <- p2_sfc_final
    })


    # Check if points are within the map boundaries
    bbox <- st_bbox(network_map_proj) # Use the bbox of the projected map
    p1_vector <- as.vector(st_coordinates(p1_proj))
    p2_vector <- as.vector(st_coordinates(p2_proj))

    # Compare; if points are outside the bounding box, issue a warning
    if (
      p1_vector[1] < bbox$xmin || p1_vector[1] > bbox$xmax ||
      p1_vector[2] < bbox$ymin || p1_vector[2] > bbox$ymax
    ) {
      warning("The first point is outside the traffic network range, distance calculation may not be possible.")
    }
    if (
      p2_vector[1] < bbox$xmin || p2_vector[1] > bbox$xmax ||
      p2_vector[2] < bbox$ymin || p2_vector[2] > bbox$ymax
    ) {
      warning("The second point is outside the traffic network range, distance calculation may not be possible.")
    }

    # 1. Convert sf object to sfnetwork
    # `st_network_cost` calculates edge lengths, ensuring each edge has a length attribute
    sf_network <- as_sfnetwork(network_map_proj, directed = FALSE) %>%
      activate("edges") %>%
      mutate(edge_length = st_length(geometry))

    # 2. Find the nearest network node to p1 and p2
    nodes <- sf_network %>% activate("nodes") %>% st_as_sf()
    start_node_index <- st_nearest_feature(p1_proj, nodes)
    end_node_index <- st_nearest_feature(p2_proj, nodes)

    # Check if nodes were successfully found
    if (is.na(start_node_index) || is.na(end_node_index)) {
      warning("Could not find network nodes adjacent to one or both points, distance calculation may not be possible.")
      return(tibble(p1_to_node = units::set_units(Inf, "km"), 
                    node_to_node = units::set_units(Inf, "km"), 
                    node_to_p2 = units::set_units(Inf, "km")))
    }
    
    # 3. Calculate the straight-line distance from p1 to the nearest node
    start_node_geom <- nodes[start_node_index, ]$geometry
    # Ensure st_distance returns a numeric value and set units
    distance_p1_to_node <- st_distance(p1_proj, start_node_geom)[1,1] %>% units::set_units("km")
    
    # 4. Calculate the straight-line distance from p2 to the nearest node  
    end_node_geom <- nodes[end_node_index, ]$geometry
    distance_node_to_p2 <- st_distance(p2_proj, end_node_geom)[1,1] %>% units::set_units("km")
    
    # 5. Calculate the shortest path distance between the two nodes on the network
    if (start_node_index == end_node_index) {
      # If it's the same node, network distance is 0
      distance_node_to_node <- units::set_units(0, "km")
    } else {
      shortest_path <- st_network_paths(
        sf_network,
        from = start_node_index,
        to = end_node_index,
        weights = "edge_length" , # Use the previously calculated edge length as weight
        type = path_type # Pass the path_type parameter here
      )
      
      # 6. Check if a path was found
      if (length(shortest_path$edge_paths[[1]]) == 0) {
        warning("No connected path between the two points.")
        distance_node_to_node <- units::set_units(Inf, "km")
      } else {
        # Calculate network path length
        edge_indices <- shortest_path$edge_paths[[1]]
        edges_data <- sf_network %>% activate("edges") %>% st_as_sf()
        # Ensure that the sum here has units and is converted to km
        distance_node_to_node <- sum(edges_data$edge_length[edge_indices]) %>% units::set_units("km")
      }
    }

    # 7. Return the three distances
    return(tibble(
      p1_to_node = distance_p1_to_node,
      node_to_node = distance_node_to_node, 
      node_to_p2 = distance_node_to_p2
    ))
  }, error = function(e) {
    # If any error occurs within the function, catch it and return a tibble with Inf values
    warning(paste("calculate_network_distance function execution error:", e$message))
    return(tibble(p1_to_node = units::set_units(Inf, "km"), 
                  node_to_node = units::set_units(Inf, "km"), 
                  node_to_p2 = units::set_units(Inf, "km")))
  })
}