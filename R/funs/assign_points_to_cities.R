#' Assign Transportation Network Points to Cities
#'
#' This function performs spatial assignment of transportation network points 
#' (such as railway stations or highway toll stations) to their corresponding 
#' cities using spatial intersection. Points that don't fall within any city 
#' boundary are assigned to the nearest city using nearest neighbor analysis.
#'
#' @param points_sf An sf object containing transportation network points with 
#'   POINT geometries. This could be railway stations, highway toll stations, 
#'   or any other transportation infrastructure points.
#' @param cities_sf An sf object containing city administrative boundaries with 
#'   POLYGON or MULTIPOLYGON geometries. Must contain at least `province` and 
#'   `citychn` columns for administrative identification.
#'
#' @return An sf object with the same points as input, but enhanced with city 
#'   assignment information:
#'   \describe{
#'     \item{original columns}{All original columns from the input points_sf}
#'     \item{province}{Province name where each point is located}
#'     \item{citychn}{City name (in Chinese) where each point is located}
#'     \item{assignment_method}{Method used for assignment:
#'       \itemize{
#'         \item "spatial_intersection": Point falls within city boundary
#'         \item "nearest_neighbor": Point assigned to closest city
#'       }
#'     }
#'   }
#'
#' @details
#' The function uses a two-step assignment process:
#' 
#' \strong{Step 1: Spatial Intersection}
#' Uses \code{\link[sf]{st_join}} to assign points to cities based on spatial 
#' containment. Points that fall within city boundaries are directly assigned.
#' 
#' \strong{Step 2: Nearest Neighbor Assignment}
#' For points that don't fall within any city boundary (resulting in NA values), 
#' the function uses \code{\link[sf]{st_nearest_feature}} to find the closest 
#' city and assigns the point to that city.
#' 
#' This two-step approach ensures complete coverage while maintaining spatial 
#' accuracy where possible. Points near administrative boundaries or in rural 
#' areas between cities are handled gracefully.
#'
#' @examples
#' \dontrun{
#' # Assign railway stations to cities
#' railway_stations_with_cities <- assign_points_to_cities(
#'   railway_station_points, 
#'   merged_sf_city
#' )
#' 
#' # Assign highway toll stations to cities  
#' highway_tolls_with_cities <- assign_points_to_cities(
#'   highway_roll_points, 
#'   merged_sf_city
#' )
#' 
#' # Check assignment results
#' assignment_summary <- railway_stations_with_cities |>
#'   st_drop_geometry() |>
#'   count(assignment_method, province)
#' }
#'
#' @note
#' \itemize{
#'   \item Input point and city datasets should use the same CRS
#'   \item The cities_sf must contain `province` and `citychn` columns
#'   \item Large datasets may require significant processing time
#'   \item Consider the appropriateness of nearest neighbor assignment for your use case
#' }
#'
#' @seealso 
#' \code{\link[sf]{st_join}} for spatial joins,
#' \code{\link[sf]{st_nearest_feature}} for nearest neighbor analysis
#'
#' @export
assign_points_to_cities <- function(points_sf, cities_sf) {
  
  # Step 1: Spatial join to assign points to containing cities
  points_with_cities <- st_join(points_sf, cities_sf)
  
  # Step 2: Identify points without city assignment
  points_without_city <- points_with_cities |>
    filter(is.na(province) | is.na(citychn))
  
  cat("Number of points not assigned to any city:", nrow(points_without_city), "\n")
  
  # Step 3: Handle unassigned points using nearest neighbor
  if (nrow(points_without_city) > 0) {
    # Find nearest city for unassigned points
    nearest_cities <- st_nearest_feature(points_without_city, cities_sf)
    
    # Assign nearest city information
    points_without_city_assigned <- points_without_city |>
      mutate(
        province = cities_sf$province[nearest_cities],
        citychn = cities_sf$citychn[nearest_cities],
        assignment_method = "nearest_neighbor"
      )
    
    # Combine with properly assigned points
    points_properly_assigned <- points_with_cities |>
      filter(!is.na(province) & !is.na(citychn)) |>
      mutate(assignment_method = "spatial_intersection")
    
    # Final complete assignment
    points_complete <- bind_rows(
      points_properly_assigned,
      points_without_city_assigned
    )
    
    cat("Complete assignment summary:\n")
    assignment_summary <- points_complete |>
      st_drop_geometry() |>
      count(assignment_method)
    print(assignment_summary)
    
  } else {
    points_complete <- points_with_cities |>
      mutate(assignment_method = "spatial_intersection")
  }
  
  return(points_complete)
}