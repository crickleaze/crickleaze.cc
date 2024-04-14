# polyline_to_df ------------------------------------------------
polyline_to_df <- function(polyline) {
  library(googlePolylines)
  decode(polyline) -> dp
  return(dp[[1]])
}

# map_polylines -------------------------------------------------
map_polylines <- function(polylines) {
  library(leaflet)
  nrow(polylines) -> num_lines
  base <- leaflet() |> addTiles()
  for (i in 1:num_lines) {
    polyline_to_df(unlist(polylines[i, "map.polyline"])) -> coords
    if (polylines[i, "type"] == "Ride") {
      this_col <- "dodgerblue"
    } else {
      this_col <- "hotpink"
    }
    # message("km away ", polylines[i, "km_away"])
    base |> addPolylines(lng = coords$lon, lat = coords$lat, color = this_col) -> base
  }
  return(base)
}

# how_far_from_home --------------------------------------------
how_far_from_home <- function(polyline) {
  
  polyline_to_df(polyline) -> df
  dist <- haversine(mean(df$lat), mean(df$lon), 50.8964, -3.03738)
  return(dist)
}

# haversine -----------------------------------------------------------

haversine <- function(lat1, lon1, lat2, lon2) {
  # Earth's radius in kilometers
  R <- 6371
  
  # Convert degrees to radians
  phi1 <- lat1 * (pi / 180)
  phi2 <- lat2 * (pi / 180)
  delta_phi <- (lat2 - lat1) * (pi / 180)
  delta_lambda <- (lon2 - lon1) * (pi / 180)
  
  # Haversine formula
  a <- sin(delta_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Distance in kilometers
  d <- R * c
  
  return(d)
}

# pending_df to results ---------------------------------------

pending_to_results <- function(pending_df) {
  library(tidyverse)
  pending_df |> 
    filter(athlete.id == 301194, map.polyline != "") |> 
    rowwise() |> 
    mutate(km_away = how_far_from_home(map.polyline)) |> 
    filter(km_away <= 100) |> 
    select(type, map.polyline, km_away) -> results
  return(results)
}