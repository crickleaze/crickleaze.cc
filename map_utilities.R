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
    # message("km away ", polylines[i, "km_away"])
    base |> addPolylines(lng = coords$lon, lat = coords$lat, color = "dodgerblue") -> base
  }
  return(base)
}