library(leaflet)
library(geosphere)
library(dplyr)
library(htmlwidgets)

bog <- c(-74.1469, 4.7016)

# --- Direct non-stop international routes from BOG ---
direct <- tribble(
  ~city,                   ~lon,       ~lat,
  # Canada
  "Montreal",              -73.741,    45.457,
  "Toronto",               -79.631,    43.678,
  # USA
  "Atlanta",               -84.428,    33.640,
  "Boston",                -71.005,    42.365,
  "Chicago O'Hare",        -87.905,    41.979,
  "Dallas-Fort Worth",     -97.040,    32.897,
  "Fort Lauderdale",       -80.153,    26.072,
  "Houston",               -95.279,    29.645,
  "Miami",                 -80.290,    25.795,
  "Newark",                -74.175,    40.690,
  "New York JFK",          -73.778,    40.641,
  "Orlando",               -81.309,    28.429,
  "Tampa",                 -82.532,    27.975,
  "Washington Dulles",     -77.456,    38.944,
  # Mexico
  "Cancún",                -86.877,    21.036,
  "Mexico City (NAICM)",   -99.072,    19.436,
  "Mexico City (NAIF)",    -98.970,    19.980,
  "Guadalajara",          -103.310,    20.521,
  "Monterrey",            -100.107,    25.778,
  # Central America & Caribbean
  "Aruba",                 -70.015,    12.501,
  "Havana",                -82.410,    22.990,
  "San José (CR)",         -84.208,     9.994,
  "Curaçao",               -68.959,    12.189,
  "San Salvador",          -89.055,    13.441,
  "Guatemala City",        -90.527,    14.583,
  "Montego Bay",           -77.913,    18.503,
  "Panama City (PTY)",     -79.384,     9.071,
  "Panama City (BLB)",     -79.599,     8.915,
  "San Juan",              -66.003,    18.439,
  "Punta Cana",            -68.363,    18.567,
  "Santo Domingo",         -69.669,    18.429,
  # South America
  "Buenos Aires (EZE)",    -58.538,   -34.822,
  "Buenos Aires (AEP)",    -58.416,   -34.559,
  "Córdoba",               -64.208,   -31.323,
  "La Paz",                -68.193,   -16.513,
  "Santa Cruz",            -63.665,   -17.644,
  "Belém",                 -48.476,    -1.380,
  "Brasília",              -47.919,   -15.862,
  "Manaus",                -60.050,    -3.038,
  "Rio de Janeiro",        -43.243,   -22.809,
  "São Paulo",             -46.656,   -23.626,
  "Santiago",              -70.786,   -33.393,
  "Guayaquil",             -79.886,    -2.158,
  "Quito",                 -78.488,    -0.129,
  "Georgetown (Guyana)",   -58.254,     6.499,
  "Asunción",              -57.519,   -25.239,
  "Cusco",                 -71.938,   -13.535,
  "Lima",                  -77.114,   -12.022,
  "Montevideo",            -56.028,   -34.838,
  "Caracas",               -66.991,    10.601,
  "Porlamar",              -63.967,    10.913,
  "Valencia (VE)",         -67.928,    10.150,
  # Europe (direct)
  "Frankfurt",               8.570,    50.033,
  "Barcelona",               2.078,    41.297,
  "Madrid",                 -3.567,    40.494,
  "Paris CDG",               2.549,    49.009,
  "London Heathrow",         -0.461,   51.477
)

# --- Via-stop routes (technically one stop via another airport) ---
via_stop <- tribble(
  ~city,              ~lon,      ~lat,       ~via,
  "Amsterdam",         4.764,    52.308,    "via Cartagena",
  "Zürich",            8.548,    47.458,    "via Cartagena",
  "Istanbul",         28.815,    40.976,    "via Panama City",
  "Dubai",            55.364,    25.253,    "via Miami",
  "Doha",             51.608,    25.261,    "via Caracas"
)

# --- Helper: draw arcs ---
draw_arcs <- function(map, destinations, color, weight, opacity, dash = NULL) {
  for (i in seq_len(nrow(destinations))) {
    arc <- tryCatch(
      gcIntermediate(bog,
                     c(destinations$lon[i], destinations$lat[i]),
                     n = 80, addStartEnd = TRUE, breakAtDateLine = TRUE),
      error = function(e) NULL
    )
    if (is.null(arc)) next
    segs <- if (is.list(arc)) arc else list(arc)
    for (seg in segs) {
      args <- list(map, lng = seg[, 1], lat = seg[, 2],
                   color = color, weight = weight, opacity = opacity)
      if (!is.null(dash)) args$dashArray <- dash
      map <- do.call(addPolylines, args)
    }
  }
  map
}

# --- Build map ---
m <- leaflet(options = leafletOptions(
  zoomControl       = TRUE,
  attributionControl = FALSE,
  scrollWheelZoom   = "center"
)) |>
  addProviderTiles("CartoDB.DarkMatter") |>
  setView(lng = -30, lat = 15, zoom = 2)

# Draw route arcs
m <- draw_arcs(m, direct,   color = "#c8a415", weight = 0.9, opacity = 0.55)
m <- draw_arcs(m, via_stop, color = "#7ab8f5", weight = 0.7, opacity = 0.40,
               dash = "4,4")

# Destination markers — direct
m <- m |>
  addCircleMarkers(
    lng = direct$lon, lat = direct$lat,
    radius = 4, color = "#c8a415", fillColor = "#c8a415",
    fillOpacity = 0.85, stroke = FALSE,
    label = direct$city,
    labelOptions = labelOptions(
      style = list("color" = "white",
                   "background-color" = "rgba(0,0,0,0.75)",
                   "border" = "none", "padding" = "3px 7px",
                   "border-radius" = "3px", "font-size" = "12px"),
      direction = "top", offset = c(0, -6)
    ),
    group = "Direct"
  )

# Destination markers — via stop
m <- m |>
  addCircleMarkers(
    lng = via_stop$lon, lat = via_stop$lat,
    radius = 4, color = "#7ab8f5", fillColor = "#7ab8f5",
    fillOpacity = 0.75, stroke = FALSE,
    label = paste0(via_stop$city, " (", via_stop$via, ")"),
    labelOptions = labelOptions(
      style = list("color" = "white",
                   "background-color" = "rgba(0,0,0,0.75)",
                   "border" = "none", "padding" = "3px 7px",
                   "border-radius" = "3px", "font-size" = "12px"),
      direction = "top", offset = c(0, -6)
    ),
    group = "Via one stop"
  )

# Bogotá marker
m <- m |>
  addCircleMarkers(
    lng = bog[1], lat = bog[2],
    radius = 8, color = "#ffffff", fillColor = "#1a5c2a",
    fillOpacity = 1, stroke = TRUE, weight = 2,
    label = "Bogotá (BOG) — El Dorado",
    labelOptions = labelOptions(
      permanent = TRUE, direction = "right", offset = c(10, 0),
      style = list("color" = "white",
                   "background-color" = "rgba(26,92,42,0.9)",
                   "border" = "none", "font-weight" = "bold",
                   "padding" = "3px 8px", "border-radius" = "3px",
                   "font-size" = "12px")
    )
  )

# Legend
m <- m |>
  addLegend(
    position = "bottomright",
    colors  = c("#c8a415", "#7ab8f5"),
    labels  = c("Non-stop", "Via one stop"),
    title   = "Routes from BOG",
    opacity = 0.9
  )

saveWidget(m, "bog_map.html", selfcontained = TRUE,
           title = "Bogotá flight connections")
message("Done: bog_map.html")
