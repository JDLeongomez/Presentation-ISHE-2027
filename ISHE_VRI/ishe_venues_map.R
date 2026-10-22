library(leaflet)
library(dplyr)
library(tibble)
library(htmlwidgets)
library(rnaturalearth)
library(sf)

# --- Sedes anteriores de ISHE ---------------------------------------------
# Coordenadas aproximadas de cada ciudad sede. "congress" = Congreso
# internacional, "si" = Summer Institute. Bogotá 2027 se marca aparte
# (próxima sede, aún no realizada).

congress <- tribble(
  ~year, ~city,                     ~country,          ~lon,      ~lat,
  2026,  "Les Eyzies",              "France",           1.0167,   44.9333,
  2024,  "Humboldt (California)",   "United States",  -124.0771,   40.8665,
  2022,  "Würzburg",                "Germany",           9.9333,   49.7833,
  2018,  "Santiago",                "Chile",           -70.6693,  -33.4489,
  2016,  "Stirling",                "United Kingdom",   -3.9369,   56.1165,
  2014,  "Belém",                   "Brazil",          -48.4902,   -1.4558,
  2012,  "Viena",                   "Austria",          16.3738,   48.2082,
  2010,  "Madison",                 "United States",   -89.4012,   43.0731,
  2008,  "Bolonia",                 "Italy",            11.3426,   44.4949,
  2006,  "Detroit",                 "United States",   -83.0458,   42.3314,
  2004,  "Gante",                   "Belgium",           3.7174,   51.0543,
  2002,  "Montreal",                "Canada",          -73.5673,   45.5017,
  2000,  "Salamanca",               "Spain",            -5.6635,   40.9701,
  1998,  "Burnaby",                 "Canada",         -122.9199,   49.2488,
  1996,  "Viena",                   "Austria",          16.3738,   48.2082,
  1994,  "Toronto",                 "Canada",          -79.3832,   43.6532
)

summer_institute <- tribble(
  ~year, ~city,                      ~country,          ~lon,      ~lat,
  2025,  "Santiago",                 "Chile",           -70.9693,  -33.7489,
  2025,  "Viena",                    "Austria",          16.6738,   48.5082,
  2023,  "Detroit",                  "United States",   -83.3458,   42.6314,
  2023,  "České Budějovice",         "Czechia",          14.4743,   48.9747,
  2023,  "Recife",                   "Brazil",          -34.8770,   -8.0476,
  2019,  "Zadar",                    "Croatia",          15.2314,   44.1194,
  2017,  "Boise",                    "United States",  -116.2023,   43.6150,
  2015,  "Atenas",                   "Greece",           23.7275,   37.9838,
  2013,  "Ann Arbor",                "United States",   -83.7430,   42.2808,
  2011,  "Praga",                    "Czechia",          14.4378,   50.0755,
  2009,  "Maine (Orono)",            "United States",   -68.6712,   44.9007,
  2007,  "Andechs",                  "Germany",          11.1922,   47.9767
)

bogota_next <- tribble(
  ~year, ~city,     ~lon,     ~lat,
  2027,  "Bogotá", -74.0721,   4.7110
)

host_countries <- c(
  "Colombia", "France", "United States of America", "Germany", "Chile",
  "United Kingdom", "Brazil", "Austria", "Italy", "Belgium", "Canada",
  "Spain", "Czechia", "Croatia", "Greece"
)

# --- Mapa base (igual estética que bog_map.html: sin tiles externos) ------
world <- ne_countries(scale = "small", returnclass = "sf")
world_hosts <- world |> filter(name %in% host_countries)

label_style <- function(bg) {
  labelOptions(
    style = list("color" = "white", "background-color" = bg,
                 "border" = "none", "padding" = "3px 7px",
                 "border-radius" = "3px", "font-size" = "12px"),
    direction = "top", offset = c(0, -6)
  )
}

m <- leaflet(options = leafletOptions(
  zoomControl        = TRUE,
  attributionControl = FALSE,
  scrollWheelZoom    = "center"
)) |>
  addPolygons(data = world,
              fillColor = "#336b47", fillOpacity = 1,
              color = "#4a8a60", weight = 0.4,
              options = pathOptions(clickable = FALSE)) |>
  addPolygons(data = world_hosts,
              fillColor = "#c8a415", fillOpacity = 0.45,
              color = "#c8a415", weight = 1,
              options = pathOptions(clickable = FALSE)) |>
  htmlwidgets::onRender("function(el) { el.style.background = '#0f3d1e'; }") |>
  setView(lng = -10, lat = 25, zoom = 2)

m <- m |>
  addCircleMarkers(
    data = congress, lng = ~lon, lat = ~lat,
    radius = 5, color = "#c8a415", fillColor = "#c8a415",
    fillOpacity = 0.9, stroke = FALSE,
    label = ~paste0(year, " · ", city),
    labelOptions = label_style("rgba(26,46,15,0.85)"),
    group = "Congreso"
  ) |>
  addCircleMarkers(
    data = summer_institute, lng = ~lon, lat = ~lat,
    radius = 5, color = "#7ab8f5", fillColor = "#7ab8f5",
    fillOpacity = 0.85, stroke = FALSE,
    label = ~paste0(year, " · ", city),
    labelOptions = label_style("rgba(15,30,46,0.85)"),
    group = "Summer Institute"
  ) |>
  addCircleMarkers(
    data = bogota_next, lng = ~lon, lat = ~lat,
    radius = 9, color = "#ffffff", fillColor = "#1a5c2a",
    fillOpacity = 1, stroke = TRUE, weight = 2,
    label = "2027 · Bogotá (próximo)",
    labelOptions = labelOptions(
      permanent = TRUE, direction = "right", offset = c(10, 0),
      style = list("color" = "white",
                   "background-color" = "rgba(26,92,42,0.9)",
                   "border" = "none", "font-weight" = "bold",
                   "padding" = "3px 8px", "border-radius" = "3px",
                   "font-size" = "12px")
    )
  )

m <- m |>
  addLegend(
    position = "bottomleft",
    colors  = c("#c8a415", "#7ab8f5", "#1a5c2a"),
    labels  = c("Congreso", "Summer Institute", "Bogotá 2027 (próximo)"),
    opacity = 0.9
  )

saveWidget(m, "ishe_venues_map.html", selfcontained = TRUE,
           title = "Sedes anteriores de ISHE")
message("Done: ishe_venues_map.html")
