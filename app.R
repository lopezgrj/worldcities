library(shiny)
library(sf)
library(dplyr)
library(leaflet)

gpkg_path <- "data/worldcities.gpkg"
layer_name <- "worldcities"

if (!file.exists(gpkg_path)) {
  stop(sprintf("GeoPackage not found at %s. Run R/import_worldcities_to_gpkg.R first.", gpkg_path))
}

worldcities <- sf::st_read(gpkg_path, layer = layer_name, quiet = TRUE)

# Keep only non-empty geometries for robust map rendering.
worldcities <- worldcities[!sf::st_is_empty(worldcities), ]

if (!("population" %in% names(worldcities))) {
  worldcities$population <- NA_real_
}

worldcities$population <- suppressWarnings(as.numeric(worldcities$population))
max_pop_millions <- if (all(is.na(worldcities$population))) 0 else ceiling(max(worldcities$population, na.rm = TRUE) / 1e6)

if (!("city" %in% names(worldcities))) {
  worldcities$city <- "Unknown"
}

if (!("country" %in% names(worldcities))) {
  worldcities$country <- "Unknown"
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      html, body {
        width: 100%;
        height: 100%;
        margin: 0;
        padding: 0;
      }
      .container-fluid {
        width: 100%;
        max-width: none;
        height: 100vh;
        margin: 0;
        padding: 10px 14px 10px 14px;
      }
      #map {
        direction: ltr;
      }
      #map_wrap {
        height: calc(100vh - 150px);
      }
      #map_wrap .leaflet,
      #map_wrap .leaflet-container {
        height: 100% !important;
      }
      "
    ))
  ),
  titlePanel("World Cities Interactive Map"),
  fluidRow(
    column(
      width = 9,
      sliderInput(
        "min_pop",
        "Minimum population",
        min = 0,
        max = max_pop_millions,
        value = 0,
        step = 1,
        post = "M",
        pre = "",
        sep = ","
      )
    ),
    column(
      width = 3,
      checkboxInput("show_labels", "Show city labels", value = FALSE),
      helpText("Tip: Use zoom + pan to explore regions. Click a point for details.")
    )
  ),
  div(id = "map_wrap", leafletOutput("map", width = "100%", height = "100%"))
)

server <- function(input, output, session) {
  filtered <- reactive({
    data <- worldcities
    if (all(is.na(data$population))) {
      return(data)
    }
    data %>%
      filter(!is.na(population) & population >= (input$min_pop * 1e6))
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      ) %>%
      setView(lng = 10, lat = 20, zoom = 2)
  })

  observe({
    data <- filtered()

    wiki_city <- utils::URLencode(as.character(data$city), reserved = TRUE)
    wiki_url <- paste0("https://en.wikipedia.org/wiki/", wiki_city)

    popup_text <- paste0(
      "<b><a href='", wiki_url, "' target='_blank' rel='noopener noreferrer'>", data$city, "</a></b><br>",
      "Country: ", data$country, "<br>",
      "Population: ", ifelse(is.na(data$population), "NA", format(data$population, big.mark = ","))
    )

    map_obj <- leafletProxy("map", data = data) %>%
      clearMarkers()

    if (isTRUE(input$show_labels)) {
      map_obj <- map_obj %>%
        addCircleMarkers(
          radius = 6,
          stroke = FALSE,
          fillOpacity = 0.7,
          popup = popup_text,
          label = ~city
        )
    } else {
      map_obj <- map_obj %>%
        addCircleMarkers(
          radius = 6,
          stroke = FALSE,
          fillOpacity = 0.7,
          popup = popup_text
        )
    }
  })
}

shinyApp(ui, server)
