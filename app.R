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

if (!("region" %in% names(worldcities))) {
  if ("admin_name" %in% names(worldcities)) {
    worldcities$region <- worldcities$admin_name
  } else {
    worldcities$region <- "Unknown"
  }
}

worldcities$country <- as.character(worldcities$country)
worldcities$region <- as.character(worldcities$region)
worldcities$country[is.na(worldcities$country) | trimws(worldcities$country) == ""] <- "Unknown"
worldcities$region[is.na(worldcities$region) | trimws(worldcities$region) == ""] <- "Unknown"

country_choices <- c("All", sort(unique(worldcities$country)))
region_choices <- c("All", sort(unique(worldcities$region)))

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
      width = 4,
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
      selectInput("country_filter", "Country", choices = country_choices, selected = "All")
    ),
    column(
      width = 3,
      selectInput("region_filter", "Region", choices = region_choices, selected = "All")
    ),
    column(
      width = 2,
      checkboxInput("show_labels", "Show city labels", value = FALSE),
      helpText("Tip: Use zoom + pan to explore regions. Click a point for details.")
    )
  ),
  div(id = "map_wrap", leafletOutput("map", width = "100%", height = "100%"))
)

server <- function(input, output, session) {
  observeEvent(input$country_filter, {
    region_pool <- worldcities
    if (!is.null(input$country_filter) && input$country_filter != "All") {
      region_pool <- region_pool %>% filter(country == input$country_filter)
    }

    updated_regions <- c("All", sort(unique(region_pool$region)))
    selected_region <- input$region_filter
    if (is.null(selected_region) || !(selected_region %in% updated_regions)) {
      selected_region <- "All"
    }

    updateSelectInput(session, "region_filter", choices = updated_regions, selected = selected_region)
  }, ignoreInit = FALSE)

  filtered <- reactive({
    data <- worldcities %>%
      filter(
        input$country_filter == "All" | country == input$country_filter,
        input$region_filter == "All" | region == input$region_filter
      )

    if (!all(is.na(data$population))) {
      data <- data %>% filter(!is.na(population) & population >= (input$min_pop * 1e6))
    }

    data
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
    req(nrow(data) > 0)

    bbox <- sf::st_bbox(data)
    if (any(!is.finite(bbox))) {
      return()
    }

    leafletProxy("map") %>%
      fitBounds(
        lng1 = unname(bbox["xmin"]),
        lat1 = unname(bbox["ymin"]),
        lng2 = unname(bbox["xmax"]),
        lat2 = unname(bbox["ymax"])
      )
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
      clearMarkers() %>%
      clearGroup("city_labels") %>%
      addCircleMarkers(
        radius = 6,
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = popup_text
      )

    if (isTRUE(input$show_labels) && nrow(data) > 0) {
      coords <- sf::st_coordinates(data)
      data$lng <- coords[, 1]
      data$lat <- coords[, 2]

      map_obj %>%
        addLabelOnlyMarkers(
          data = data,
          lng = ~lng,
          lat = ~lat,
          label = ~city,
          group = "city_labels",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            style = list("font-size" = "11px", "font-weight" = "600")
          )
        )
    }
  })
}

shinyApp(ui, server)
