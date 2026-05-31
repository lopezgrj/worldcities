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

if (!("continent" %in% names(worldcities))) {
  worldcities$continent <- NA_character_
}

worldcities$country <- as.character(worldcities$country)
worldcities$continent <- as.character(worldcities$continent)

if (all(is.na(worldcities$continent) | trimws(worldcities$continent) == "") &&
    requireNamespace("rnaturalearth", quietly = TRUE)) {
  countries_ref <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    sf::st_drop_geometry()

  iso_lookup <- countries_ref %>%
    transmute(iso2 = toupper(as.character(iso_a2)), continent_lookup = as.character(continent)) %>%
    filter(!is.na(iso2), iso2 != "", !is.na(continent_lookup), continent_lookup != "") %>%
    distinct(iso2, .keep_all = TRUE)

  name_lookup <- bind_rows(
    countries_ref %>% transmute(country_key = tolower(as.character(name)), continent_lookup = as.character(continent)),
    countries_ref %>% transmute(country_key = tolower(as.character(name_long)), continent_lookup = as.character(continent)),
    countries_ref %>% transmute(country_key = tolower(as.character(formal_en)), continent_lookup = as.character(continent)),
    countries_ref %>% transmute(country_key = tolower(as.character(sovereignt)), continent_lookup = as.character(continent))
  ) %>%
    filter(!is.na(country_key), country_key != "", !is.na(continent_lookup), continent_lookup != "") %>%
    distinct(country_key, .keep_all = TRUE)

  derived_continent <- rep(NA_character_, nrow(worldcities))

  if ("iso2" %in% names(worldcities)) {
    iso_idx <- match(toupper(as.character(worldcities$iso2)), iso_lookup$iso2)
    derived_continent <- iso_lookup$continent_lookup[iso_idx]
  }

  missing_idx <- is.na(derived_continent) | trimws(derived_continent) == ""
  if (any(missing_idx)) {
    country_idx <- match(tolower(as.character(worldcities$country[missing_idx])), name_lookup$country_key)
    derived_continent[missing_idx] <- name_lookup$continent_lookup[country_idx]
  }

  replace_idx <- is.na(worldcities$continent) | trimws(worldcities$continent) == ""
  worldcities$continent[replace_idx] <- derived_continent[replace_idx]
}

worldcities$country[is.na(worldcities$country) | trimws(worldcities$country) == ""] <- "Unknown"
worldcities$continent[is.na(worldcities$continent) | trimws(worldcities$continent) == ""] <- "Unknown"

continent_choices <- c("All", sort(unique(worldcities$continent)))
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
      selectInput("continent_filter", "Continent", choices = continent_choices, selected = "All")
    ),
    column(
      width = 3,
      selectizeInput("country_filter", "Country", choices = NULL, selected = "All")
    ),
    column(
      width = 2,
      checkboxInput("show_labels", "Show city labels", value = FALSE),
      helpText("Tip: Use zoom + pan to explore continents and countries. Click a point for details.")
    )
  ),
  div(id = "map_wrap", leafletOutput("map", width = "100%", height = "100%"))
)

server <- function(input, output, session) {
  observeEvent(input$continent_filter, {
    country_pool <- worldcities
    if (!is.null(input$continent_filter) && input$continent_filter != "All") {
      country_pool <- country_pool %>% filter(continent == input$continent_filter)
    }

    updated_countries <- c("All", sort(unique(country_pool$country)))
    selected_country <- input$country_filter
    if (is.null(selected_country) || !(selected_country %in% updated_countries)) {
      selected_country <- "All"
    }

    updateSelectizeInput(
      session,
      "country_filter",
      choices = updated_countries,
      selected = selected_country,
      server = TRUE
    )
  }, ignoreInit = FALSE)

  filtered <- reactive({
    data <- worldcities %>%
      filter(
        input$continent_filter == "All" | continent == input$continent_filter,
        input$country_filter == "All" | country == input$country_filter
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

    if (nrow(data) > 0 && any(!is.na(data$population))) {
      pop_min <- min(data$population, na.rm = TRUE)
      pop_max <- max(data$population, na.rm = TRUE)

      if (isTRUE(all.equal(pop_min, pop_max))) {
        marker_fill <- rep("#08306b", nrow(data))
      } else {
        pop_palette <- colorNumeric(
          palette = c("#dbeaf7", "#08306b"),
          domain = c(pop_min, pop_max),
          na.color = "#bdbdbd"
        )
        marker_fill <- pop_palette(data$population)
      }
    } else {
      marker_fill <- rep("#5f9ea0", nrow(data))
    }

    map_obj <- leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      clearGroup("city_labels") %>%
      addCircleMarkers(
        radius = 6,
        stroke = TRUE,
        weight = 0.7,
        color = marker_fill,
        fillColor = marker_fill,
        fillOpacity = 0.8,
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
