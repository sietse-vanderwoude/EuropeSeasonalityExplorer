library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

ui = fluidPage(
  titlePanel("European Disturbance Seasonality Explorer"),
  p("Welcome to a simple European disturbance explorer app. This app currently uses a 20km hexagonal grid size, and a 2ha minimum disturbance threshold. Select the disturbance type and seasonality metric you are interested in, and click on the map to see the entire and aggregated time series. The blue dots on the time series indicate the peaks."),
  p("Modality: Number of peaks where peak height is greatest in a moving window +- 60 days"),
  p("Mean Day: Distance from July 31st in days of mean day of disturbance"),
  p("Seasonality Index: Degree of seasonality compared to uniform distribution"),
  p("Area: Total area disturbed"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "tum_class", "Disturbance type",
        choices = setNames(1:3, c("Wind/Bark beetle","Fire","Harvest")),
        selected = 1
      ),
      selectInput(
        "metric", "Metric to visualize",
        choices = c(
          "Modality"                  = "modality",
          "Mean Day"             = "mean_doy_diff",
          "Seasonality Index"         = "si",
          "Area (ha)"                 = "total_area_ha"
        ),
        selected = "modality"
      )
    ),
    mainPanel(
      leafletOutput("map", height = 500),
      fluidRow(
        column(6, plotOutput("ts_plot")),
        column(6, plotOutput("doy_plot"))
      )
    )
  )
)


server = function(input, output, session, min_disturbed_area_ha = 2) {
  crs_leaflet <- 4326
  hex_grid_with_metrics <- readRDS("hex_app_data_full.rds")
  hex_doy <- readRDS("hex_app_data_doy.rds")
  hex_smoothed <- readRDS("hex_app_data_smoothed.rds")
  
  # 2) Reactive subset + de‐duplication for overlap metrics
  hex_mod <- reactive({
    df <- hex_grid_with_metrics %>%
      filter(tum_class == input$tum_class)
    
    df <- if (input$metric == "modality") {
      df %>%
        mutate(
          metric_val = factor(
            ifelse(modality == 1, "1 peak", ">1 peaks"),
            levels = c("1 peak", ">1 peaks")
          ),
          metric_label = as.character(metric_val)
        )
    } else {
      df %>%
        mutate(
          metric_val = as.numeric(.data[[input$metric]])*0.001,
          metric_label = round(metric_val, 2)
        )
    }
    
    df
  })
  
  # 3) Render Leaflet map
  output$map <- renderLeaflet({
    df <- hex_mod()
    if (input$metric == "modality") {
      pal <- colorFactor(palette = c("lightblue", "darkblue"), 
                         domain = levels(df$metric_val), 
                         na.color = "transparent")
    } else {
      dr <- df$metric_val
      pal <- colorNumeric("viridis", domain = dr, na.color = "transparent")
    }
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        layerId     = ~system_id,
        fillColor   = ~pal(metric_val),
        fillOpacity = 0.7,
        color       = "#555555",
        weight      = 0.5,
        label       = ~paste0("Hex: ", system_id,
                              "<br>", input$metric, ": ", metric_label)
      ) %>%
      addLegend(
        pal    = pal,
        values = df$metric_val,
        title  = input$metric,
        position = "bottomright"
      )
  })
  
  # 4) Time‐series plot (unchanged)
  selected_hex <- reactiveVal(NULL)
  observeEvent(input$map_shape_click, {
    selected_hex(input$map_shape_click$id)
  })
  output$ts_plot <- renderPlot({
    req(selected_hex())
    df <- hex_smoothed %>%
      filter(system_id == selected_hex(),
             tum_class    == input$tum_class)%>%
      mutate(date = as.Date(as.character(date_int), format = "%Y%m%d"))
    req(nrow(df) > 0)
    ggplot(df, aes(date, smoothed_px * 0.00001)) +
      geom_line(color = "black") +
      theme_minimal() +
      labs(
        title = paste("Time series for hex", selected_hex()),
        x = "Date", y = "Disturbed area [ha]"
      )
  })
  
  # 5) DOY profile (unchanged)
  output$doy_plot <- renderPlot({
    req(selected_hex())
    df <- hex_doy %>%
      filter(system_id == selected_hex(),
             tum_class    == input$tum_class)
    req(nrow(df) > 0)
    
    pm <-  hex_grid_with_metrics %>%
      filter(system_id == selected_hex(),
             tum_class    == input$tum_class)
    #peaks_raw <- as.integer(names(pm$raw_peak_list[[1]]))
    peaks     <- as.integer(names(pm$peak_list[[1]]))
    #troughs   <- as.integer(names(pm$trough_list[[1]]))
    #thresh    <- pm$quantile_val
    #exceed    <- pm$exceed_days[[1]]
    
    max_y <- max(df$doy_count * 0.00001, na.rm = TRUE)
    
    ggplot(df, aes(doy, doy_count  * 0.00001)) +
      geom_line(color = "darkred") +
      geom_point(data = df %>% filter(doy %in% peaks),
                 aes(doy, doy_count  * 0.00001),
                 color = "blue", size = 2) +
      scale_y_continuous(limits = c(0, NA)) +  # Ensures y-axis starts at 0
      theme_minimal() +
      labs(
        title = paste("DOY profile for hex", selected_hex()),
        x = "Day of Year", y = "Disturbed area [ha]"
      )
  })
}

shinyApp(ui = ui, server = server)