library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(stringr)
library(plotly)
library(shinydashboard)
library(DT)
library(leaflet.extras)
library(sf)
library(RColorBrewer)
library(shinyjqui)

# Set working dic

# Loading all datasets

# import stations data with passengers flow and coordinates
all_stations_info <- st_read("stations_with_city.geojson") %>%
  st_transform(4326) %>%
  mutate(Pax_annual = as.numeric(Pax_annual),
         Size_category = case_when(
           #setting the criterion of station sizes
           Pax_annual < 30000 ~ "Small",
           Pax_annual < 100000 ~ "Medium",
           TRUE ~ "Large"
         ))
#import cities geo info
cities <- st_read("victoria_cities.geojson")
station_monthly_flow <- read.csv("summary.csv") %>%
  group_by(Station_Name, Year, Month, Mode) %>%
  summarise(Monthly_Flow = sum(Total_Boardings) + sum(Total_Alightings),
            .groups = "drop") %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep="-")))

# matching function about matching station from different dataset with slightly different format
find_matching_station <- function(stop_name, station_names) {
  # remove unnecessary part and lowercase
  clean_stop_name <- tolower(str_replace(stop_name, " Railway Station| Station", ""))
  
  # extract name from brackets
  bracket_name <- str_extract(clean_stop_name, "\\(([^)]+)\\)")
  if (!is.na(bracket_name)) {
    clean_name <- str_trim(str_replace_all(bracket_name, "[\\(\\)]", ""))
    matches <- station_names[str_detect(tolower(station_names), fixed(clean_name))]
    if (length(matches) > 0) {
      return(matches[1])
    }
  }
  
  # If nothing matchs or no brackets, then try the whole name
  matches <- station_names[str_detect(tolower(station_names), fixed(clean_stop_name))]
  if (length(matches) > 0) {
    return(matches[1])
  }
  
  # If failed on matching whole name, then match the first word
  first_word <- word(clean_stop_name, 1)
  matches <- station_names[str_detect(tolower(station_names), fixed(first_word))]
  
  if (length(matches) == 1) {
    return(matches[1])
  } else if (length(matches) > 1) {
    # If matched multi stations then use the first two words
    first_two_words <- word(clean_stop_name, 1, 2)
    refined_matches <- matches[str_detect(tolower(matches), fixed(first_two_words))]
    if (length(refined_matches) > 0) {
      return(refined_matches[1])
    }
  }
  
  # If no matched at all, then return NA
  return(NA)
}

# combine and update the dataset
all_stations_data <- all_stations_info %>%
  rowwise() %>%
  mutate(matching_station = find_matching_station(Stop_name, station_monthly_flow$Station_Name)) %>%
  left_join(station_monthly_flow, by = c("matching_station" = "Station_Name"))

# Import the route datasets
read_shapefile <- function(file_path) {
  tryCatch({
    data <- st_read(file_path)
    # CRS = WGS84
    st_crs(data) <- 4326
    return(data)
  }, error = function(e) {
    stop(paste("Error reading shapefile:", file_path, "\n", e$message))
  })
}


train_stations <- read_shapefile("./train_route.shp")
route_names <- unique(train_stations$ROUTEUSSP)

tram_routes <- read_shapefile("PTV_METRO_TRAM_ROUTE.shp")
tram_route_names <- unique(tram_routes$ROUTESHTNM)
tram_stations <- st_read("PTV_METRO_TRAM_STOP.shp")

if (!inherits(tram_stations, "sf")) {
  tram_stations <- st_as_sf(tram_stations, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
}


# UI
ui <- dashboardPage(
  skin = "blue",  # change the color of dashboard
  dashboardHeader(title = span("Victoria Public Transport", style = "font-weight: 300")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Train Station", tabName = "dashboard", icon = icon("subway")),
      menuItem("Public Transport Routes", tabName = "public_transport_routes", icon = icon("route"))
    )
  ),
  #use css to deco the dashboard
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .box {
          box-shadow: 0 0 1px rgba(0,0,0,.125), 0 1px 3px rgba(0,0,0,.2);
          border-radius: 0.25rem;
        }
        .box-title {
          font-size: 1.1rem;
          font-weight: 400;
        }
        .selectize-input {
          border-radius: 0.25rem;
        }
        .btn-default {
          background-color: #f8f9fa;
          border-color: #ddd;
          color: #444;
        }
        /* Set height of map to fill available space */
        #map-box {
          height: calc(100vh - 200px); /* Dynamic height based on viewport */
        }
      "))
    ),
    tabItems(
      #dashbaors about station information
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 3,
                       box(
                         width = NULL,
                         title = "Filters",
                         status = "primary",
                         solidHeader = TRUE,
                         #filter has type, size, city, station dropselect and date range can filter the stations
                         selectInput("station_type", "Station Type", choices = c("All", unique(all_stations_info$Type))),
                         selectInput("size_category", "Size Category", choices = c("All", "Small", "Medium", "Large")),
                         selectInput("city", "City", choices = c("All", unique(all_stations_info$name))),
                         selectizeInput("selected_stations", "Selected Stations", choices = NULL, multiple = TRUE),
                         dateRangeInput("date_range", "Date Range", 
                                        start = min(station_monthly_flow$Date), 
                                        end = max(station_monthly_flow$Date)),
                         textInput("search_station", "Search Station", ""),
                         #can download the data
                         downloadButton("download_data", "Download Data", class = "btn-block"),
                         #reset every conditions
                         actionButton("reset_filters", "Reset Filters", class = "btn-block")
                       ),
                       #pie chart of size distribution
                       box(
                         width = NULL,
                         title = "Station Size Distribution",
                         status = "info",
                         solidHeader = TRUE,
                         plotlyOutput("station_size_pie", height = 300)
                       )
                ),
                column(width = 9,
                       #has three boxes on the top, showing total stations, annual passengers and the busiest station by the filter
                       fluidRow(
                         valueBoxOutput("total_stations_box", width = 4),
                         valueBoxOutput("total_passengers_box", width = 4),
                         valueBoxOutput("busiest_station_box", width = 4)
                       ),
                       box(
                         #Map of Australia and stations of Victoria 
                         width = NULL,
                         title = "Station Map",
                         status = "primary",
                         solidHeader = TRUE,
                         leafletOutput("map", height = 500)
                       ),
                       fluidRow(
                         # Flow chart of monthly passengers flow of selected stations
                         column(width = 6,
                                box(
                                  width = NULL,
                                  title = "Monthly Flow Chart",
                                  status = "info",
                                  solidHeader = TRUE,
                                  plotlyOutput("station_plot", height = 300)
                                )
                         ),
                         #Bar chart of top N monthly passengers of selected stations
                         column(width = 6,
                                box(
                                  width = NULL,
                                  title = "Top Monthly Flow for Selected Stations",
                                  status = "info",
                                  solidHeader = TRUE,
                                  selectInput("top_n", "Select Top N:", choices = c(3, 5), selected = 5),
                                  div(
                                    style = "overflow: auto; max-height: 500px;",
                                    plotlyOutput("top_N_monthly_flow")
                                  )
                                )
                         )
                       )
                )
              )
      ),
      
      # Route tab of tram and train
      tabItem(tabName = "public_transport_routes",
              fluidRow(
                #Filter of select transport type, include tram and train, can choose multi conditions, after chose the condition
                #will display the dropselection to choose the train line or tram line.
                column(width = 3,
                       box(
                         width = NULL,
                         title = "Select Transport Type and Routes",
                         solidHeader = TRUE,
                         checkboxGroupInput("transport_type", "Transport Type:",
                                            choices = c("Train", "Tram"),
                                            inline = TRUE),
                         conditionalPanel(
                           condition = "input.transport_type.includes('Train')",
                           selectizeInput("train_route", "Select Train Routes:",
                                          choices = c(route_names),
                                          multiple = TRUE,
                                          options = list(placeholder = 'Select train routes'))
                         ),
                         conditionalPanel(
                           condition = "input.transport_type.includes('Tram')",
                           selectizeInput("tram_route", "Select Tram Routes:",
                                          choices = c(tram_route_names),
                                          multiple = TRUE,
                                          options = list(placeholder = 'Select tram routes'))
                         )
                       )
                ),
                #Route map of selectecd trams or trains
                column(width = 9,
                       box(
                         width = NULL,
                         title = "Route Map",
                         solidHeader = TRUE,
                         leafletOutput("route_map", height=850)
                       )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter the station by condition
  filtered_stations <- reactive({
    data <- all_stations_data
    
    #Filter by station type
    if (!is.null(input$station_type) && input$station_type != "All") {
      data <- data %>% filter(Type == input$station_type)
    }
    #Filter by station type
    if (!is.null(input$size_category) && input$size_category != "All") {
      data <- data %>% filter(Size_category == input$size_category)
    }
    #Filter by city
    if (!is.null(input$city) && input$city != "All") {
      data <- data %>% filter(name == input$city)
    }
    
    # Search function
    if (!is.null(input$search_station) && input$search_station != "") {
      data <- data %>% 
        filter(str_detect(tolower(Stop_name), tolower(input$search_station)))
    }
    
    data %>% 
      distinct(Stop_name, .keep_all = TRUE) %>% 
      mutate(
        Stop_name = as.character(Stop_name),
        Pax_annual = as.numeric(Pax_annual)
      ) %>% 
      filter(!is.na(Stop_name) & !is.na(Pax_annual))
  })
  
  # The box is showing total stations 
  output$total_stations_box <- renderValueBox({
    stations_count <- nrow(filtered_stations())
    valueBox(
      value = as.character(stations_count),
      subtitle = "Total Stations",
      icon = icon("subway"),
      color = "blue"
    )
  })
  
  #The box is showing Annual passengers
  output$total_passengers_box <- renderValueBox({
    total_passengers <- sum(filtered_stations()$Pax_annual, na.rm = TRUE)
    valueBox(
      value = scales::comma(total_passengers),
      subtitle = "Annual Passengers",
      icon = icon("users-between-lines"),
      color = "green"
    )
  })
  
  #Filter the busiest station
  busiest_station <- reactive({
    filtered_stations() %>%
      arrange(desc(Pax_annual)) %>%
      slice(1)
  })
  
  #The box is showing the busiest station
  output$busiest_station_box <- renderValueBox({
    station <- busiest_station()
    
    if (nrow(station) > 0 && 
        !all(is.na(station$Stop_name)) && 
        !all(is.na(station$Pax_annual)) &&
        length(station$Stop_name) > 0 &&
        length(station$Pax_annual) > 0) {
      
      station_name <- as.character(station$Stop_name[1])
      if (station_name == "") {
        station_name <- "Unknown"
      }
      
      valueBox(
        station_name,
        "Busiest Station",
        icon = icon("building"),
        color = "yellow"
      )
    } else {
      valueBox(
        "N/A",
        "Busiest Station",
        icon = icon("building"),
        color = "yellow"
      )
    }
  })
  
  
  # Station map
  output$map <- renderLeaflet({
    stations_to_plot <- filtered_stations()
    
    if (nrow(stations_to_plot) > 0) {
      leaflet(stations_to_plot) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMarkers(
          lng = ~st_coordinates(geometry)[,1], 
          lat = ~st_coordinates(geometry)[,2],
          #Showing Stop name, annual passengers flow, city, station type. 
          label = ~lapply(paste("<strong>", Stop_name, "</strong>",
                                "<br>Annual Passengers:", scales::comma(Pax_annual),
                                "<br>City: ",name,
                                "<br>Type:", Type), HTML),
          layerId = ~Stop_name,
          clusterOptions = markerClusterOptions(
            spiderfyOnMaxZoom = TRUE,
            zoomToBoundsOnClick = TRUE
          ),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto",
            html = TRUE
          )
        )
    } else {
      #No result just display the map directly
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 144.9631, lat = -37.8136, zoom = 10) %>%
        addControl(
          html = "<h4>No stations match the current filters</h4>",
          position = "topright"
        )
    }
  })
  
  # Update the selectable stations
  observe({
    stations <- filtered_stations()$Stop_name
    updateSelectizeInput(session, "selected_stations", choices = stations, server = TRUE)
  })
  
  # Choose the station
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    selected <- input$selected_stations
    if (!(click$id %in% selected)) {
      selected <- c(selected, click$id)
      updateSelectizeInput(session, "selected_stations", selected = selected)
    }
  })
  
  # Plot the flow chart
  output$station_plot <- renderPlotly({
    req(input$selected_stations)
    
    selected_data <- all_stations_data %>%
      filter(Stop_name %in% input$selected_stations,
             #can filter by date
             Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      select(Stop_name, Date, Monthly_Flow, Year, Month, Mode, Type) %>%
      arrange(Date) %>%
      filter(!is.na(Date) & !is.na(Monthly_Flow) & is.finite(Monthly_Flow))
    
    if (nrow(selected_data) > 0) {
      p <- ggplot(selected_data, aes(x = Date, y = Monthly_Flow, group = interaction(Stop_name, Mode), color = Stop_name, linetype = Mode)) +
        geom_line(size = 1) +
        labs(title = "Monthly Flow for Selected Stations",
             x = "Date", y = "Monthly Flow") +
        scale_x_date(
          #shoew the x-axis
          date_breaks = "1 month",
          date_labels = "%Y-%m",
          expand = c(0.02, 0.1)
        ) +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill=NA, size=0.5)
        )
      #polt colour by line
      ggplotly(p, tooltip = c("x", "y", "colour", "linetype")) %>%
        layout(hoverlabel = list(bgcolor = "white"),
               hovermode = "closest") %>%
        config(displayModeBar = FALSE)
    } else {
      #if no data
      plot_ly() %>%
        add_annotations(
          x = 0.5,
          y = 0.5,
          text = "No data available for selected stations",
          showarrow = FALSE
        )
    }
  })
  
  # filter data of selected stations to rank the monthly passengers flow
  top_monthly_flow_data <- reactive({
    req(input$selected_stations)
    req(input$top_n)  # can choose top 3 or top 5
    
    all_stations_data %>%
      filter(Stop_name %in% input$selected_stations) %>%
      group_by(Stop_name) %>%
      top_n(as.integer(input$top_n), Monthly_Flow) %>%
      mutate(Month_Year = format(Date, "%Y-%m")) %>%
      arrange(Stop_name, desc(Monthly_Flow)) %>%
      mutate(Rank = row_number()) %>%
      ungroup()
  })
  # bar chart of selected stations
  output$top_N_monthly_flow <- renderPlotly({
    data <- top_monthly_flow_data()
    
    p <- ggplot(data, aes(x = reorder(Stop_name, -Monthly_Flow), y = Monthly_Flow, fill = factor(Rank))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Monthly_Flow), position = position_dodge(width = 0.9), vjust = -0.25) +
      scale_fill_brewer(palette = "Set3", name = "Rank") +
      labs(title = paste("Top", input$top_n, "Monthly Flow for Selected Stations"),
           x = "Station Name",
           y = "Monthly Flow") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()  # make eaiser to read the data
    
    ggplotly(p) %>%
      layout(barmode = "group",
             hoverlabel = list(bgcolor = "white"),
             showlegend = TRUE) %>%
      config(displayModeBar = FALSE)  # hide the toolbar 
  })
  
  # Calculate the amount of different station sizes by filter
  station_size_distribution <- reactive({
    data <- filtered_stations() %>%
      st_drop_geometry() %>%
      count(Size_category) %>%
      arrange(desc(n))
    
    print("Station size distribution data:")
    print(data)
    
    data
  })
  
  # plot the pie chart
  output$station_size_pie <- renderPlotly({
    data <- station_size_distribution()
    
    if (nrow(data) == 0) {
      return(plot_ly() %>% 
               add_annotations(
                 text = "No data available",
                 showarrow = FALSE,
                 font = list(size = 20)
               ))
    }
    #try-catch make application not crash
    tryCatch({
      plot_ly(data, labels = ~Size_category, values = ~n, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              # format and color of chart
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste(Size_category, ": ", n, " stations"),
              marker = list(colors = c("Small" = "#1f77b4", "Medium" = "#ff7f0e", "Large" = "#2ca02c"),
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE) %>%
        layout(title = list(text = "Station Size Distribution", font = list(size = 14)),
               margin = list(l = 20, r = 20, t = 40, b = 20),
               height = 300)
    }, error = function(e) {
      print(paste("Error in pie chart:", e$message))
      plot_ly() %>% 
        add_annotations(
          text = "Error generating pie chart",
          showarrow = FALSE,
          font = list(size = 20)
        )
    })
  })
  
  #download function into csn file
  output$download_data <- downloadHandler(
    filename = function() {
      paste("station_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      selected_data <- all_stations_data %>%
        filter(Stop_name %in% input$selected_stations,
               Date >= input$date_range[1],
               Date <= input$date_range[2]) %>%
        select(Stop_name, Date, Monthly_Flow, Mode, Type)
      
      write.csv(selected_data, file, row.names = FALSE)
    }
  )
  
  #filter routes and store into a list, make user can choose multi routes and types
  selected_routes <- reactive({
    routes <- list()
    stations <- list()
    
    if ("Train" %in% input$transport_type && length(input$train_route) > 0) {
      train_data <- train_stations %>%
        filter(ROUTEUSSP %in% input$train_route) %>%
        arrange(as.numeric(STOP_ID))
      routes$train <- train_data
      stations$train <- train_data
    }
    
    if ("Tram" %in% input$transport_type && length(input$tram_route) > 0) {
      tram_route_data <- tram_routes %>% filter(ROUTESHTNM %in% input$tram_route)
      tram_station_data <- tram_stations %>% filter(ROUTEUSSP %in% input$tram_route)
      routes$tram <- tram_route_data
      stations$tram <- tram_station_data
    }
    
    list(routes = routes, stations = stations)
  })
  
  #route map 
  output$route_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = mean(st_coordinates(train_stations)[,1]), 
              lat = mean(st_coordinates(train_stations)[,2]), 
              zoom = 10)
  })
  
  color_palette <- reactive({
    n_colors <- length(input$train_route) + length(input$tram_route)
    colorRampPalette(brewer.pal(8, "Set2"))(n_colors)
  })
  
  observe({
    #get the selected routes and colors
    route_data <- selected_routes()
    colors <- color_palette()
    
    #clear current elements on the map 
    leafletProxy("route_map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()
    
    legend_colors <- c()
    legend_labels <- c()
    
    # process the train route
    if (!is.null(route_data$routes$train)) {
      for (i in seq_along(input$train_route)) {
        route <- input$train_route[i]
        color <- colors[i]
        
        # filter the route 
        route_data_filtered <- route_data$routes$train %>% filter(ROUTEUSSP == route)
        station_data_filtered <- route_data$stations$train %>% filter(ROUTEUSSP == route)
        
        #plot stop and route on the map
        leafletProxy("route_map") %>%
          addPolylines(data = route_data_filtered,
                       lng = ~st_coordinates(geometry)[,1],
                       lat = ~st_coordinates(geometry)[,2],
                       color = color,
                       weight = 4, 
                       opacity = 0.8,
                       smoothFactor = 0.5) %>%
          addCircleMarkers(data = station_data_filtered,
                           lng = ~st_coordinates(geometry)[,1],
                           lat = ~st_coordinates(geometry)[,2],
                           radius = 6,
                           color = color,
                           fillColor = "white",
                           stroke = TRUE,
                           weight = 2,
                           fillOpacity = 1,
                           label = ~lapply(paste("<strong>", STOP_NAME, "</strong>",
                                          "<br>Stop ID:", STOP_ID,
                                          "<br>Ticket Zone:", TICKETZONE),HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px",
                             direction = "auto",
                             html= TRUE
                           ))
        
        legend_colors <- c(legend_colors, color)
        legend_labels <- c(legend_labels, paste("Train:", route))
      }
    }
    
    # process the tram route 
    if (!is.null(route_data$routes$tram)) {
      for (i in seq_along(input$tram_route)) {
        route <- input$tram_route[i]
        color <- colors[length(input$train_route) + i]
        # filter the route 
        route_data_filtered <- route_data$routes$tram %>% filter(ROUTESHTNM == route)
        station_data_filtered <- route_data$stations$tram %>% filter(ROUTEUSSP == route)
        
        #plot stop and route on the map
        leafletProxy("route_map") %>%
          addPolylines(data = route_data_filtered,
                       color = color,
                       weight = 5,
                       opacity = 0.8) %>%
          addCircleMarkers(data = station_data_filtered,
                           radius = 6,
                           color = color,
                           fillColor = "white",
                           stroke = TRUE,
                           weight = 2,
                           fillOpacity = 1,
                           label = ~lapply(paste("<strong>", STOP_NAME, "</strong>",
                                          "<br>Route:", ROUTEUSSP), HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px",
                             direction = "auto",
                             html = TRUE
                           ))
        
        legend_colors <- c(legend_colors, color)
        legend_labels <- c(legend_labels, paste("Tram:", route))
      }
    }
    
    if (length(legend_colors) > 0) {
      leafletProxy("route_map") %>%
        addLegend(position = "bottomright",
                  colors = legend_colors,
                  labels = legend_labels,
                  opacity = 1,
                  title = "Routes")
    }
  })

  # reset filter button to reset the map 
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "station_type", selected = "All")
    updateSelectInput(session, "size_category", selected = "All")
    updateSelectInput(session, "city", selected = "All")
    updateSelectizeInput(session, "selected_stations", selected = character(0))
    updateTextInput(session, "search_station", value = "")
    updateDateRangeInput(session, "date_range", 
                         start = min(station_monthly_flow$Date), 
                         end = max(station_monthly_flow$Date))
  })
}

# Run the app
shinyApp(ui, server)
