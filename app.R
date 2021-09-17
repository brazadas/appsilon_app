library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
library(shinybusy)


# Reading data globaly (transversal within simultaneous users)
dt <- read_csv("data/ships.csv")

# Loading module functions to global environment
source("global.R")



ui <- semanticPage(tags$body(tags$style(".ui.toggle.checkbox input:checked~label:before {background-color: #a7bccf !important;}
                                         .leaflet-tooltip {background-color: #f6f6f6 !important;
                                                           font-size: 15px !important;
                                                           opacity: 0.8 !important;
                                                           padding: 1rem;}")),
                   add_busy_bar(color = "#115385"),
                   div(class = "ui raised segment", 
                       style = "background: #f6f6f6; border-radius: 0.5rem; height: 600px; padding: 2rem; position: fixed; left: 2rem; opacity: 0.8; top: 7rem; width: 370px; z-index: 10;",
                       h2(class = "ui header", icon("small ship"), div(class = "content", "Marine data")),
                       dropdown_pairUI("dropdown", dt),
                       br(),
                       br(),
                       toggle("all_obs", "Show all observations", TRUE),
                       br(),
                       div(class = "ui divider"),
                       br(),
                       uiOutput("note")),
                   leafletOutput("map", height = "98%"))


server <- shinyServer(function (input, output, session) {
  
  # Rendering initial leaflet map
  output$map <- renderLeaflet(leaflet() %>% addTiles() %>% setView(13.6, 55.02, 7))
  
  # Calling module server part, retrieving dropdown selections
  in_0 <- dropdown_pairServer("dropdown", dt)
  
  # Filtering data according to dropdown selections and calculating distances using haversine formula
  # and taking earth radius as 6378000 meters
  filtered_data <- eventReactive(input$`dropdown-vessel`, {
    
    req(in_0())
    d0 <- dt %>%
      filter(ship_type %in% in_0()[1],
             SHIP_ID %in% as.numeric(in_0()[2]),
             is_parked %in% 0) %>%
      mutate(speed_mps = SPEED * 0.51444) %>%
      arrange(DATETIME)
    if (nrow(d0) > 0) {
      d1 <- map_df(1:(nrow(d0) - 1),
                   ~data.frame(row = .x,
                               a = (sin((d0$LAT[.x + 1] - d0$LAT[.x]) / 2) ^ 2) + (cos(d0$LAT[.x]) * cos(d0$LAT[.x + 1]) * (sin((d0$LON[.x + 1] - d0$LON[.x]) / 2) ^ 2))) %>%
                     mutate(c = 2 * atan2(a ^ (1/2),  (1 - a) ^ (1/2)),
                            d = 6378000 * c))
      # Selecting greatest distance and in case of multiple cases with same distances choosin the recent one
      d2 <- d1 %>% filter(d %in% max(d)) %>% filter(row %in% max(row))
      list(d0, d0[c(d2$row, d2$row + 1), ], d2$d)
    } else {
      NULL
    }
    
  })
  
  # Rendering note
  output$note <- renderUI({
    
    req(filtered_data())
    tagList(card(div(class = "content",
                     div(class = "header", in_0()[2]), 
                     div(class = "meta", in_0()[1]), 
                     div(class = "description",
                         HTML(paste0("Longest distance sailed within consecutive observations: <b>",
                                     format(filtered_data()[[3]], big.mark = ",", small.mark = "."), 
                                     " m </b>"))))),
            br(),
            div(class = "sub header", icon("circle", style = "color: #bb6b00;"), "Sailed paths"),
            div(class = "sub header", icon("circle", style = "color: #351c6a;"), "Longest sailed path"),
            br(),
            div(class = "meta", "*Hover over the points to find more information"))
    
  })
  
  # Rendering routes of selected vessel
  observe({
    
    req(filtered_data())
    l0 <- leafletProxy("map")  %>%
      clearMarkers() %>%
      clearShapes() 
    if (input$all_obs) {
      l0 <- l0 %>%
        addCircleMarkers(lng = filtered_data()[[1]]$LON, 
                         lat = filtered_data()[[1]]$LAT, 
                         label = paste0("<b> Date and time of the observation: </b>",
                                        filtered_data()[[1]]$DATETIME,
                                        "<br/><b> Speed in meters per second: </b>", 
                                        filtered_data()[[1]]$speed_mps) %>% map(~HTML(.x)),
                         color = "#bb6b00",
                         radius = 2,
                         fillOpacity = 0.7,
                         opacity = 0.7) %>%
        addPolylines(lng = filtered_data()[[1]]$LON, lat = filtered_data()[[1]]$LAT, color = "#bb6b00", opacity = 0.7)
    }
    l0 %>%
      addCircleMarkers(lng = filtered_data()[[2]]$LON,
                       lat = filtered_data()[[2]]$LAT, 
                       label = paste0("<b> Date and time of the observation: </b>", 
                                      filtered_data()[[2]]$DATETIME,
                                      "<br/><b> Speed in meters per second: </b>", 
                                      filtered_data()[[2]]$speed_mps) %>% map(~HTML(.x)), 
                       color = "#351c6a", 
                       radius = 2,
                       fillOpacity = 0.9,
                       opacity = 0.9) %>%
      addPolylines(lng = filtered_data()[[2]]$LON, lat = filtered_data()[[2]]$LAT, color = "#351c6a", opacity = 0.9) %>% 
      flyTo(lng = filtered_data()[[2]]$LON[1], lat = filtered_data()[[2]]$LAT[1], 10)
    
  })
  
})



shinyApp(ui = ui, server = server)