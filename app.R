library(shiny)
library(shiny.semantic)
library(tidyverse)
library(leaflet)
library(shinybusy)


# Reading data globaly (transversal within simultaneous users)
dt <- read_csv("data/ships.csv", n_max = 10000)
# dt0 <- read_csv("data/ships.csv")

# Loading module functions to global environment
source("global.R")



ui <- semanticPage(add_busy_bar(color = "alicewhite"),
                   div(class = "ui raised segment", 
                       style = "background: #f6f6f6; border-radius: 0.5rem; height: 60vh; padding: 2rem; position: fixed; left: 2rem; opacity: 0.8; top: 7rem; width: 370px; z-index: 10;",
                       # h1(class = "ui icon header", icon("ship"), div(class = "content", "Marine data")),
                       h2(class = "ui header", icon("small ship"), div(class = "content", "Marine data")),
                       # h1(class = "ui header", "Marine data"),
                       dropdown_pairUI("dropdown", dt),
                       br(),
                       br(),
                       toggle("all_obs", "Show all observations", TRUE),
                       uiOutput("information")),
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
    req(in_0())
    d0 <- dt %>%
      filter(ship_type %in% in_0()[1],
             SHIP_ID %in% as.numeric(in_0()[2]),
             is_parked %in% 0) %>%
      arrange(DATETIME)
    if (nrow(d0) > 0) {
      d1 <- map_df(1:(nrow(d0) - 1),
                   ~data.frame(row = .x,
                               a = (sin((d0$LAT[.x + 1] - d0$LAT[.x]) / 2) ^ 2) + (cos(d0$LAT[.x]) * cos(d0$LAT[.x + 1]) * (sin((d0$LON[.x + 1] - d0$LON[.x]) / 2) ^ 2))) %>%
                     mutate(c = 2 * atan2(a ^ (1/2),  (1 - a) ^ (1/2)),
                            d = 6378000 * c))
      d2 <- d1 %>% filter(d %in% max(d)) %>% filter(row %in% max(row))
      list(d0, d0[c(d2$row, d2$row + 1), ])
    } else {
      NULL
    }
    
  })
  
  
  # Rendering routes of selected vessel
  observe({
    
    req(filtered_data())
    assign("p0", filtered_data(), envir = globalenv())
    
    l0 <- leafletProxy("map")  %>%
      clearMarkers() %>%
        clearShapes() 
    if (input$all_obs) {
      l0 <- l0 %>%
        addCircles(lng = filtered_data()[[1]]$LON, lat = filtered_data()[[1]]$LAT, label = 1:nrow(filtered_data()[[1]]), color = "#bb6b00", opacity = 0.7) %>%
        addPolylines(lng = filtered_data()[[1]]$LON, lat = filtered_data()[[1]]$LAT, color = "#bb6b00", opacity = 0.7)
    }
    l0 %>%
      addCircles(lng = filtered_data()[[2]]$LON, lat = filtered_data()[[2]]$LAT, label = 1:nrow(filtered_data()[[2]]), color = "#351c6a", opacity = 0.9) %>%
      addPolylines(lng = filtered_data()[[2]]$LON, lat = filtered_data()[[2]]$LAT, color = "#351c6a", opacity = 0.9) %>% 
      flyTo(lng = filtered_data()[[2]]$LON[1], lat = filtered_data()[[2]]$LAT[1], 10)
      
  })
  
})



shinyApp(ui = ui, server = server)