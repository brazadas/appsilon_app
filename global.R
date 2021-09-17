# Pair of dropdowns module
dropdown_pairUI <- function (id, data) {
  
  # Module's namespace  
  ns <- NS(id)
  tagList(h4(class = "header", "Select vessel type"),         
          dropdown_input(ns("vessel_type"), sort(unique(data$ship_type)), default_text = "Select vessel type"),
          h4(class = "header", "Select vessel"),
          dropdown_input(ns("vessel"), " ", default_text = "Select vessel"))
  
}

dropdown_pairServer <- function (id, data) {
  
  moduleServer(id, function (input, output, session) {
    # Updating vessel's dropdown depending on vessel type chosen
    observeEvent(input$vessel_type, {
      filtered_data <- data %>% filter(ship_type %in% input$vessel_type)
      update_dropdown_input(session, "vessel", sort(unique(filtered_data$SHIP_ID)), value = NULL)
    })
    # Returning dropdown selections
    reactive(c(input$vessel_type, input$vessel))
  })
  
}
