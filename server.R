library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)

data_loading <- function() {
  ships <<- readRDS("ships_calculated.Rds")
  choices_table <<- ships %>% dplyr::select(ship_type, SHIP_ID)
}

data_loading()


server <- shinyServer(function(input, output, session) {
  observe({
    updateSelectInput(session,
                      inputId = "vessel",
                      choices = unique(choices_table[choices_table$ship_type == input$type, "SHIP_ID"]))
  })
  
  get_vessel_data <- function() {
    ships %>% filter(SHIP_ID == input$vessel) ->> vessel
  }
  
  output$distance <- renderText({
    get_vessel_data()
    paste("The vessel travelled", vessel$dist, "meters")
  })
  
  output$dates <- renderText({
    get_vessel_data()
    paste("The vessel travelled between",
          vessel$prevDATE,
          "and",
          vessel$DATETIME)
  })
  
  output$map_all <- renderLeaflet({
    map <- data.frame(
      lat = c(ships$prevLAT, ships$LAT),
      long = c(ships$prevLON, ships$prevLON)
    )
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = map,
                   lng = ~ long,
                   lat = ~ lat)
  })
  
  output$map <- renderLeaflet({
    get_vessel_data()
    map <- data.frame(
      lat = c(vessel$prevLAT, vessel$LAT),
      long = c(vessel$prevLON, vessel$prevLON)
    )
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = map,
                   lng = ~ long,
                   lat = ~ lat)
    
  })
  
  output$top_vessels <- renderPlotly({
    top_ships <- ships %>% arrange(desc(dist)) %>% head(10)
    top_ships$dist_km <- round(top_ships$dist / 1000)
    ggplotly(
      ggplot(top_ships, mapping = aes(
        x = reorder(SHIP_ID, dist_km),
        y = dist_km,
        fill = dist_km
      )) +
        geom_bar(stat = "identity") +
        xlab("Vessel ID") +
        ylab("Distance (km)") +
        coord_flip()
    )
  })
  
  output$top_types <- renderPlotly({
    av_dist <- ships %>% group_by(ship_type) %>%
      summarise(dist_km = round(mean(dist)))
    av_dist$dist_km <- round(av_dist$dist_km / 1000)
    ggplotly(
      ggplot(av_dist, mapping = aes(
        x = reorder(ship_type, dist_km),
        y = dist_km,
        fill = dist_km
      )) +
        geom_bar(stat = "identity") +
        xlab("Vessel type") +
        ylab("Distance (km)") +
        coord_flip()
    )
  })
  
})