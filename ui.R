library(shiny)
library(semantic.dashboard)
library(DT)
library(leaflet)
library(plotly)


data_loading <- function() {
  ships <<- readRDS("ships_calculated.Rds")
  choices_table <<- ships %>% dplyr::select(ship_type, SHIP_ID)
}

data_loading()


ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE, collapsed = TRUE, sidebarMenu()),
  dashboardBody(tabItems(
    selected = 1,
    tabItem(
      tabName = "main",
      fluidRow(
        box(
          width = 8,
          color = "blue",
          ribbon = TRUE,
          column(
            width = 8,
            img(
              src = 'https://i.ibb.co/fYdDP5k/1.png',
              width = "100%",
              height = "50%"
            )
          )
        ),
        box(
          width = 8,
          color = "blue",
          ribbon = TRUE,
          column(
            width = 8,
            selectInput(
              inputId = "type",
              label = "Select a vessel type",
              choices = unique(choices_table$ship_type),
              selected = c("Cargo"),
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL
            ),
            selectInput(
              inputId = "vessel",
              label = "Select a vessel",
              choices = unique(choices_table$SHIP_ID),
              selected = c("3615"),
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL
            )
          )
        )
      ),
      fluidRow(
        box(
          width = 8,
          title = "Longest distance - time",
          color = "blue",
          ribbon = TRUE,
          title_side = "top right",
          column(width = 8, textOutput("dates"))
        ),
        box(
          width = 8,
          title = "Longest distance - distance",
          color = "blue",
          ribbon = TRUE,
          title_side = "top right",
          column(width = 8, textOutput("distance"))
        )
      ),
      fluidRow(
        box(
          width = 8,
          title = "Longest distance  - map",
          color = "blue",
          ribbon = TRUE,
          title_side = "top right",
          column(width = 8, leafletOutput("map"))
        ),
        box(
          width = 8,
          title = "Longest distance map (all vessels)",
          color = "blue",
          ribbon = TRUE,
          title_side = "top right",
          column(width = 8, leafletOutput("map_all"))
        )
      ),
      fluidRow(
        box(
          width = 8,
          title = "Top vessels (according to max. distance)",
          color = "blue",
          ribbon = TRUE,
          title_side = "top right",
          column(width = 8, plotlyOutput("top_vessels"))
        ),
        box(
          width = 8,
          title = "Average max. distance per vessel type",
          color = "blue",
          ribbon = TRUE,
          title_side = "top right",
          column(width = 8, plotlyOutput("top_types"))
        )
      )
    )
  )),
  theme = "solar"
)