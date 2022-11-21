#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tmap)
library(DT)
library(dplyr)
data <- readRDS("data.RDS")
# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Élection au Québec - Comparaison avec 2022."),
  sidebarPanel(
    selectInput("parti", "Parti", choices = c("CAQ", "PLQ", "QS", "PQ", "PCQ"), selected = "CAQ"),
    selectInput("annee", "Année", choices = c("2018", "2022", "evolution"), selected = "2022"),
    width = 2
  ),

  # Sidebar with a slider input for number of bins


  # Show a plot of the generated distribution
  mainPanel(
    tmapOutput("partiMap", height = "500"),
    dataTableOutput("table"),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
  output$table <- renderDT({
    DT::datatable(sf::st_drop_geometry(data), filter = "top", )
  })


  output$partiMap <- renderTmap({
    data |>
      tm_shape() +
      tm_polygons(
        col = "tauxVote2022_CAQ",
        zindex = 401,
        alpha = 0.5,
        border.alpha = 0.2,
        palette = "BuPu")
      # ) +
      #   tm_view(
      #       set.view = 6,
      #       set.bounds = c(-85, 43, -50, 58)
      #   )
  })
  
  observe({
      if (input$annee == "evolution") {
          stat <- paste0("augmentation_", input$parti)
      } else {
          stat <- paste0("tauxVote", input$annee, "_", input$parti)
      }
      if (input$annee == "evolution") {
          palette <- NULL
      } else {
          palette <- "BuPu"
      }
      tmapProxy("partiMap", session, {
          tm_remove_layer(401) +
              tm_shape(data) +
              tm_polygons(
                  col = stat,
                  alpha = 0.5,
                  border.alpha = 0.2,
                  palette = palette
              ) 
      })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
