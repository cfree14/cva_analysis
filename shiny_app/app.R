
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Read data
scores <- readRDS(file.path(datadir, "cva_scores.Rds"))
subscores <- readRDS(file.path(datadir, "cva_subscores.Rds"))

# Read scripts
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Regions
regions <- sort(unique(subscores$region))



# User interface
################################################################################

# User interface
ui <- fluidPage(
  
  # Title
  titlePanel("U.S. Regional Climate Vulnerability Assessments"),
  
  # Background
  #h3("Background"),
  
  # Select region
  selectInput(inputId = "region", label = "Select a region:",
              choices = regions,  multiple = F),
  br(),
  
  # Plot scores
  h3("CVA scores"),
  plotOutput(outputId = "plot_scores", width=500, height=600),
  
  # Plot subscores
  h3("CVA subscores"),
  plotOutput(outputId = "plot_subscores", width=1500, height=850),
  
  # Plot subscore data quality
  h3("CVA subscore data quality"),
  plotOutput(outputId = "plot_subscore_quality", width=1500, height=850)
   
)


# Server
################################################################################

# Server
server <- function(input, output, session){
  
  # Plot subscores
  output$plot_subscores <- renderPlot({
    g <- plot_subscores(data = subscores,
                        region=input$region)
    g
  })
  
  # Plot subscores
  output$plot_scores <- renderPlot({
    g <- plot_scores(data = scores,
                     region=input$region)
    g
  })
  
  # Plot subscore data quality
  output$plot_subscore_quality <- renderPlot({
    g <- plot_subscore_quality(data = subscores,
                               region=input$region)
    g
  })
  
}

shinyApp(ui = ui, server = server)


# OLD CODE

# # Plot exposure scores
# h3("Exposure subscores"),
# plotOutput(outputId = "plot_subscores_exposure", width=750, height=750),
# 
# # Plot sensitivity scores
# h3("Sensitivity subscores"),
# plotOutput(outputId = "plot_subscores_sensitivity", width=750, height=750)

# # Plot exposure subscores
# output$plot_subscores_exposure <- renderPlot({
#   g <- plot_subscores_exposure(data = data,
#                                region=input$region)
#   g
# })
# 
# # Plot sensitivity subscores
# output$plot_subscores_sensitivity <- renderPlot({
#   g <- plot_subscores_sensitivity(data = data,
#                                region=input$region)
#   g
# })



