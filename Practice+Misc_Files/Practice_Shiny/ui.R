#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(scales)
library(viridis)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body, .container-fluid, .well {
        background-color: #1e1e1e;   /* dark gray/black background */
        color: white;                /* text color */
      }
      .shiny-input-container {
        color: white;                /* input text color */
      }
      .irs--shiny .irs-bar, 
      .irs--shiny .irs-bar-edge, 
      .irs--shiny .irs-single {
        background-color: #444;      /* slider bar color */
      }
    "))
  ),
  
  fluidRow( # first title
    column(12, align = "center",
    titlePanel("Rent-to-Income Ratio by State"))
  ),
  
  
  fluidRow( # slider
    column(12,
           style = "display: flex; justify-content: center; margin-bottom: 10px;",
           sliderInput(
             "year_map", "Select Year:",
             min = 2005, max = 2023,
             value = 2005, step = 1, sep = ""
           )
    )
  ),
  
  fluidRow( #map
    column(12, offset = 1,
           style = "display: flex; justify-content: center; width: 85%;",
           plotOutput("rent_map", width = "100%", height = "600px")
  )),
  
    
    column(12,
          style = "display: flex; justify-content: center; width: 50%; height: 50%",
          shinyWidgets::pickerInput(
          inputId = "state_select",
          label = "Add a state to compare (optional):",
          choices = NULL,  # will be updated from server
          selected = NULL,
          multiple = TRUE,  # single selection
          options = list (
            `max-options` = 1,
            `live-search` = TRUE
            ),
          width = "200px"
    )
  ),
    
  
  fluidRow( #bar graph
    column(12, 
           offset = 2,
           style = "display: flex; justify-content: center; width: 75%;",
           plotlyOutput("top_bottom_bar", height = "400px")
    )
    ),
  
  
  
  titlePanel("State vs US Education Estimates"),
  
  # First row: Year slider + State selector
  fluidRow(
    column(
      width = 6,
      sliderInput("year",
                  "Select Year:",
                  min = 2005,
                  max = 2023,
                  value = 2005,
                  step = 1,
                  sep = "")
    ),
    
    column(
      width = 6,
      pickerInput(
        inputId = "edu_states",
        selected = "Washington",
        label = "Select up to 4 States:",
        choices = NULL,
        multiple = TRUE,
        options = list(
          `max-options` = 4,
          `live-search` = TRUE
        )
      )
    ),
  
  # Second row: Plot
  fluidRow(
    column(
      width = 12,
      plotlyOutput("barplot")
    )
  )
  
  
  
  )
  )  

