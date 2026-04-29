library(shiny)

ui <- fluidPage(
  titlePanel("Rent vs Homeownership Dashboard (2005-2023)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        'region',
        'Select City',
        choices = c('National'),
        selected = 'National'
      ),
      sliderInput(
        'year_range',
        'Year Range',
        min = 2005,
        max = 2023,
        value = c(2005,2023),
        step = 1,
        sep = ''
      ),
      checkboxInput("adjust_inflation", 'Inflation-adjust values?', value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Rent vs Owner Cost',
                 plotOutput('rent_owner_plot')),
        tabPanel('Homeownership Trend',
                 plotOutput('homeownership_plot')),
        tabPanel('Inflation-Adjusted Rent Trend',
                 plotOutput('rent_inflation_plot'))
      )
    )
  )
)