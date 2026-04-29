
#Loads libraries, if you need to add a library, then add it in here
pacman::p_load(shiny, shinyWidgets, bslib, ggplot2, tidyverse, plotly, scales, viridis)

#contains all the pages and stuff
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
    page_navbar(
    title = "Costly Living", #Title at the top left
    bg = "#2D89C8", #Background color of navigation bar (top bar), could change later
    inverse = TRUE, #Honestly don't know what this does at the moment
    
    #Each "nav_panel" is a page of the app that can be clicked between
    #Already laid out the spaces for each our things but I guess if
    #anyones got any issues with it, bring it up in the group chat
    #-Sebastian
    
    #Page containing overview of the project VVV
    nav_panel(title = "About", p(
      htmlOutput("text"),
      tags$a("https://github.com/UWB-Adv-Data-Vis/CostlyLiving", href = "https://github.com/UWB-Adv-Data-Vis/CostlyLiving"),
      tags$a("https://github.com/UWB-Adv-Data-Vis-2025-Autumn/costlyliving-hoa",href = "https://github.com/UWB-Adv-Data-Vis-2025-Autumn/costlyliving-hoa"),
      htmlOutput("text_2"),
      
      # first title
      fluidRow( 
        column(12, align = "center",
               titlePanel("Rent-to-Income Ratio by State"))
      ),
      
      # year slider
      fluidRow( 
        column(12,
               style = "display: flex; justify-content: center; margin-bottom: 10px;",
               sliderInput(
                 "year_map", "Select Year:",
                 min = 2005, max = 2023,
                 value = 2005, step = 1, sep = ""
               )
        )
      ),
      
      # map graph
      fluidRow( 
        column(12, offset = 1,
               style = "display: flex; justify-content: center; width: 85%;",
               plotOutput("rent_map", width = "100%", height = "600px")
        )),
      
      # top and bottom 5 bar graph state selector (Now using selectInput)
      fluidRow(
        column(12,
               style = "display: flex; justify-content: center; width: 50%; height: 50%",
               selectInput( # Changed from pickerInput
                 inputId = "state_select",
                 label = "Add States to Compare (optional):",
                 choices = NULL, # populated from server
                 multiple = TRUE, # Allows server-side max=1 enforcement
                 width = "200px"
               )
        )
      ),
      # bar graph
      fluidRow( 
        column(12, 
               offset = 2,
               style = "display: flex; justify-content: center; width: 75%;",
               plotlyOutput("top_bottom_bar", height = "400px")
        )
      ),
      
      )),
    
    #Page containing Oscars stuff VVV
    nav_panel(title = "Rent Burden", p(
      #Page content goes inside here
      fluidPage(
        titlePanel("Rent Burden Over Time"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput("city", "Choose a city:", choices = NULL)  # choices populated in server
          ),
          mainPanel(
            plotlyOutput("rentPlot", height = "400px"),
            tags$hr(),
            plotOutput("rentHeatmap", height = "500px", width = "100%")  # heatmap
          )
        )
      )
    )),
    
    #Page containing Connors Stuff VVV
    nav_panel(title = "Mortgage", p(
      #Page content goes inside here
      fluidPage(
        titlePanel("Rent vs Homeownership Dashboard (2005-2023)"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput('region', 'Select City',
                        choices = c('National'),
                        selected = 'National'),
            
            sliderInput('year_range', 'Year Range',
                        min = 2005, max = 2023,
                        value = c(2005, 2023),
                        step = 1, sep = ''),
            
            checkboxInput("adjust_inflation",
                          'Inflation-adjust values?',
                          value = TRUE)
          ),
          
          mainPanel(
            h3("Rent vs Owner Cost"),
            plotOutput('rent_owner_plot', height = "300px"),
            tags$hr(),
            
            h3("Homeownership Trend"),
            plotOutput('homeownership_plot', height = "300px"),
            tags$hr(),
            
            h3("Inflation-Adjusted Rent Trend"),
            plotOutput('rent_inflation_plot', height = "300px")
          )
        )
      )
      

    )),
    
    #Page containing Sebastians stuff VVV
    nav_panel(title = "Age", p(
      fluidPage( #Page that encapsulates everything
        titlePanel("Rent and Age Groups"), #makes big title up top with my topic
        tabsetPanel( #object that can contain panels, I use it to make two clean tabs for my two graphs
          #First tab VVV
          tabPanel("Total Number of Renters Per Age Group", fluid = TRUE,
                   sidebarLayout( #Side bar for input devices
                     sidebarPanel(
                       sliderTextInput( #Slider input bar for year
                         inputId = "years",
                         label = "Choose Year from 2005 to 2023 (Excluding 2020):",
                         choices = c(2005:2019,2021:2023),
                         selected = 2023)
                     ),
                     mainPanel( #Graph 1
                       plotOutput("TotalRenterAgeGroups")
                     )
                   )
          ),
          #Second Graph VVV
          tabPanel("Renters vs. Owners, per State", fluid = TRUE,
                   sidebarLayout( #Side bar for input devices
                     sidebarPanel(
                       sliderTextInput( #slider input for year
                         inputId = "years",
                         label = "Choose Year from 2005 to 2023 (Excluding 2020):",
                         choices = c(2005:2019,2021:2023),
                         selected = 2023),
                       selectInput( #selection input for state
                         inputId = "state",
                         label = "Choose State/Territory: ",
                         choices = c("Alabama","Alaska","Arizona","Arkansas","California",
                                     "Colorado","Connecticut","Delaware","District of Columbia",
                                     "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana",
                                     "Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland",
                                     "Massachusetts","Michigan","Minnesota","Mississippi",
                                     "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                                     "New Jersey","New Mexico","New York","North Carolina",
                                     "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
                                     "Puerto Rico","Rhode Island","South Carolina",
                                     "South Dakota","Tennessee","Texas","Utah","Vermont",
                                     "Virginia","Washington","West Virginia","Wisconsin",
                                     "Wyoming"),
                         selected = "Washington"
                       )
                     ),
                     mainPanel( #Graph 2
                       plotly::plotlyOutput("RenterVOwner")
                     )
                   )
          )
        )
      )
    )),
    
    #Page containing Rikus Stuff VVV
    nav_panel(title = "Education",
      
              titlePanel("State vs US Education Estimates"),
              
              # year selector slider
              fluidRow(
                column(
                  width = 6,
                  sliderInput("year_edu",
                              "Select Year:",
                              min = 2005,
                              max = 2023,
                              value = 2005,
                              step = 1,
                              sep = "")
                ),
                # state selector (Now using selectInput)
                column(
                  width = 6,
                  selectInput( # Changed from pickerInput
                    inputId = "edu_states",
                    selected = "Washington",
                    label = "Select up to 4 States:",
                    choices = NULL, # populated from server
                    multiple = TRUE # Allows server-side max=4 enforcement
                  )
                ),
                
                # education bar plot
                fluidRow(
                  column(
                    width = 12,
                    plotlyOutput("state_edu_barplot")
                  )
                )
              )
    ),
    nav_spacer(), # Don't really know what this specific line does
    
    #Navigation menu with button for clickable links
    #currently has link to our github page and original data challenge
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(tags$a("Team Github REPO", href = "https://github.com/UWB-Adv-Data-Vis-2025-Autumn/costlyliving-hoa")),
      nav_item(tags$a("Data Challenge Github REPO", href = "https://github.com/UWB-Adv-Data-Vis/CostlyLiving"))
    )
    )
    

  )



