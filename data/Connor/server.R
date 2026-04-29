library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

server <- function(input, output, session) {
  
  loaded <- readRDS("Connor/final_data.rds")
  acs_df <- loaded$data
  top50_names <- loaded$top50_names
  
  observe({
    updateSelectInput(
      session,
      "region",
      choices = c("National", top50_names),
      selected = "National"
    )
  })
  
  filtered_data <- reactive({
    df <- acs_df %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
    
    if (input$region == "National") {
      # Return only the precomputed national rows
      df <- df %>% filter(region_type == "National")
    } else {
      # City selected: match the cleaned name
      df <- df %>% filter(NAME_clean == input$region)
    }
    
    df <- df %>% mutate(year = as.numeric(year))
    
    df
  })
  
  output$rent_owner_plot <- renderPlot({
    df <- filtered_data()
    
    if (!"rent" %in% names(df)) df <- df %>% rename(rent = median_rent)
    if (!"owner_cost" %in% names(df)) df <- df %>% rename(owner_cost = median_owner_cost)
    
    if (input$adjust_inflation) {
      ggplot(df, aes(x = year)) +
        geom_smooth(aes(y = rent_real, color = "Rent (real)"), size = 1) +
        geom_smooth(aes(y = owner_cost_real, color = "Owner Cost (real)"), size = 1) +
        labs(y = 'Monthly Cost (inflation-adjusted)', color = '') +
        theme_minimal()
    } else {
      ggplot(df, aes(x = year)) +
        geom_smooth(aes(y = rent, color = 'Rent'), size = 1) +
        geom_smooth(aes(y = owner_cost, color = "Owner Cost"), size = 1) +
        labs(y = 'Monthly Cost (nominal)', color = '') +
        theme_minimal()
    }
  })
  
  # Stacked bar: Owner vs Renter share
  output$homeownership_plot <- renderPlot({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      plot.new()
      title("No data available for the selected region / years")
      return()
    }
    
    df_long <- df %>%
      select(year, homeownership_rate, renter_share) %>%
      pivot_longer(cols = c(homeownership_rate, renter_share),
                   names_to = "type",
                   values_to = "share") %>%
      mutate(type = recode(type,
                           homeownership_rate = "Owner",
                           renter_share = "Renter"))
    
    ggplot(df_long, aes(x = factor(year), y = share, fill = type)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      scale_fill_manual(values = c("Owner" = "steelblue", "Renter" = "orange")) +
      labs(y = "Share of Occupied Housing Units", x = "Year", fill = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Inflation-adjusted rent trend
  output$rent_inflation_plot <- renderPlot({
    df <- filtered_data()
    
    if (input$region == "National") {
      plot_df <- df %>% filter(region_type == "National")
      label_col <- "NAME"  # national NAME is just "United States"
    } else {
      plot_df <- df %>% filter(NAME_clean == input$region)
      label_col <- "NAME_clean"
    }
    
    yvar <- if (input$adjust_inflation && "rent_real" %in% names(plot_df)) "rent_real" else "median_rent"
    
    ggplot(plot_df, aes_string(x = "year", y = yvar)) +
      geom_line(size = 1) +
      labs(y = "Median Rent (adjusted if selected)", x = "Year") +
      theme_minimal()
  })
  
}
