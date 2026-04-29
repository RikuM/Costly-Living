#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
library(statebins)
library(httr)


# --- Function for all the graphs ---
function(input, output, session) {
  
  # --- Importing all the data ---
  #housing_map_data <- read.csv("C:/Users/matsu/OneDrive/Documents/homework/Fall 25/BDATA 412/costlyliving-hoa/housing_data_2005_2023.csv")
  #housing_map_data$year <- as.numeric(housing_map_data$year)
  #state_edu_income_data <- read.csv("C:/Users/matsu/OneDrive/Documents/homework/Fall 25/BDATA 412/costlyliving-hoa/education_earnings_acs1.csv")
  #country_edu_income_data <- read.csv("C:/Users/matsu/OneDrive/Documents/homework/Fall 25/BDATA 412/costlyliving-hoa/us_edu_earnings.csv")
#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
  Sys.setenv(GITHUB_PAT = "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")
  GIT_TOKEN <- Sys.getenv("GITHUB_PAT")
  housing_map_data_RAW_URL <- "https://raw.githubusercontent.com/UWB-Adv-Data-Vis-2025-Autumn/costlyliving-hoa/refs/heads/main/housing_data_2005_2023.csv?token=GHSAT0AAAAAADQNS2K4AYQ7QNPZXTZGACAK2JOIVYA"
  state_edu_income_data_RAW_URL <- "https://raw.githubusercontent.com/UWB-Adv-Data-Vis-2025-Autumn/costlyliving-hoa/refs/heads/main/education_earnings_acs1.csv?token=GHSAT0AAAAAADQK4VZOPB62VU53URUG4U5S2JM52WA"
  country_edu_income_data_RAW_URL <- "https://raw.githubusercontent.com/UWB-Adv-Data-Vis-2025-Autumn/costlyliving-hoa/refs/heads/main/us_edu_earnings.csv?token=GHSAT0AAAAAADQK4VZP3SWKOWGGAFG3B7XU2JM7VAA"
  GIT_TOKEN <- Sys.getenv("GITHUB_PAT")
  
  if (GIT_TOKEN == "") {
    stop("FATAL: GITHUB_PAT environment variable not set. Cannot access private repo.")
  }
  
  fetch_private_csv <- function(url, token) {
    
    response <- httr::GET(
      url,
      httr::add_headers(Authorization = paste("token", token))
    )
    
    if (httr::http_status(response)$category == "Success") {
      data_content <- httr::content(response, "text", encoding = "UTF-8")
      df <- readr::read_csv(data_content)
      message(paste("Successfully loaded:", basename(url)))
      return(df)
    } else {
      stop(paste("Failed to retrieve file:", basename(url),
                 "Status:", httr::status_code(response),
                 ". Check PAT and URL."))
    }
  }
  housing_map_data <- tryCatch({
    fetch_private_csv(housing_map_data_RAW_URL, GIT_TOKEN)
  }, error = function(e) {
    stop(paste("Error loading housing data:", e$message))
  })
  housing_map_data$year <- as.numeric(housing_map_data$year)
  state_edu_income_data <- tryCatch({
    fetch_private_csv(state_edu_income_data_RAW_URL, GIT_TOKEN)
  }, error = function(e) {
    stop(paste("Error loading state education data:", e$message))
  })
  country_edu_income_data <- tryCatch({
    fetch_private_csv(country_edu_income_data_RAW_URL, GIT_TOKEN)
  }, error = function(e) {
    stop(paste("Error loading country education data:", e$message))
  })
  
  print(paste("GIT_TOKEN status:", ifelse(Sys.getenv("GIT_TOKEN") != "", "PRESENT", "MISSING/EMPTY")))
  print(paste("Data Loaded: housing_map_data has", ifelse(exists("housing_map_data") && !is.null(housing_map_data), nrow(housing_map_data), "NULL"), "rows."))
  print(paste("Data Loaded: state_edu_income_data has", ifelse(exists("state_edu_income_data") && !is.null(state_edu_income_data), nrow(state_edu_income_data), "NULL"), "rows."))
  print("--- Column Names for Data Inspection ---")
  print(names(housing_map_data))
  print(names(state_edu_income_data))
  print("----------------------------------------")
  
    observe({
    print("--- state OBSERVE RUNNING ---")
    updatePickerInput(session,
                      inputId = "states",
                      choices = sort(unique(housing_map_data$NAME))
                      )
  }) 
  
    # --- map of rent to income ---
    output$rent_map <- renderPlot({
      
      # --- filter the year for slider --- 
      plot_map <- housing_map_data %>% filter(year == input$year_map)
      
      # --- map plot --- 
      ggplot(plot_map, aes(state = NAME, fill = rent_to_income_ratio)) +
        geom_statebins(border_col = "grey",
                       lbl_size = 6) +
        scale_fill_viridis_c(
          name = "Rent to Income Ratio",
          option = "magma",     
          direction = -1,
          labels = scales::number_format(accuracy = 0.01)
        ) +
        labs(
          title = paste("Rent to Income Ratio", input$year_map, " ( excluded)."),
          caption = "Source: US CENSUS"
        ) +
        theme_statebins() +
        theme(
          panel.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
          plot.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
          plot.title = element_text(color = "white", face = "bold", size = 16, hjust = 0.5),
          plot.title.position = "panel",
          legend.background = element_rect(fill = "#1e1e1e"),
          legend.text = element_text(color = "white", size = 15, face = "bold"),
          legend.title = element_text(color = "white", size = 17, face = "bold"),
          legend.position = c(0.5, 1),      # x = 0.5 (middle), y = 1 (top)
          legend.justification = c(0.5, 1),
          legend.direction = "horizontal",
          legend.title.position = "top",
          
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(0.5, "cm"),
          
        )
      
    })#end
    
    
    observe({
      # 1. POPULATE CHOICES
      print("--- state_select OBSERVE RUNNING ---")
      updatePickerInput(
        session,
        inputId = "state_select",
        choices = sort(unique(housing_map_data$NAME)),
        selected = character(0) 
      )
    })
    
    observeEvent(input$state_select, {
      print("--- state_select OBSERVEEVENT RUNNING ---")
      # Check if the user has selected more than 1 state
      if (length(input$state_select) > 1) {
        
        # If they have, immediately update the input to only keep the first state selected
        updateSelectInput(session, "state_select",
                          selected = input$state_select[1])
      }
    })
    
    #bar of top & bottom 5
    output$top_bottom_bar <- renderPlotly({
      
      req(input$year_map)
      
      plot_map_bar <- housing_map_data %>% 
                      filter(year == input$year_map)  
      
      top5 <- plot_map_bar %>% top_n(5, rent_to_income_ratio)
      bottom5 <- plot_map_bar %>% top_n(-5, rent_to_income_ratio)
      bar_data <- bind_rows(top5, bottom5) %>%
                  arrange(rent_to_income_ratio)
      
      # Add user-selected state if it exists and is not already included
      if (!is.null(input$state_select) &&
          input$state_select %in% plot_map_bar$NAME &&
          !(input$state_select %in% bar_data$NAME)) {
        
        user_state <- plot_map_bar %>% filter(NAME == input$state_select)
        bar_data <- bind_rows(bar_data, user_state) %>% arrange(rent_to_income_ratio)
      }
      
      
     
      
      p <- ggplot(bar_data, aes(x = reorder(NAME, -rent_to_income_ratio), y = rent_to_income_ratio,
                                fill = rent_to_income_ratio,
                                text = paste0("<b>State:", NAME, 
                                             "<br>Rent-to-Income Ratio:", round(rent_to_income_ratio, 3),
                                             "<br>Region:", region_name, "</b>"))) +
        geom_col() +
        scale_fill_viridis_c(option = "magma", direction = -1) +
        #scale_fill_manual(values = c("FALSE" = "grey", "TRUE" = "red")) +
        coord_flip() +
        labs(x = "State", 
             y = "Rent-to-Income Ratio",
             title = paste( input$year_map, " Top & Bottom 5 States Rent-to-Income Ratio")) +
        theme_minimal() +
        theme(
          axis.text = element_text(color = "white"),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
          plot.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
          plot.title = element_text(color = "white", face = "bold", size = 12),
          legend.background = element_rect(fill = "#1e1e1e"),
          legend.text = element_text(color = "white", size = 9, face = "bold"),
          legend.title = element_text(color = "white", size = 12, face = "bold"),
          legend.position = "none"
        )
      
      # Convert to interactive plotly
      ggplotly(p, tooltip = "text")
    }) #end
    
    #
    observe({
      shinyWidgets::updatePickerInput(
        session,
        inputId = "edu_states",
        choices = sort(unique(state_edu_income_data$NAME)),
        selected = input$edu_states
      )
    })
      
    observeEvent(input$states, {
      if (length(input$states) > 4) {
        updateSelectInput(session, "edu_states",
                          selected = input$states[1:4])
      }
    }, ignoreInit = TRUE)
    
    output$barplot <- renderPlotly({
      
      req(input$year)
      
      
      # --- Copy your original data frames (DO NOT overwrite them) ---
      df_s <- state_edu_income_data %>%
        filter(year == input$year,
               NAME %in% input$edu_states)
      
      df_u <- country_edu_income_data %>%
        filter(year == input$year) %>%
        mutate(NAME = "United States Average")
      
      # Combine
      df_combined <- bind_rows(df_s, df_u) %>%
                     filter(year != 2020)
      
      
      df_combined <- df_combined %>%
        mutate(
          is_US = ifelse(NAME == "United States Average", TRUE, FALSE)
        )
      
      
      # Order categories
      df_combined$education_level <- factor(
        df_combined$education_level,
        levels = c(
          "Less than high school",
          "High school graduate",
          "Some college or associate's",
          "Bachelor's degree",
          "Graduate or professional degree"
        )
      )
      
      df_combined$NAME <- factor(df_combined$NAME, levels = c(sort(unique(df_s$NAME)), "United States Average"))
      
      num_states <- length(input$edu_states)
      
      us_color_fixed <- c("United States Average" = "#000080")
      
      state_palette <- RColorBrewer::brewer.pal(max(3, num_states), "Set1")
      
      state_colors <- state_palette[1 : num_states]
      
      names(state_colors) <- sort(unique(df_s$NAME))
      
      all_colors <- c(state_colors, us_color_fixed)
      
      # Build plot
      bar_plot <- ggplot(df_combined, aes(
        x = education_level,
        y = estimate,
        fill = NAME,
        text = paste0(
          "<b>Region:</b> ", NAME,
          "<br><b>Education:</b> ", education_level,
          "<br><b>Estimate:</b> $", format(round(estimate, 0), big.mark = ",")
        )
      )) +
        geom_col(position = position_dodge(width = 0.9)) +
        scale_fill_manual(values = all_colors) +
        labs(
          title = paste("Education Estimates -", input$year),
          x = "Education Level",
          y = "Estimate (Dollars)"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(color = "white"),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
          plot.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
          plot.title = element_text(color = "white", face = "bold", size = 12),
          legend.background = element_rect(fill = "#1e1e1e"),
          legend.text = element_text(color = "white", size = 9, face = "bold"),
          legend.title = element_text(color = "white", size = 12, face = "bold"),
          legend.position = "none"
        )
      
      ggplotly(bar_plot, tooltip = "text")
    })
    
    
    
}


