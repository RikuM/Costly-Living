
#Loads libraries, if you need to add a library, then add it in here
pacman::p_load(shiny, tidyverse, tidycensus, purrr, readxl, janitor, knitr, ggplot2,reshape2,dplyr, viridis, scales, statebins, plotly, readr, curl, httr, shinyWidgets)
library(shinyWidgets)
#Server Logic

#New method of accessing data
#IMPORTANT: LOAD ALL THE CHUNKS IN THE 'DataLoad.Rmd' FILE BEFORE TRYING TO RUN THE APP
load('.RData')
renter_occupied_age <- renter_occupied_new 
owner_occupied_age <- owner_occupied_new

function(input, output, session) {
  
  #housing_map_data <- read.csv("C:/Users/matsu/OneDrive/Documents/homework/Fall 25/BDATA 412/costlyliving-hoa/housing_data_2005_2023.csv")
  #housing_map_data$year <- as.numeric(housing_map_data$year)
  #state_edu_income_data <- read.csv("C:/Users/matsu/OneDrive/Documents/homework/Fall 25/BDATA 412/costlyliving-hoa/education_earnings_acs1.csv")
  #country_edu_income_data <- read.csv("C:/Users/matsu/OneDrive/Documents/homework/Fall 25/BDATA 412/costlyliving-hoa/us_edu_earnings.csv")
  
  output$text <- renderUI({
    HTML("The objective of our dashboard is to visually demonstrate how the housing industry challenges renters, showing the percentage of rent burdened people, cost of renting vs. owning, and rent to income ratios.
          <br><br>Our group pulled US Census data to help calculate rent burden, looking at whether they are spending more than 30% or 50% of their income, as well as calculating rent to income ratios, the comparison of home owners vs renters, different age ranges, and different amounts of education.
          <br><br>The dashboard is based upon the Autumn 2025 BIS 412 Advanced Data Visualization Data Challenge, titled “CostlyLiving”.
         <br><br>The goal of the challenge is to design interactive visualizations using data from the American Community Survey (ACS) to “illuminate patterns in the rising cost of living”.
         <br><br>The github REPO for the challenge, as well as our teams github REPO, can be accessed through the links below:"
          )
  })
  
  output$text_2 <- renderUI({
    HTML("Or in the links menu (located in the top right corner or through the tabs, depending on screen size):
         <br><br>The main goal of our dashboard is to highlight how factors like, rent burden, age, mortgage, and education play a part in the struggle renters face due to the rising cost of living. As well as help users understand where renters are most financially strained and what social factors may be associated with higher levels of rent burden.
         <br><br>Below is one of the visualizations we made, looking at the Rent to Income ratio, sorted by state."
         )
  })
  
  
  #Output for my (Sebastian's) first graph  
  output$TotalRenterAgeGroups <- renderPlot({
    renter_occupied_age %>% #Take dataset
      select(-NAME) %>% #Remove NAME Column since it isn't necessary
      group_by(Age_Range)  %>% #Group by the age ranges
      filter(Age_Range != "total") %>% #Remove rows that have an age range labeled "total"
      filter(year == input$years) %>% #Filter the years in the dataset to the users inputted year
      summarise(Total = sum(Total)) %>% #sums up the data to make it easy to visual
      ggplot(aes(Age_Range,Total)) + #The plot
      geom_col(fill = "#EA742D", color = "black") +
      ggtitle(
        "Total Number of Renters Per Age Group", 
        subtitle = paste0("From the ", input$years, " 1-year American Community Survey.")) +
      labs(
        x = "Age Range",
        y = "# of People"
      ) +
      scale_x_discrete(labels = c("15-24","25-34","35-44","45-54","55-59","60-64","65-74","74-84","85+")) +
      scale_y_continuous(labels = scales::label_number()) +
      geom_label(aes(label = Total, vjust = 2))
    
  })
  
  #Output for my (Sebastian's) second graph
  output$RenterVOwner <- renderPlotly({
    #two temp datasets to add indicators of homeowner type
    rent_temp <- renter_occupied_age %>%
      mutate(NAME = paste(NAME, "Renters"))
    
    owner_temp <- owner_occupied_age  %>%
      mutate(NAME = paste(NAME, "Owners"))
    
    #temp dataset used in the graph that combines the two above datasets
    temp <- merge(rent_temp,owner_temp, all = TRUE) %>%
      filter(Age_Range != "total") %>% #filters out totals
      filter(year == input$years) #filters to users specified year
    
    #first element of ggplot filters data to be the users inputted state
    p <- ggplot(temp[temp$NAME %in% c(paste0(input$state, " Renters") ,paste0(input$state, " Owners")),] ,aes(x=Age_Range, y= Total, fill= NAME, colour = NAME)) + 
      geom_bar(position="dodge", stat="identity", color = "black") +
      scale_x_discrete(labels = c("15-24","25-34","35-44","45-54","55-59","60-64","65-74","74-84","85+")) +
      scale_y_continuous(labels = scales::label_number()) +
      ggtitle(
        paste0("Total Renters V. Owners, per age group, in ", input$state), 
        subtitle = paste0("From the ", input$years, " 1-year American Community Survey.")) +
      labs(
        x = "Age Range",
        y = "# of People"
      ) +
      scale_fill_manual(values = c("#11FA63","#7F55F0"), labels = c("Owners","Renters")) +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom"
      )
    
    plotly::ggplotly(p, tooltip = "Total")
    
  })
  
  
  #--- Riku Code Start --- 
  
  # --- state picker --- 
  #observe({
  #  print("--- state_riku OBSERVE RUNNING ---")
  #  updatePickerInput(session,
  #                    inputId = "states_riku",
  #                    choices = sort(unique(housing_map_data$NAME))
  #                    )
  #}) 
  
  # --- top and bottom 5 bar upload ---
  observe({
    req(housing_map_data)
    req(state_edu_income_data)
    print("--- state_select OBSERVE RUNNING ---")
    # 1. POPULATE CHOICES
    updateSelectInput(session,
                      inputId = "state_select",
                      choices = sort(unique(housing_map_data$NAME)),
                      selected = character(0)
    )
  })
  
  full_state_choices <- sort(unique(housing_map_data$NAME))
  # --- 1 selection picker ---
  observeEvent(input$states_select, {
    req(housing_map_data)
    req(state_edu_income_data)
    print("--- state_select OBSERVEEVENT RUNNING ---")
    selected_states <- input$state_select
    num_selected <- length(selected_states)
    
    # CASE 1: The user somehow selected 3+ states before the server could respond.
    if (num_selected > 2) {
      
      # 1. Preserve the first two selections.
      selected_states <- selected_states[1:2] 
      
      # 2. Reset the input using the full list, but only the first two are 'selected'.
      updateSelectInput(session, 
                        "state_select",
                        choices = full_state_choices,
                        selected = selected_states)
      
      # After this, the code immediately falls through to the next check.
    } 
    
    # CASE 2: Exactly 2 states are selected. This is the intended state preservation step.
    else if (num_selected == 2) {
      
      # **This is how the previous input is kept:**
      # We rewrite the 'choices' list to contain only the two selected states.
      # This makes all other states visually disappear or become unselectable
      # in the dropdown menu until the user deselects one of the current two.
      updateSelectInput(session, 
                        "state_select",
                        choices = selected_states, # Only show the two selected states
                        selected = selected_states)
      
    }
    
    # CASE 3: 0 or 1 state is selected. Allow the user to pick more.
    else {
      
      # Restore the full list of choices so the user can select their first or second state.
      updateSelectInput(session, 
                        "state_select",
                        choices = full_state_choices,
                        selected = selected_states)
    }
  }, ignoreInit = TRUE)
  
  # --- education state choice --- 
  observe({
    req(housing_map_data)
    req(state_edu_income_data)
    print("--- edu_state OBSERVE RUNNING ---")
    
    current_selection <- input$edu_states
    
    initial_selection <- if (is.null(current_selection)) {
      "Washington" 
    } else {
      current_selection
    }
    
    updateSelectInput(
      session,
      inputId = "edu_states",
      choices = sort(unique(state_edu_income_data$NAME)),
      selected = initial_selection
    )
  })
  
  # --- pick from 4 states ---
  observeEvent(input$edu_states, {
    req(housing_map_data)
    req(state_edu_income_data)
    if (length(input$edu_states) > 4) {
      # Use base Shiny updateSelectInput
      updateSelectInput(session, "edu_states",
                        selected = input$edu_states[1:4])
    }
  }, ignoreInit = TRUE)
  
  # --- map of rent to income ---
  output$rent_map <- renderPlot({
    req(housing_map_data, state_edu_income_data, input$year_map)
    
    plot_map <- housing_map_data %>% filter(year == input$year_map)
    
    ggplot(plot_map, aes(state = NAME, fill = rent_to_income_ratio)) +
      geom_statebins(border_col = "grey", lbl_size = 6) +
      scale_fill_viridis_c(
        name = "Rent to Income Ratio",
        option = "magma",
        direction = -1,
        labels = scales::number_format(accuracy = 0.01)
      ) +
      labs(
        title = paste("Rent to Income Ratio", input$year_map, " (2020 excluded)."),
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
        legend.position = c(0.5, 1),
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal",
        legend.title.position = "top",
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.5, "cm"),
      )
  }) #end
  
  
  
  #bar of top & bottom 5
  output$top_bottom_bar <- renderPlotly({
    req(housing_map_data, state_edu_income_data, input$year_map)
    
    plot_map_bar <- housing_map_data %>%
      filter(year == input$year_map)
    
    top5 <- plot_map_bar %>% top_n(5, rent_to_income_ratio)
    bottom5 <- plot_map_bar %>% top_n(-5, rent_to_income_ratio)
    bar_data <- bind_rows(top5, bottom5) %>%
      arrange(rent_to_income_ratio)
    
    # Get the selected states (can be 0, 1, or 2)
    selected_states <- input$state_select
    
    # Add user-selected state if it exists and is not already included
    if (length(selected_states) > 0) {
      
      # Filter for selected states that are NOT already in bar_data
      states_to_add <- selected_states[!(selected_states %in% bar_data$NAME)]
      
      if (length(states_to_add) > 0) {
        user_states <- plot_map_bar %>% filter(NAME %in% states_to_add)
        
        # Add the states and re-arrange
        bar_data <- bind_rows(bar_data, user_states) %>% arrange(rent_to_income_ratio)
      }
    }
    
    p <- ggplot(bar_data, aes(x = reorder(NAME, -rent_to_income_ratio), y = rent_to_income_ratio,
                              fill = rent_to_income_ratio,
                              text = paste0("<b>State:", NAME,
                                            "<br>Rent-to-Income Ratio:", round(rent_to_income_ratio, 3),
                                            "<br>Region:", region_name, "</b>"))) +
      geom_col() +
      scale_fill_viridis_c(option = "magma", direction = -1) +
      coord_flip() +
      labs(x = "State",
           y = "Rent-to-Income Ratio",
           title = paste( input$year_map, " Top & Bottom 5 States Rent-to-Income Ratio")) +
      theme_minimal() +
      theme(
        axis.text = element_text(color = "white"),
        axis.title = element_blank(),
        axis.line = element_line(color = "#444444"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
        plot.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
        plot.title = element_text(color = "white", face = "bold", size = 12),
        #legend.background = element_rect(fill = "#1e1e1e"),
        #legend.text = element_text(color = "white", size = 9, face = "bold"),
        #legend.title = element_text(color = "white", size = 12, face = "bold"),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text")
  }) #end
  
  
  
  
 
  
  
  # --- bar plot of the US education vs State education ---
  output$state_edu_barplot <- renderPlotly({
    req(housing_map_data, state_edu_income_data, input$year_edu)
    
    df_s <- state_edu_income_data %>%
      filter(year == input$year_edu,
             NAME %in% input$edu_states)
    
    df_u <- country_edu_income_data %>%
      filter(year == input$year_edu) %>%
      mutate(NAME = "United States Average")
    
    df_combined <- bind_rows(df_s, df_u) %>%
      filter(year != 2020)
    
    df_combined <- df_combined %>%
      mutate(
        is_US = ifelse(NAME == "United States Average", TRUE, FALSE)
      )
    
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
        # Use the correct input ID for the title year
        title = paste("Education Estimates -", input$year_edu), 
        x = "Education Level",
        y = "Estimate (Dollars)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text = element_text(color = "white", face = "bold"),
        axis.title = element_blank(),
        axis.line = element_line(color = "#444444"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
        plot.background = element_rect(color = "#1e1e1e", fill = "#1e1e1e"),
        plot.title = element_text(color = "white", face = "bold", size = 12),
        legend.background = element_rect(fill = "#1e1e1e"),
        legend.text = element_text(color = "white", size = 9, face = "bold"),
        legend.title = element_text(color = "white", size = 12, face = "bold"),
        legend.position = "none"
      ) + 
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(bar_plot, tooltip = "text")
  }) #end
  #Riku end
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #----------------------------Connor start -------------------------------
  #------------------------------------------------------------------------
  
  acs_df <- loaded$data %>% mutate(NAME_clean = as.character(NAME))
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
        scale_y_continuous(labels = scales::dollar) +
        scale_x_continuous(breaks = df$year, labels  = df$year) +
        labs(y = 'Monthly Cost (inflation-adjusted)', color = '') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(df, aes(x = year)) +
        geom_smooth(aes(y = rent, color = 'Rent'), size = 1) +
        geom_smooth(aes(y = owner_cost, color = "Owner Cost"), size = 1) +
        scale_y_continuous(labels = scales::dollar) +
        scale_x_continuous(breaks = df$year, labels = df$year) +
        labs(y = 'Monthly Cost (nominal)', color = '') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  #=========================================================================
  #---------------------------Connor end------------------------------------
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  #Oscar code start 
  
  
  
  # --- populate city choices dynamically ---
  observe({
    updateSelectInput(session, "city", choices = sort(unique(rent_big_cities$NAME)))
  })
  
  # --- line plot ---
  output$rentPlot <- renderPlotly({
    req(input$city)
    
    df <- rent_big_cities %>% filter(NAME == input$city)
    
    p <- ggplot(df, aes(year, pct_burdened)) +
      geom_line(size = 1.2, color = "#2D89C8") +
      labs(
        title = paste("Rent Burden Over Time:", input$city),
        x = "Year",
        y = "% Cost-Burdened (≥ 30%)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # --- heatmap ---
  output$rentHeatmap <- renderPlot({
    ggplot(rent_big_cities, aes(x = year, y = NAME, fill = pct_burdened)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "magma") +
      labs(
        title = "Heatmap of Rent Burden Over Time",
        x = "Year",
        y = "City",
        fill = "% Burdened"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  #Oscar code end 
}
