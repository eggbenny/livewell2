# server.R
# Benedito Chou
# Dec 5 2020

# --- Server ----------------------------------------------

# --- Server
shinyServer(function(input, output, session) {
    
    # --- Filter and Control ---
  
    # Block one or the other
    observe({
      
      if (input$iv_top5 != "Select a Measure") {
        shinyjs::disable("change")
        shinyjs::enable("change_top5")
        updateSliderInput(session, "change", value = 0)
        updateSelectizeInput(session, "iv", selected = "Select a Measure")
      }
      
      if (input$iv != "Select a Measure") {
        shinyjs::disable("change_top5")
        shinyjs::enable("change")
        updateSliderInput(session, "change_top5", value = 0)
        updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
      } 

    })
  
    # Update county filter based on state
    observe({
        
      data <- filter(ana_data_1_wgeo, state == input$state)  
      county_lst <- sort(unique(data$county))
      
      updateSelectizeInput(session, "county",
        choices = county_lst
      )
        
    })
    
    # --- Data Process ---
  
    # Calculate Index
    z_geo_df <- reactive({
      # Calculate play index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      z_data_1_wgeo_long <- ana_data_1_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          focus = ifelse(state == input$state & county == input$county, 1, 0),
          value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = 100 - score,
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_1_wgeo <- z_data_1_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_percent_fair_or_poor_health) # Use fair and poor health as a proxy
      
      # Add physical_inactivity_back
      phy_inactive_wgeo <- dplyr::select(ana_data_1_wgeo, fips, state, county, population, percent_physically_inactive)
      
      z_data_1_wgeo <- left_join(z_data_1_wgeo, phy_inactive_wgeo, by = c("fips", "state", "county")) %>%
        ungroup()
      
      names(z_data_1_wgeo) <- str_replace_all(names(z_data_1_wgeo), "^value_", "")
      
      data.check <<- z_data_1_wgeo
      
      return(z_data_1_wgeo)
      
    })
  
    # Create data subset
    select_geo_df <- reactive({
        data <- filter(z_geo_df(), state == input$state, county == input$county)
        
        data.check <<- data
        return(data)
    })
    
    # Get weight for iv
    slider_data <- reactive({
        data <- filter(m_step_df, var_name == input$iv | var_name %in% c(
           "percent_fair_or_poor_health",
           "percent_adults_with_obesity",
           "percent_insufficient_sleep",
           "percent_smokers",
           "percent_excessive_drinking"
        ))
    })
    
    # Get weight for criterion
    criterion_slider_data <- reactive({
         # years_of_potential_life_lost_rate,
         # average_number_of_physically_unhealthy_days,
         # average_number_of_mentally_unhealthy_days,
         # preventable_hospitalization_rate
    })
    
    # Chart data
    plot_data <- reactive({
        data <- filter(z_geo_df(), state == input$state)
        data <- mutate(data, 
          focus = ifelse(state == input$state & county == input$county, 1, 0))
        return(data)
    })
    
    # --- InfoBox ---
    output$population <- renderInfoBox({
        
        pop <- select_geo_df() %>%
           dplyr::select(population) %>%
            unlist() %>%
            as.character()
        
        infoBox(
          "Population", pop,
          fill = TRUE,
          color = "teal"
        )
        
    })
    
    output$pop_impact <- renderInfoBox({
        
        value <-  select_geo_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        
         # slider data
        slider_data <- slider_data()
        
        slider_data.check <<- slider_data
        
        b <- filter(slider_data, var_name == input$iv) %>%
          dplyr::select(b) %>% unlist() %>% as.numeric()
        b_top5 <- filter(slider_data, var_name == input$iv_top5) %>%
          dplyr::select(b) %>% unlist() %>% as.numeric()
        b1 <- filter(slider_data, var_name == "percent_adults_with_obesity") %>%
         dplyr::select(b) %>% unlist() %>% as.numeric()
        b2 <- filter(slider_data, var_name == "percent_fair_or_poor_health") %>%
         dplyr::select(b) %>% unlist() %>% as.numeric()
        b3 <- filter(slider_data, var_name == "percent_with_access_to_exercise_oppurtunities") %>%
         dplyr::select(b) %>% unlist() %>% as.numeric()
        b4 <- filter(slider_data, var_name == "percent_excessive_drinking") %>%
          dplyr::select(b) %>% unlist() %>% as.numeric()
        b5 <- filter(slider_data, var_name == "percent_insufficient_sleep") %>%
         dplyr::select(b) %>% unlist() %>% as.numeric()
        
        
        value2_0 <- abs(round(value * ((b * input$change)/100), 0))
        value2_top5 <- abs(round(value * ((b_top5 * input$change_top5)/100), 0))
        value2_1 <- abs(round(value * ((b1 * input$change1)/100), 0))
        value2_2 <- abs(round(value * ((b2 * input$change2)/100), 0))
        value2_3 <- abs(round(value * ((b3 * input$change3)/100), 0))
        value2_4 <- abs(round(value * ((b4 * input$change4)/100), 0))
        value2_5 <- abs(round(value * ((b5 * input$change5)/100), 0))
        
        value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
    
        value2_per = round((value2 / value) * 100, 1)
        
        value3 <- paste0(value2, " (", value2_per, "%)")
        
        infoBox(
          "Est' Population Impacted", value3, 
          fill = TRUE,
          color = "teal"
        )
    })
    
    output$iv_echo <- renderInfoBox({
        
        value <-   select_geo_df() %>%
            dplyr::select(input$iv) %>%
            unlist() %>%
            as.numeric()
        
        value <- round(value, 1)
        
        infoBox(
          input$iv, value, 
          fill = TRUE,
          color = "yellow"
        )
        
    })
    
    output$iv_change <- renderInfoBox({
        
        value <-   select_geo_df() %>%
            dplyr::select(input$iv) %>%
            unlist() %>%
            as.character()
        
        infoBox(
          input$iv, value, 
          fill = TRUE,
          color = "yellow"
        )
        
    })
    
    output$iv_rank <- renderInfoBox({
        
        rank_df <- ana_data_1_wgeo %>%
            dplyr::select("fips", "state", "county", input$iv) %>%
            rename("focus_iv" = input$iv) %>%
            mutate(
             focus = ifelse(state == input$state & county == input$county, 1, 0),
             focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv),
             rank_value = rank(focus_iv),
             per_rank_value = (1 - percent_rank(focus_iv)) * 100
            ) %>%
            dplyr::filter(state == input$state, county == input$county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
        
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "teal"
        )   
        
    })
    
    output$dv_echo <- renderInfoBox({
        
        value <-   select_geo_df() %>%
            dplyr::select(percent_physically_inactive) %>%
            unlist() %>%
            as.numeric()
        
        value <- round(value, 1)
        
        infoBox(
          "percent_physically_inactive", value, 
          fill = TRUE,
          color = "navy"
        )
        
    })
    
    output$dv_rank <- renderInfoBox({
      
        # Get weight aka slider data
        slider_data <- slider_data()
        
        rank_df <-  ana_data_1_wgeo %>%
            dplyr::select(fips, state, county, percent_physically_inactive) %>%
            mutate(
             focus = ifelse(state == input$state & county == input$county, 1, 0),
             percent_physically_inactive = ifelse(focus == 1, (percent_physically_inactive + (slider_data$b * input$change)), percent_physically_inactive),
              rank_value = rank(percent_physically_inactive),
              per_rank_value = (1 - percent_rank(percent_physically_inactive)) * 100
            ) %>%
            dplyr::filter(state == input$state, county == input$county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
    
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "navy"
        )   
        
    })
        
     output$index_echo <- renderInfoBox({
        
        value <- select_geo_df() %>%
            dplyr::select(score) %>%
            unlist() %>%
            as.numeric()
        
        value <- round(value, 1)
        
        infoBox(
          "Play Index", value, 
          fill = TRUE,
          color = "blue"
        )
        
     })
        
      output$index_rank <- renderInfoBox({
        
        rank_df <- z_geo_df() %>%
            dplyr::select(fips, state, county, score) %>%
            mutate(
              rank_value = rank(-score),
              per_rank_value = percent_rank(score) * 100
            ) %>%
            dplyr::filter(state == input$state, county == input$county)
        
        rank_value <- rank_df %>%
            dplyr::select(rank_value) %>%
            unlist() %>%
            as.numeric()
    
        per_rank_value <- rank_df %>%
            dplyr::select(per_rank_value) %>%
            unlist() %>%
            as.numeric() %>%
            round(1)
        
        rank_value_2 <- paste(rank_value, " (", per_rank_value, "th)")
            
        infoBox(
          "National Rank (Percentile)", rank_value_2, 
          fill = TRUE,
          color = "blue"
        )   
      })
      
      
    # Card that show correlation between Play Index and the 4 Criterion
      
     output$criterion1_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$years_of_potential_life_lost_rate, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Year of Potential",br(), "Life Lost")),
          value, 
          fill = TRUE,
          color = "olive"
        )
        
     })
     
     output$criterion2_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$average_number_of_physically_unhealthy_days, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Avg # of",br(), "Physically Unhealthy Days")),
          value, 
          fill = TRUE,
          color = "olive"
        )
        
     })
     
     output$criterion3_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$average_number_of_mentally_unhealthy_days, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Avg # of",br(), "Mentally Unhealthy Days")),
          value,
          fill = TRUE,
          color = "olive"
        )
        
     })
     
     output$criterion4_card <- renderInfoBox({
        
        data <- z_geo_w_criterion_df()
        
        value <- cor(data$score, data$preventable_hospitalization_rate, 
            method = "pearson", use = "complete.obs")
        
        value <- round(value, 2)
        
        infoBox(
          HTML(paste("Preventable",br(), "Hospitalization Rate")),
          value, 
          fill = TRUE,
          color = "olive"
        )
        
     })

        
    
    # --- Chart and Plot ----
     
     z_geo_w_criterion_df <- reactive({
      
       data <- z_geo_df()
       
       data.1 <<- data
       
       # Join with criterion variable
       data <- left_join(data, ana_data_1_criterion, by = c("fips", "state", "county"))
       
       return(data)
       
     })
      
      
    # Simple between the Play index and the four criterion
      
    output$test_criterion_plot_1 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$years_of_potential_life_lost_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, years_of_potential_life_lost_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
      
    output$test_criterion_plot_2 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$average_number_of_physically_unhealthy_days, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, average_number_of_physically_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
      labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_3 <- renderPlot({
      
     data <- z_geo_w_criterion_df()
    
     value <- cor(data$score, data$average_number_of_mentally_unhealthy_days, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, average_number_of_mentally_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
      labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_4 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$preventable_hospitalization_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, preventable_hospitalization_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    # test scatter grid plot
    output$test_grid_plot <- renderPlot({
      
        # Get data
        data <- plot_data()
        
        # Get weight aka slider data
        slider_data <- slider_data()
        
        # Make focus generic
        if (input$iv_top5 != "Select a Measure") {
        data <- data %>%
            rename(
              "top5_iv" = input$iv_top5)  %>%
          mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
        }
        
        if (input$iv != "Select a Measure") {
          
        data <- data %>%
            rename(
              "focus_iv" = input$iv) %>%
          mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
        }
        
        # Modify the data
        data <- mutate(data,
          label = paste0(county, "\n", state),
          label = ifelse(focus == 1, label, NA),
          percent_physically_inactive = ifelse(focus == 1, (percent_physically_inactive + (slider_data$b * input$change)), percent_physically_inactive))
        
        data.check <<- data
        
        # Make plot
        ggplot(data, 
            aes(percent_physically_inactive, score, color = focus)) +
            geom_point(size = 5) +
            geom_label_repel(aes(label = label),
                       color = "black") +
            labs(y = "Play Index (0 to 100)", x = input$iv) +
            theme_minimal() +
            theme(legend.position = "none")
        
    })
    
    # --- Test Table ---
    
    output$test_table <- DT::renderDataTable({
       datatable(m_step_df,
         options = list(
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df)[-1], digits = 2)
    })
    
    # --- IV Contribution ---
    output$iv_contr_stack_plot <- renderPlot({
      
 data <- m_step_df %>% arrange(pratt) %>% 
        mutate(
          rank = rank(-pratt),
          order_var_name = ifelse(rank <= 3, var_name, "other")
        ) %>%
      filter(rank <= 3)
      
      # Create fake other
      data_other <- tibble(
        var_name = "Other",
        pratt = 1 - sum(data$pratt))
      
      data <- bind_rows(data, data_other)
      
      ggplot(data, aes(1, pratt, fill = reorder(var_name, pratt))) + 
              geom_bar(position = "fill", stat = "identity") +
              geom_text(aes(label = 
              str_replace(paste(var_name, "\n",            
                round(pratt * 100), 1), "percent_", "% ")), position = "fill", hjust = 0.5, angle = 90, vjust = 0) +
              coord_flip() + 
              theme_minimal() +
                theme(
                legend.position = "none",          
                axis.title = element_blank(),
                axis.text = element_blank()) +
      scale_fill_brewer(palette = "YlGnBu")
      
    })
    
    output$iv_contr_plot <- renderPlot({
      
      data <- m_step_df %>% arrange(pratt) %>% 
        mutate(
          rank = rank(-pratt),
          order_var_name = ifelse(rank <= 3, var_name, "other")
        ) %>%
      filter(rank <= 3)
      
      # Create fake other
      data_other <- tibble(
        var_name = "Other",
        pratt = 1 - sum(data$pratt))
      
      data <- bind_rows(data, data_other)
      
      ggplot(data, aes(reorder(var_name, -pratt), 1, fill = pratt)) + 
        geom_tile() + 
        geom_text(aes(label = 
            str_replace(paste(var_name, "\n",            
            round(pratt * 100), 1), "percent_", "% "))) +
        theme_minimal() +
        theme(
          legend.position = "none",          
          axis.title = element_blank(),
          axis.text = element_blank()) +
        scale_fill_gradient(low = "grey", high = "yellow")
      
    })
    
    # --- Test Play Index ---
    output$play_index_hist <- renderPlot({

      data <- z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_histogram() +
        theme_minimal()
      
    })
    
    output$play_index_boxplot <- renderPlot({

      data <- z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_boxplot() +
        theme_minimal()
      
    })
    
    output$play_table <- DT::renderDataTable({
      
       datatable(z_geo_df,
         options = list(
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df)[-1], digits = 2)
    })


})
