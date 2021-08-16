# server.R
# Benedito Chou
# Aug 15 2021

# --- Server ----------------------------------------------

# --- Server
shinyServer(function(input, output, session) {
  
    # --- reset measure ---
  
    observeEvent(input$reset_play, {
      updateSelectizeInput(session, "iv", selected = "Select a Measure")
      updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
      updateSelectizeInput(session, "iv_domain", selected = "Key Impact")
      updateSliderInput(session, "change", value = 0)
      updateSliderInput(session, "change_top5", value = 0)
      shinyjs::disable("change")
      shinyjs::enable("change_top5")
      
    })
  
    observeEvent(input$reset_rest, {
      updateSelectizeInput(session, "rest_iv", selected = "Select a Measure")
      updateSelectizeInput(session, "rest_iv_top5", selected = "Select a Measure")
      updateSelectizeInput(session, "rest_iv_domain", selected = "Key Impact")
      updateSliderInput(session, "rest_change", value = 0)
      updateSliderInput(session, "rest_change_top5", value = 0)
      shinyjs::disable("rest_change")
      shinyjs::enable("rest_change_top5")
      
    })
    
    observeEvent(input$reset_work, {
      updateSelectizeInput(session, "work_iv", selected = "Select a Measure")
      updateSelectizeInput(session, "work_iv_top5", selected = "Select a Measure")
      updateSelectizeInput(session, "work_iv_domain", selected = "Key Impact")
      updateSliderInput(session, "work_change", value = 0)
      updateSliderInput(session, "work_change_top5", value = 0)
      shinyjs::disable("work_change")
      shinyjs::enable("work_change_top5")
      
    })
  
    # --- measure_lst Switch ---
  
   observe({
     
     # if (input$iv_top5 == "percent_fair_or_poor_health") {
     #   updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_rest))
     # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
     #   updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_lst_play))
     # }
     
   })
    
    # --- Filter and Control ---
   
   # Hide Matrix
   observeEvent(input$hide_play_matrix, {
     shinyjs::toggle(id = "play_matrix_box")
   })
   
   observeEvent(input$hide_rest_matrix, {
     shinyjs::toggle(id = "rest_matrix_box")
   })
   
   observeEvent(input$hide_work_matrix, {
     shinyjs::toggle(id = "work_matrix_box")
   })
   
   # Hide Sankey
   observeEvent(input$hide_play_sankey, {
     shinyjs::toggle(id = "play_sankey_box")
   })
   
   observeEvent(input$hide_rest_sankey, {
     shinyjs::toggle(id = "rest_sankey_box")
   })
   
   observeEvent(input$hide_work_sankey, {
     shinyjs::toggle(id = "work_sankey_box")
   })
   
   # Hide Scatterplot
   observeEvent(input$hide_play_scatter, {
     shinyjs::toggle(id = "play_scatter_box")
   })
   
   observeEvent(input$hide_rest_scatter, {
     shinyjs::toggle(id = "rest_scatter_box")
   })
   
   observeEvent(input$hide_work_scatter, {
     shinyjs::toggle(id = "work_scatter_box")
   })
  
    # Block one or the other
   # Play Index
    observe({
      
      if (input$iv_top5 == "per_fair_or_poor_health") {
       
       if (input$iv_domain != "Key Impact") {
         choice_lst <- filter(domain_map, Domain == input$iv_domain, var_name %in% measure_lst_fp_health) %>%
           dplyr::select(var_name) %>% unlist() %>% as.character()
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", choice_lst))
        } else {
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_fp_health))
        }
        
        
      } else if (input$iv_top5 == "per_w_grad_or_prof_degree") {
       
       if (input$iv_domain != "Key Impact") {
         choice_lst <- filter(domain_map, Domain == input$iv_domain, var_name %in% measure_lst_grad) %>%
           dplyr::select(var_name) %>% unlist() %>% as.character()
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", choice_lst))
      } else {
       updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_grad))
        
      }
        
      } else if (input$iv_top5 == "per_adults_with_diabetes") {
       
       if (input$iv_domain != "Key Impact") {
         choice_lst <- filter(domain_map, Domain == input$iv_domain, var_name %in% measure_lst_diabetes) %>%
           dplyr::select(var_name) %>% unlist() %>% as.character()
         updateSelectizeInput(session, "iv", choices = c("Select a Measure", choice_lst))
      } else {
       updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_diabetes))
      }
        
      } else if (input$iv_top5 != "Select a Measure") {
        shinyjs::disable("change")
        shinyjs::enable("change_top5")
        updateSliderInput(session, "change", value = 0)
        updateSelectizeInput(session, "iv", selected = "Select a Measure")
        
       # if (input$iv_top5 == "percent_fair_or_poor_health") {
       # updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_fp_health))
       # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
       # # updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_lst_play))
       # }
        
      }
    })
    
    observe({
      
       if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        shinyjs::disable("change_top5")
        shinyjs::enable("change")
        updateSliderInput(session, "change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")

       # if (input$iv_top5 == "percent_fair_or_poor_health") {
       # # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_rest))
       # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
       # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_play))
       # }

      } else if (!input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        shinyjs::disable("change_top5")
        shinyjs::enable("change")
        updateSliderInput(session, "change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
      }
    })
    
    
    # Rest Index
    observe({
      
      if (input$rest_iv_top5 == "avg_no_of_mentally_unhealthy_days") {
        
        if (input$rest_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$rest_iv_domain, var_name %in% measure_lst_avg_m_days) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", measure_top3_lst_avg_m_days))
        }
        
        
      } else if (input$rest_iv_top5 == "per_physically_inactive") {
        
        if (input$rest_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$rest_iv_domain, var_name %in% measure_lst_phy_inactive) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", measure_top3_lst_phy_inactive))
          
        }
        
      } else if (input$rest_iv_top5 == "per_adults_with_obesity") {
        
        if (input$rest_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$rest_iv_domain, var_name %in% measure_lst_obesity) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "rest_iv", choices = c("Select a Measure", measure_top3_lst_obesity))
        }
        
      } else if (input$rest_iv_top5 != "Select a Measure") {
        shinyjs::disable("rest_change")
        shinyjs::enable("rest_change_top5")
        updateSliderInput(session, "rest_change", value = 0)
        updateSelectizeInput(session, "rest_iv", selected = "Select a Measure")
        
        # if (input$iv_top5 == "percent_fair_or_poor_health") {
        # updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_fp_health))
        # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
        # # updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_lst_play))
        # }
      
       }
      
    })
  
    observe({
      
      if (input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        shinyjs::disable("rest_change_top5")
        shinyjs::enable("rest_change")
        updateSliderInput(session, "rest_change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
        
        # if (input$iv_top5 == "percent_fair_or_poor_health") {
        # # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_rest))
        # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_play))
        # }
        
      } else if (!input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        shinyjs::disable("rest_change_top5")
        shinyjs::enable("rest_change")
        updateSliderInput(session, "rest_change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
      }
    })
    
    
    # Work Index
    observe({
      
      if (input$work_iv_top5 == "teen_birth_rate") {
        
        if (input$work_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$work_iv_domain, var_name %in% measure_lst_teen_brate) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "work_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "work_iv", choices = c("Select a Measure", measure_top3_lst_teen_brate))
        }
        
        
      } else if (input$work_iv_top5 == "per_w_grad_or_prof_degree") {
        
        if (input$work_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$work_iv_domain, var_name %in% measure_lst_grad) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "work_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "work_iv", choices = c("Select a Measure", measure_top3_lst_grad))
          
        }
        
      } else if (input$work_iv_top5 == "avg_no_of_mentally_unhealthy_days") {
        
        if (input$work_iv_domain != "Key Impact") {
          choice_lst <- filter(domain_map, Domain == input$work_iv_domain, var_name %in% measure_lst_avg_m_days) %>%
            dplyr::select(var_name) %>% unlist() %>% as.character()
          updateSelectizeInput(session, "work_iv", choices = c("Select a Measure", choice_lst))
        } else {
          updateSelectizeInput(session, "work_iv", choices = c("Select a Measure", measure_top3_lst_avg_m_days))
        }
        
      } else if (input$work_iv_top5 != "Select a Measure") {
        shinyjs::disable("work_change")
        shinyjs::enable("work_change_top5")
        updateSliderInput(session, "work_change", value = 0)
        updateSelectizeInput(session, "work_iv", selected = "Select a Measure")
        
        # if (input$iv_top5 == "percent_fair_or_poor_health") {
        # updateSelectizeInput(session, "iv", choices = c("Select a Measure", measure_top3_lst_fp_health))
        # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
        # # updateSelectizeInput(session, "iv", selected = "Select a Measure", choices = c("Select a Measure", measure_lst_play))
        # }
        
      }
      
    })
    
    observe({
      
      if (input$work_iv_top5 %in% measure_top3_lst_work & input$work_iv != "Select a Measure") {
        shinyjs::disable("work_change_top5")
        shinyjs::enable("work_change")
        updateSliderInput(session, "work_change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
        
        # if (input$iv_top5 == "percent_fair_or_poor_health") {
        # # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_work))
        # } else if (input$iv_top5 != "percent_fair_or_poor_health") {
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure", choices = c("Select a Measure", measure_top3_lst_play))
        # }
        
      } else if (!input$work_iv_top5 %in% measure_top3_lst_work & input$work_iv != "Select a Measure") {
        shinyjs::disable("work_change_top5")
        shinyjs::enable("work_change")
        updateSliderInput(session, "work_change_top5", value = 0)
        # updateSelectizeInput(session, "iv_top5", selected = "Select a Measure")
      }
    })
  
    # Update county and region filter based on state
    observe({
        
      data <- filter(ana_data_wgeo, state == input$state)  
      county_lst <- sort(unique(data$county))
      
      updateSelectizeInput(session, "county",
        choices = county_lst
      )
      
      Rdata <- filter(ana_data_wgeo, state == input$state)  
      region_lst <- sort(unique(Rdata$RegionOrg))
      
      updateSelectInput(session, "region", 
        choices = c("--", region_lst))
        
    })
    
    observe({
        
      data <- filter(ana_data_full_wgeo, state == input$rest_state)  
      county_lst <- sort(unique(data$county))
      
      updateSelectizeInput(session, "rest_county",
        choices = county_lst
      )
      
      Rdata <- filter(ana_data_wgeo, state == input$rest_state)  
      region_lst <- sort(unique(Rdata$RegionOrg))
      
      updateSelectInput(session, "rest_region", 
                        choices = c("--", region_lst))
        
    })
    
    observe({
      
      data <- filter(ana_data_full_wgeo, state == input$work_state)  
      county_lst <- sort(unique(data$county))
      
      updateSelectizeInput(session, "work_county",
                           choices = county_lst
      )
      
      Rdata <- filter(ana_data_wgeo, state == input$work_state)  
      region_lst <- sort(unique(Rdata$RegionOrg))
      
      updateSelectInput(session, "work_region", 
                        choices = c("--", region_lst))
      
    })
    
    # --- Data Process ---
    
    # Play Index b change
    # Calculate the b to use for change from slider
    play_slider_change_b <- reactive({
      
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
          
          b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          # Get sign so it doesn't mess up the multiplier
          sign <- sign(bm)
          real_b <- b1 * bm * sign
          
          print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
          
        } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
          
          b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          # Get sign so it doesn't mess up the multiplier
          sign <- sign(bm)
          real_b <- b2 * bm * sign
          
          print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
          
        } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
          
          b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          # Get sign so it doesn't mess up the multiplier
          sign <- sign(bm)
          real_b <- b3 * bm * sign
          print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b3, 3)))
        
        } else {
          
          real_b <- slider_data$b
          print(paste0("Normal Route: ", input$iv_top5, "):", round(real_b, 3)))
          
        }
        
        print(paste0("slider change b: ", round(real_b), 3))
        
        return(real_b)
      
    })
    
    # Rest Index b change
    # Calculate the b to use for change from slider
    rest_slider_change_b <- reactive({
      
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        # if (input$rest_iv_top5 == "routine_doctor_checkup_past_years_18plus" & input$rest_iv != "Select a Measure") {
        #   
        #   b1 <- filter(slider_data, var_name == "routine_doctor_checkup_past_years_18plus") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$rest_iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b1 * bm
        #   
        #   print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
        #   
        # } else if (input$rest_iv_top5 == "years_of_potential_life_lost_rate" & input$rest_iv != "Select a Measure") {
        #   
        #   b2 <- filter(slider_data, var_name == "years_of_potential_life_lost_rate") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$rest_iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b2 * bm
        #   
        #   print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
        #   
        # } else if 
        
        if (input$rest_iv_top5 == "avg_no_of_mentally_unhealthy_days" & input$rest_iv != "Select a Measure") {
          
          b1 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$rest_iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          # Get sign so it doesn't mess up the multiplier
          sign <- sign(bm)
          real_b <- b1 * bm * sign
          print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
        

        } else if (input$rest_iv_top5 == "per_physically_inactive" & input$rest_iv != "Select a Measure") {

          b2 <- filter(slider_data, var_name == "per_physically_inactive") %>%
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$rest_iv) %>%
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          # Get sign so it doesn't mess up the multiplier
          sign <- sign(bm)
          real_b <- b2 * bm * sign

          print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
        } else if (input$rest_iv_top5 == "per_adults_with_obesity" & input$rest_iv != "Select a Measure") {
          
          b3 <- filter(slider_data, var_name == "per_adults_with_obesity") %>%
            dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$rest_iv) %>%
            dplyr::select(b) %>% unlist() %>% as.numeric()
          # Get sign so it doesn't mess up the multiplier
          sign <- sign(bm)
          real_b <- b3 * bm * sign
          
          print(paste0("Mod Route(", input$rest_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b3, 3)))
          
      
        } else {
          
          real_b <- slider_data$b
          print(paste0("Normal Route: ", input$rest_iv_top5, "):", round(real_b, 3)))
          
        }
        
        print(paste0("slider change b: ", round(real_b), 3))
        
        return(real_b)
      
    })
    
    # Wor Index b change
    # Calculate the b to use for change from slider
    work_slider_change_b <- reactive({
      
      # Get weight aka slider data
      slider_data <- work_slider_data()
      slider_med_data <- work_slider_med_data()
      
      if (nrow(slider_data) == 0) {
        slider_data <- data.frame(b = 0)
      }
      
      if (nrow(slider_med_data) == 0) {
        slider_med_data <- data.frame(b = 0)
      }
      
      if (input$work_iv_top5 == "teen_birth_rate" & input$work_iv != "Select a Measure") {
        
        b1 <- filter(slider_data, var_name == "teen_birth_rate") %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        bm <- filter(slider_med_data, var_name == input$work_iv) %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        # Get sign so it doesn't mess up the multiplier
        sign <- sign(bm)
        real_b <- b1 * bm * sign
        
        print(paste0("Mod Route(", input$work_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
        
      } else if (input$work_iv_top5 == "per_w_grad_or_prof_degree" & input$work_iv != "Select a Measure") {
        
        b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        bm <- filter(slider_med_data, var_name == input$work_iv) %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        # Get sign so it doesn't mess up the multiplier
        sign <- sign(bm)
        real_b <- b2 * bm * sign
        
        print(paste0("Mod Route(", input$work_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
        
      } else if (input$work_iv_top5 == "avg_no_of_mentally_unhealthy_days" & input$work_iv != "Select a Measure") {
        
        b3 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        bm <- filter(slider_med_data, var_name == input$work_iv) %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        # Get sign so it doesn't mess up the multiplier
        sign <- sign(bm)
        real_b <- b3 * bm * sign
        print(paste0("Mod Route(", input$work_iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b3, 3)))
        
      } else {
        
        real_b <- slider_data$b
        print(paste0("Normal Route: ", input$work_iv_top5, "):", round(real_b, 3)))
        
      }
      
      print(paste0("slider change b: ", round(real_b), 3))
      
      return(real_b)
      
    })
  
    # Calculate Index County
    play_z_geo_df <- reactive({
      # Calculate index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
        # Change value?
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          value_change <- input$change_top5
        } else {
          value_change <- input$change
        }
      
       z_data_wgeo_long <- play_ana_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          focus = ifelse(state == input$state & county == input$county, 1, 0))
      
      if (input$region != "--") {
      z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, 0),
              focus = ifelse(is.na(focus), focusOrg, focus))
      }
       
       
      # Get real_b from slider
      play_slider_change_b.check <<- play_slider_change_b()
      real_b <- play_slider_change_b()
       
      if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        
        print("Second Layer")
      z_data_wgeo_long <- z_data_wgeo_long %>%
            mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + (input$change * real_b), value))
        
      } else if (input$iv_top5 == "Select a Measure" & input$iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
      z_data_wgeo_long <- z_data_wgeo_long %>%
            mutate(
            value = ifelse(focus == 1 & var_name == input$iv, value + input$change_top5, value))
        
      } else {
        
        
        print("Main Layer - Top 3 Measures")
      z_data_wgeo_long <- z_data_wgeo_long %>%
            mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        # ungroup() %>%
        # group_by(var_name) %>%
        mutate(
        #   focus = ifelse(state == input$state & county == input$county, 1, 0),
          # value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        ungroup() %>%
        mutate(
          score = play_w_per_fair_or_poor_health,
          quintile = ntile(score, 5)) # Use fair and poor health as a proxy
    
      # Add physical_inactivity_back
      phy_inactive_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_physically_inactive)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_uninsured, primary_care_physicians_ratio, per_unemployed, x20th_percentile_income, per_single_parent_households, social_association_rate, 
      # # TODO: fix it for real so index changes
      # per_american_indian_alaska_native, per_asian, per_hispanic, avg_no_of_mentally_unhealthy_days, per_adults_with_diabetes, number_single_parent_households, per_severe_housing_problems, overcrowding, per_homeowners) #violent_crime_rate, severe_housing_cost_burden
      
      z_data_wgeo <- left_join(z_data_wgeo, phy_inactive_wgeo, by = c("fips", "state", "county")) %>% ungroup()
      # left_join(additional_wgeo, by = c("fips", "state", "county")) %>%
      #     ungroup()
     
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      return(z_data_wgeo)
      
    })
    
    rest_z_geo_df <- reactive({
      # Calculate index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
      # Change value?
      if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
        value_change <- input$rest_change_top5
      } else {
        value_change <- input$rest_change
      }
      
      
      z_data_wgeo_long <- rest_ana_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0))
      
      if (input$rest_region != "--") {
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            focusOrg = focus,
            focus = ifelse(Region == input$rest_region, 1, 0),
            focus = ifelse(is.na(focus), focusOrg, focus))
      }
      
      
      # Get real_b from slider
      rest_slider_change_b.check <<- rest_slider_change_b()
      real_b <- rest_slider_change_b()
      
      if (input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + (input$rest_change * real_b), value))
        
      } else if (input$rest_iv_top5 == "Select a Measure" & input$rest_iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv, value + input$rest_change_top5, value))
        
      } else {
        
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + input$rest_change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0),
          # value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + input$rest_change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$rest_iv, value + input$rest_change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        ungroup() %>%
        mutate(
          score = play_w_avg_no_of_mentally_unhealthy_days,
          quintile = ntile(score, 5)) # Use fair and poor health as a proxy) # Use avg_no_of_mentally_unhealthy_days as a proxy
      
      # Add per_insufficient_sleep back
      per_insufficient_sleep_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_insufficient_sleep)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, primary_care_physicians_ratio) #violent_crime_rate, severe_housing_cost_burden x20th_percentile_income percent_uninsured percent_single_parent_households percent_unemployed social_association_rate
      
      # rest_z_data_1_wgeo <- left_join(rest_z_data_1_wgeo, additional_wgeo, by = c("fips", "state", "county")) %>%
      #   ungroup()
      
      z_data_wgeo <- left_join(z_data_wgeo, per_insufficient_sleep_wgeo, by = c("fips", "state", "county")) %>% ungroup()
      # left_join(additional_wgeo, by = c("fips", "state", "county")) %>%
      
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      return(z_data_wgeo)
      
    })
    
    work_z_geo_df <- reactive({
      # Calculate index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
      # Change value?
      if(input$work_iv_top5 != "Select a Measure" & input$work_iv == "Select a Measure") {
        value_change <- input$work_change_top5
      } else {
        value_change <- input$work_change
      }
      
      
      z_data_wgeo_long <- work_ana_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          focus = ifelse(state == input$work_state & county == input$work_county, 1, 0))
      
      if (input$work_region != "--") {
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            focusOrg = focus,
            focus = ifelse(Region == input$work_region, 1, 0),
            focus = ifelse(is.na(focus), focusOrg, focus))
      }
      
      
      # Get real_b from slider
      work_slider_change_b.check <<- work_slider_change_b()
      real_b <- work_slider_change_b()
      
      if (input$work_iv_top5 %in% measure_top3_lst_work & input$work_iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$work_iv_top5, value + (input$work_change * real_b), value))
        
      } else if (input$work_iv_top5 == "Select a Measure" & input$work_iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$work_iv, value + input$work_change_top5, value))
        
      } else {
        
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$work_iv_top5, value + input$work_change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # focus = ifelse(state == input$work_state & county == input$work_county, 1, 0),
          # value = ifelse(focus == 1 & var_name == input$work_iv_top5, value + input$work_change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$work_iv, value + input$work_change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        ungroup() %>%
        mutate(
          score = play_w_teen_birth_rate,
          quintile = ntile(score, 5)) # Use fair and poor health as a proxy) # Use teen birth rate as a proxy
      
      # Add per_w_a_disability back
      per_w_a_disability_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_w_a_disability)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, primary_care_physicians_ratio) #violent_crime_rate, severe_housing_cost_burden x20th_percentile_income percent_uninsured percent_single_parent_households percent_unemployed social_association_rate
      
      # work_z_data_1_wgeo <- left_join(work_z_data_1_wgeo, additional_wgeo, by = c("fips", "state", "county")) %>%
      #   ungroup()
      
      z_data_wgeo <- left_join(z_data_wgeo, per_w_a_disability_wgeo, by = c("fips", "state", "county")) %>% ungroup()
      # left_join(additional_wgeo, by = c("fips", "state", "county")) %>%
      
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      return(z_data_wgeo)
      
    })
    
    
    # Calculate Index Region
    play_z_geo_region_df <- reactive({
      # Calculate index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
        # Change value?
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          value_change <- input$change_top5
        } else {
          value_change <- input$change
        }
      
      z_data_wgeo_long <- play_ana_data_wgeo_long %>%
        mutate(focus = ifelse(state == input$state & Region == input$region, 1, 0))
      
      # Get real_b from slider
      play_slider_change_b.check <<- play_slider_change_b()
      real_b <- play_slider_change_b()
      
      if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + (input$change * real_b), value))
        
      } else if (input$iv_top5 == "Select a Measure" & input$iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$iv, value + input$change_top5, value))
        
      } else {
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, Region, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_per_fair_or_poor_health) # Use fair and poor health as a proxy
      
      # Add physical_inactivity_back
      phy_inactive_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_physically_inactive)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_uninsured, primary_care_physicians_ratio, per_unemployed, x20th_percentile_income, per_single_parent_households, social_association_rate) #violent_crime_rate, severe_housing_cost_burden
      
      z_data_wgeo <- left_join(z_data_wgeo, phy_inactive_wgeo, by = c("fips", "state", "county")) %>%
        ungroup()
      #left_join(additional_wgeo, by = c("fips", "state", "county")) 
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      
      return(z_data_wgeo)
      
    })
    
    rest_z_geo_region_df <- reactive({
      # Calculate index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
      # Change value?
      if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
        value_change <- input$rest_change_top5
      } else {
        value_change <- input$rest_change
      }
      
      z_data_wgeo_long <- rest_ana_data_wgeo_long %>%
        mutate(focus = ifelse(state == input$rest_state & Region == input$rest_region, 1, 0))
      
      # Get real_b from slider
      rest_slider_change_b.check <<- rest_slider_change_b()
      real_b <- rest_slider_change_b()
      
      if (input$rest_iv_top5 %in% measure_top3_lst_rest & input$rest_iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + (input$rest_change * real_b), value))
        
      } else if (input$rest_iv_top5 == "Select a Measure" & input$rest_iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv, value + input$rest_change_top5, value))
        
      } else {
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$rest_iv_top5, value + input$rest_change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, Region, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_avg_no_of_mentally_unhealthy_days) # Use avg_no_of_mentally_unhealthy_days as a proxy
      
      # Add per_insufficient_sleep back
      per_insufficient_sleep_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_insufficient_sleep)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_uninsured, primary_care_physicians_ratio, per_unemployed, x20th_percentile_income, per_single_parent_households, social_association_rate) #violent_crime_rate, severe_housing_cost_burden
      
      z_data_wgeo <- left_join(z_data_wgeo, per_insufficient_sleep_wgeo, by = c("fips", "state", "county")) %>%
        ungroup()
      #left_join(additional_wgeo, by = c("fips", "state", "county")) 
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      
      return(z_data_wgeo)
      
    })
    
    work_z_geo_region_df <- reactive({
      # Calculate index
      # Step 1 - Convert all metric to z score
      # Step 2 - Rescale it to 0 to 100
      # Step 3 - Filter to play iv Q: What about the DV?
      # Step 4 - Take the average (both unweighted and weighted using Pratt)
      
      # Change value?
      if(input$work_iv_top5 != "Select a Measure" & input$work_iv == "Select a Measure") {
        value_change <- input$work_change_top5
      } else {
        value_change <- input$work_change
      }
      
      z_data_wgeo_long <- work_ana_data_wgeo_long %>%
        mutate(focus = ifelse(state == input$work_state & Region == input$work_region, 1, 0))
      
      # Get real_b from slider
      work_slider_change_b.check <<- work_slider_change_b()
      real_b <- work_slider_change_b()
      
      if (input$work_iv_top5 %in% measure_top3_lst_work & input$work_iv != "Select a Measure") {
        
        print("Second Layer")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$work_iv_top5, value + (input$work_change * real_b), value))
        
      } else if (input$work_iv_top5 == "Select a Measure" & input$work_iv != "Select a Measure") {
        
        print("Main Layer - Other Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$work_iv, value + input$work_change_top5, value))
        
      } else {
        
        print("Main Layer - Top 3 Measures")
        z_data_wgeo_long <- z_data_wgeo_long %>%
          mutate(
            value = ifelse(focus == 1 & var_name == input$work_iv_top5, value + input$work_change_top5, value))
        
      }
      
      z_data_wgeo_long <- z_data_wgeo_long %>%
        ungroup() %>%
        group_by(var_name) %>%
        mutate(
          # value = ifelse(focus == 1 & var_name == input$iv_top5, value + input$change_top5, value),
          # value = ifelse(focus == 1 & var_name == input$iv, value + input$change, value),
          # value = ifelse(focus == 1 & var_name == "percent_fair_or_poor_health", value + input$change1, value),
          # value = ifelse(focus == 1 & var_name == "percent_adults_with_obesity", value + input$change2, value),
          # value = ifelse(focus == 1 & var_name == "percent_insufficient_sleep", value + input$change3, value),
          # value = ifelse(focus == 1 & var_name == "percent_smokers", value + input$change4, value),
          # value = ifelse(focus == 1 & var_name == "percent_excessive_drinking", value + input$change5, value),
          z_value = as.numeric(scale(value)),
          score = scales::rescale(z_value, c(0, 100)),
          score = ifelse(Direction == "N", 100 - score, score),
          play = ifelse(!is.na(b), 1, 0)) %>%
        filter(play == 1) %>%
        group_by(fips, state, county) %>%
        mutate(
          play_uw = mean(score, na.rm = T),
          play_w = weighted.mean(score, pratt, na.rm = T)
        ) %>%
        dplyr::select(-play)
      
      # Convert back to wide format
      z_data_wgeo <- z_data_wgeo_long %>%
        dplyr::select(fips, state, Region, county, var_name, value, z_value, score, play_uw, play_w) %>%
        pivot_wider(names_from = "var_name", values_from = c(value, z_value, score, play_uw, play_w)) %>%
        mutate(
          score = play_w_teen_birth_rate) # Use per teen birth rate as a proxy
      
      # Add per_w_a_disability back
      per_w_a_disability_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, population, per_w_a_disability)
      
      # Add new measure back
      # additional_wgeo <- dplyr::select(ana_data_full_wgeo, fips, state, county, per_uninsured, primary_care_physicians_ratio, per_unemployed, x20th_percentile_income, per_single_parent_households, social_association_rate) #violent_crime_rate, severe_housing_cost_burden
      
      z_data_wgeo <- left_join(z_data_wgeo, per_w_a_disability_wgeo, by = c("fips", "state", "county")) %>%
        ungroup()
      #left_join(additional_wgeo, by = c("fips", "state", "county")) 
      
      names(z_data_wgeo) <- str_replace_all(names(z_data_wgeo), "^value_", "")
      
      
      return(z_data_wgeo)
      
    })

  
    # Create data subset
    play_select_geo_df <- reactive({
      
        data <- filter(play_z_geo_df(), state == input$state, county == input$county)
        return(data)
        
    })
    
    play_select_geo_region_df <- reactive({
      
        data <- filter(play_z_geo_region_df(), state == input$state, Region == input$region)
        return(data)
        
    })
    
    rest_select_geo_df <- reactive({
        data <- filter(rest_z_geo_df(), state == input$rest_state, county == input$rest_county)
        
        return(data)
    })
    
    rest_select_geo_region_df <- reactive({
      
      data <- filter(rest_z_geo_region_df(), state == input$rest_state, Region == input$rest_region)
      return(data)
      
    })
    
    work_select_geo_df <- reactive({
      data <- filter(work_z_geo_df(), state == input$work_state, county == input$work_county)
      
      return(data)
    })
    
    work_select_geo_region_df <- reactive({
      
      bcheck <<- work_z_geo_region_df()
      print(input$work_state)
      print(input$work_region)
      
      data <- filter(work_z_geo_region_df(), state == input$work_state, Region == input$work_region)
      return(data)
      
    })
    
    # Get weight for iv
    play_slider_data <- reactive({
      
      data <- filter(m_step_df_play, var_name == input$iv | var_name == input$iv_top5)
        # data <- filter(m_step_df_play, var_name == input$iv | var_name %in% c(
        #    "percent_fair_or_poor_health",
        #    "percent_adults_with_obesity",
        #    "percent_insufficient_sleep",
        #    "percent_smokers",
        #    "percent_excessive_drinking"
        # ))
    })
    
    play_slider_med_data <- reactive({
      
      # double b if fair and poor health as mediator
      if (input$iv_top5 == "per_fair_or_poor_health") {
        data <- filter(m_step_df_fp_health, var_name == input$iv)
      } else if (input$iv_top5 == "per_w_grad_or_prof_degree") {
        data <- filter(m_step_df_grad, var_name == input$iv)      
      } else if (input$iv_top5 == "per_adults_with_diabetes") {
        data <- filter(m_step_df_diabetes, var_name == input$iv)
      } else {
        data <- data.frame(NA)
      }
      
      return(data)
      
    })
    
    rest_slider_data <- reactive({
      
      data <- filter(m_step_df_rest, var_name == input$rest_iv | var_name == input$rest_iv_top5)

        # data <- filter(m_step_df_rest, var_name == input$iv | var_name %in% c(
        #    "percent_fair_or_poor_health",
        #    "percent_adults_with_obesity",
        #    "percent_insufficient_sleep",
        #    "percent_smokers",
        #    "percent_excessive_drinking"
        # ))
    })
    
    rest_slider_med_data <- reactive({
      
      # double b if fair and poor health as mediator
      if (input$rest_iv_top5 == "avg_no_of_mentally_unhealthy_days") {
        data <- filter(m_step_df_avg_m_days, var_name == input$rest_iv)
      } else if (input$rest_iv_top5 == "per_physically_inactive") {
        data <- filter(m_step_df_phy_inactive, var_name == input$rest_iv)      
      } else if (input$rest_iv_top5 == "per_adults_with_obesity") {
        data <- filter(m_step_df_obesity, var_name == input$rest_iv)
      } else {
        data <- data.frame(NA)
      }
      
      return(data)
      
    })
    
    work_slider_data <- reactive({
      
      data <- filter(m_step_df_work, var_name == input$work_iv | var_name == input$work_iv_top5)
      
      # data <- filter(m_step_df_rest, var_name == input$iv | var_name %in% c(
      #    "percent_fair_or_poor_health",
      #    "percent_adults_with_obesity",
      #    "percent_insufficient_sleep",
      #    "percent_smokers",
      #    "percent_excessive_drinking"
      # ))
    })
    
    work_slider_med_data <- reactive({
      
      # double b if fair and poor health as mediator
      if (input$work_iv_top5 == "teen_birth_rate") {
        data <- filter(m_step_df_teen_brate, var_name == input$work_iv)
      } else if (input$work_iv_top5 == "per_w_grad_or_prof_degree") {
        data <- filter(m_step_df_grad, var_name == input$work_iv)      
      } else if (input$work_iv_top5 == "avg_no_of_mentally_unhealthy_days") {
        data <- filter(m_step_df_avg_m_days, var_name == input$work_iv)
      } else {
        data <- data.frame(NA)
      }
      
      return(data)
      
    })
    
    
    # Get weight for criterion
    criterion_slider_data <- reactive({
         # years_of_potential_life_lost_rate,
         # avg_no_of_physically_unhealthy_days,
         # avg_no_of_mentally_unhealthy_days,
         # preventable_hospitalization_rate
    })
    
    # Chart data
    plot_data <- reactive({
        data <- filter(play_z_geo_df(), state == input$state)
        data <- mutate(data, 
          focus = ifelse(state == input$state & county == input$county, 1, 0))
        return(data)
    })
    
   plot_region_data <- reactive({
        data <- filter(z_geo_region_df(), state == input$state)
        data <- mutate(data, 
          focus = ifelse(state == input$state & Region == input$region, 1, 0))
        return(data)
    })
    
    rest_plot_data <- reactive({
        data <- filter(rest_z_geo_df(), state == input$rest_state)
        
        data <- mutate(data, 
          focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0))
        return(data)
    })
    
    work_plot_data <- reactive({
      data <- filter(work_z_geo_df(), state == input$work_state)
      
      data <- mutate(data, 
                     focus = ifelse(state == input$work_state & county == input$work_county, 1, 0))
      return(data)
    })
    
    # --- InfoBox ---
    output$population <- renderInfoBox({
        
      # Region Pop
        pop_region <- play_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(population, sum, na.rm = T)) %>%
          dplyr::select(population) %>%
            unlist() %>%
            as.character()
      
      # County Pop
        pop_county <- play_select_geo_df() %>%
           dplyr::select(population) %>%
            unlist() %>%
            as.character()
        
        if (input$region != "--") {
          pop <- pop_region
          label <- "Region"
        } else {
          pop <- pop_county
          label <- 'County'
        }
        
        infoBox(
          paste(label, " Population"), pop,
          fill = TRUE,
          color = "teal"
        )
        
    })
    
    output$rest_population <- renderInfoBox({
      
      # Region Pop
      pop_region <- rest_select_geo_region_df() %>%
        group_by(Region) %>% 
        summarise(across(population, sum, na.rm = T)) %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.character()
      
      # County Pop
      pop_county <- rest_select_geo_df() %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.character()
      
      if (input$rest_region != "--") {
        pop <- pop_region
        label <- "Region"
      } else {
        pop <- pop_county
        label <- 'County'
      }
      
      infoBox(
        paste(label, " Population"), pop,
        fill = TRUE,
        color = "teal"
      )
      
    })
    
    output$work_population <- renderInfoBox({
      
      # Region Pop
      pop_region <- work_select_geo_region_df() %>%
        group_by(Region) %>% 
        summarise(across(population, sum, na.rm = T)) %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.character()
      
      # County Pop
      pop_county <- work_select_geo_df() %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.character()
      
      if (input$work_region != "--") {
        pop <- pop_region
        label <- "Region"
      } else {
        pop <- pop_county
        label <- 'County'
      }
      
      infoBox(
        paste(label, " Population"), pop,
        fill = TRUE,
        color = "teal"
      )
      
    })
    
    
    output$pop_impact <- renderInfoBox({
      
        # Region Pop
        pop_region <- play_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(population, sum, na.rm = T)) %>%
          dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        
        pop_county <- play_select_geo_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        
        if (input$region != "--") {
          value <- pop_region
        } else {
          value <- pop_county
        }
        
         # slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        slider_data.check <<-  slider_data
        slider_med_data.check <<- slider_med_data
        
        # b <- filter(slider_data, var_name == input$iv) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # 
        #   # if (input$iv_top5 == "percent_fair_or_poor_health") {
        #   #  b <- filter(slider_med_data, var_name == input$iv) %>%
        #   #    dplyr::select(b) %>% unlist() %>% as.numeric()
        #   # }
        # 
        # b_top5 <- filter(slider_data, var_name == input$iv_top5) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        
        # b1 <- filter(slider_data, var_name == "per_adults_with_obesity") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b3 <- filter(slider_data, var_name == "per_with_access_to_exercise_oppurtunities") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b4 <- filter(slider_data, var_name == "per_excessive_drinking") %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b5 <- filter(slider_data, var_name == "per_insufficient_sleep") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b1 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b1))
        # } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>%
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b2 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        # } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #  
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b3 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b3))
        #  
        # } else {
        #   # print(paste("Normal Route: ", b))
        # }
        
       # Get real_b from slider
       play_slider_change_b.check <<- play_slider_change_b()
       real_b <- play_slider_change_b()
        
        value2_0 <- round(value * ((real_b * input$change)/100), 0)
        value2_top5 <- round(value * ((real_b * input$change_top5)/100), 0)
       
        # value2_0 <- round(value * ((b * input$change)/100), 0)
        # value2_top5 <- round(value * ((b_top5 * input$change_top5)/100), 0)
        # value2_1 <- abs(round(value * ((b1 * input$change1)/100), 0))
        # value2_2 <- abs(round(value * ((b2 * input$change2)/100), 0))
        # value2_3 <- abs(round(value * ((b3 * input$change3)/100), 0))
        # value2_4 <- abs(round(value * ((b4 * input$change4)/100), 0))
        # value2_5 <- abs(round(value * ((b5 * input$change5)/100), 0))
        
        value2 <- abs(sum(value2_0, value2_top5, na.rm = T))
        
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
        


        value2_per = round((value2 / value) * 100, 1)
        # 
        # print(paste0("pop impact:", value2, "      ", value, "      ", value2_per, "%"))
        # 
        value3 <- paste0(value2, " (", value2_per, "%)")
        
        infoBox(
          "Est' Population Impacted", value3, 
          fill = TRUE,
          color = "teal"
        )
    })
    
    output$rest_pop_impact <- renderInfoBox({
        
      # Region Pop
      pop_region <- rest_select_geo_region_df() %>%
        group_by(Region) %>% 
        summarise(across(population, sum, na.rm = T)) %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.numeric()
      
      pop_county <- rest_select_geo_df() %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.numeric()
      
      if (input$rest_region != "--") {
        value <- pop_region
      } else {
        value <- pop_county
      }
        
         # slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        slider_data.check <<-  slider_data
        slider_med_data.check <<- slider_med_data
        
        # b <- filter(slider_data, var_name == input$rest_iv) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b_top5 <- filter(slider_data, var_name == input$rest_iv_top5) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b1 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b2 <- filter(slider_data, var_name == "per_smokers") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b3 <- filter(slider_data, var_name == "per_black") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b4 <- filter(slider_data, var_name == "average_daily_pm2_5") %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b5 <- filter(slider_data, var_name == "per_non_hispanic_white") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        
       # Get real_b from slider
       rest_slider_change_b.check <<- rest_slider_change_b()
       real_b <- rest_slider_change_b()
        
        value2_0 <- round(value * ((real_b * input$rest_change)/100), 0)
        value2_top5 <- round(value * ((real_b * input$rest_change_top5)/100), 0)
        # value2_0 <- round(value * ((b * input$rest_change)/100), 0)
        # value2_top5 <- round(value * ((b_top5 * input$rest_change_top5)/100), 0)
        # value2_1 <- abs(round(value * ((b1 * input$rest_change1)/100), 0))
        # value2_2 <- abs(round(value * ((b2 * input$rest_change2)/100), 0))
        # value2_3 <- abs(round(value * ((b3 * input$rest_change3)/100), 0))
        # value2_4 <- abs(round(value * ((b4 * input$rest_change4)/100), 0))
        # value2_5 <- abs(round(value * ((b5 * input$rest_change5)/100), 0))
        
        value2 <- abs(sum(value2_0, value2_top5, na.rm = T))
        
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)

        value2_per = round((value2 / value) * 100, 1)
        
        value3 <- paste0(value2, " (", value2_per, "%)")
        
        infoBox(
          "Est' Population Impacted", value3, 
          fill = TRUE,
          color = "teal"
        )
    })
    
    output$work_pop_impact <- renderInfoBox({
      
      # Region Pop
      pop_region <- work_select_geo_region_df() %>%
        group_by(Region) %>% 
        summarise(across(population, sum, na.rm = T)) %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.numeric()
      
      pop_county <- work_select_geo_df() %>%
        dplyr::select(population) %>%
        unlist() %>%
        as.numeric()
      
      if (input$work_region != "--") {
        value <- pop_region
      } else {
        value <- pop_county
      }
      
      # slider data
      slider_data <- work_slider_data()
      slider_med_data <- work_slider_med_data()
      
      slider_data.check <<-  slider_data
      slider_med_data.check <<- slider_med_data
      
      # b <- filter(slider_data, var_name == input$rest_iv) %>%
      #   dplyr::select(b) %>% unlist() %>% as.numeric()
      # b_top5 <- filter(slider_data, var_name == input$rest_iv_top5) %>%
      #   dplyr::select(b) %>% unlist() %>% as.numeric()
      # b1 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>%
      #  dplyr::select(b) %>% unlist() %>% as.numeric()
      # b2 <- filter(slider_data, var_name == "per_smokers") %>%
      #  dplyr::select(b) %>% unlist() %>% as.numeric()
      # b3 <- filter(slider_data, var_name == "per_black") %>%
      #  dplyr::select(b) %>% unlist() %>% as.numeric()
      # b4 <- filter(slider_data, var_name == "average_daily_pm2_5") %>%
      #   dplyr::select(b) %>% unlist() %>% as.numeric()
      # b5 <- filter(slider_data, var_name == "per_non_hispanic_white") %>%
      #  dplyr::select(b) %>% unlist() %>% as.numeric()
      
      # Get real_b from slider
      work_slider_change_b.check <<- work_slider_change_b()
      real_b <- work_slider_change_b()
      
      value2_0 <- round(value * ((real_b * input$work_change)/100), 0)
      value2_top5 <- round(value * ((real_b * input$work_change_top5)/100), 0)
      # value2_0 <- round(value * ((b * input$rest_change)/100), 0)
      # value2_top5 <- round(value * ((b_top5 * input$rest_change_top5)/100), 0)
      # value2_1 <- abs(round(value * ((b1 * input$rest_change1)/100), 0))
      # value2_2 <- abs(round(value * ((b2 * input$rest_change2)/100), 0))
      # value2_3 <- abs(round(value * ((b3 * input$rest_change3)/100), 0))
      # value2_4 <- abs(round(value * ((b4 * input$rest_change4)/100), 0))
      # value2_5 <- abs(round(value * ((b5 * input$rest_change5)/100), 0))
      
      value2 <- abs(sum(value2_0, value2_top5, na.rm = T))
      
      # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
      
      value2_per = round((value2 / value) * 100, 1)
      
      value3 <- paste0(value2, " (", value2_per, "%)")
      
      infoBox(
        "Est' Population Impacted", value3, 
        fill = TRUE,
        color = "teal"
      )
    })
    
    output$iv_echo <- renderInfoBox({
       
        value_region <- play_select_geo_region_df() %>% 
          group_by(Region) %>% 
          summarise(across(input$iv, mean, na.rm = T)) %>% 
          dplyr::select(input$iv) %>%
            unlist() %>%
            as.numeric()  
      
        value_county <- play_select_geo_df() %>%
            dplyr::select(input$iv) %>%
            unlist() %>%
            as.numeric()
        
        if (input$region != "--") {
          value <- value_region
        } else {
          value <- value_county
        }
        
        value <- round(value, 1)
        
        infoBox(
          input$iv, value, 
          fill = TRUE,
          color = "yellow"
        )
        
    })
    
    output$iv_change <- renderInfoBox({
        
        value <- play_select_geo_df() %>%
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
        
        rank_df <- ana_data_wgeo %>%
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
        
        # value <- play_select_geo_df() %>%
        #     dplyr::select(percent_physically_inactive) %>%
        #     unlist() %>%
        #     as.numeric()
        # 
        # value <- round(value, 1)
        
        # Get data
        data <- plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$region != "--") {
          data <- data %>% mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        # Make focus generic
        # if (input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
        # data <- data %>%
        #     rename(
        #       "top5_iv" = input$iv_top5)  %>%
        #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
        # }
        
        # if (input$iv != "Select a Measure") {
        # 
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
        # }
        
       # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "focus_iv" = input$iv_top5) %>%
       #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
       #  }
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b1 * bm
        #   print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b1, 3)))
        # } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
        #   b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b2 * bm
        #   print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b2, 3)))
        # } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
        #   b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b3 * bm
        #   print(paste0("Mod Route(", input$iv, "): ", round(real_b, 3), " = ", round(bm, 3), " * ", round(b3, 3)))
        # 
        # } else {
        #   real_b <- slider_data$b
        #   print(paste0("Normal Route: ", input$iv_top5, "):", round(real_b, 3)))
        # }
        
       # Get real_b from slider
       play_slider_change_b.check <<- play_slider_change_b()
       real_b <- play_slider_change_b()
        
        # Modify the data
        data <- mutate(data,
          label = paste0(county, "\n", state),
          label = ifelse(focus == 1, label, NA),
          per_physically_inactive = ifelse(focus == 1, (per_physically_inactive + (real_b * xchange)), per_physically_inactive))
        
        # cat(print(paste0(slider_data$var_name, " - ", slider_data$b, "\n", real_b)))
        
        if (input$region != "--") {
          
          # data <- left_join(data, region_lkup, by = c("fips" = "FIPS")) %>%
          #   mutate(
          #     focusOrg = focus,
          #     focus = ifelse(Region == input$region, 1, focus),
          #     focus = ifelse(is.na(focus), focusOrg, focus))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$region) %>%
            mutate(label = paste0(Region, "  - ", input$state),
                   label = ifelse(focus == 1, label, NA))
          
         data <- region_data
          
        } else {
        
        data <- data %>%
          filter(state == input$state, county == input$county)
        
        }
        
        value <- round(data$per_physically_inactive, 1)
        
        infoBox(
          HTML(paste("% of Population",br(), "Physically Inactive")),
          paste0(value, " (", round(real_b * xchange, 1), ")"), 
          fill = TRUE,
          color = "navy"
        )
        
    })
    
    output$rest_dv_echo <- renderInfoBox({
        
        # Get data
        data <- rest_plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$rest_region != "--") {
          data <- data %>% mutate(
            focusOrg = focus,
            focus = ifelse(Region == input$rest_region, 1, focus),
            focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        # Make focus generic
        # if (input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
        # data <- data %>%
        #     rename(
        #       "top5_iv" = input$rest_iv_top5)  %>%
        #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$rest_change_top5, top5_iv))
        # }

        # if (input$rest_iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$rest_iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$rest_change, focus_iv))
        # }
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xchange <- input$rest_change_top5
        } else {
          xchange <- input$rest_change
        }
        
       # Get real_b from slider
       rest_slider_change_b.check <<- rest_slider_change_b()
       real_b <- rest_slider_change_b()
        
        # Modify the data
        data <- mutate(data,
          label = paste0(county, "\n", state),
          label = ifelse(focus == 1, label, NA),
          per_insufficient_sleep = ifelse(focus == 1, (per_insufficient_sleep + (real_b * xchange)), per_insufficient_sleep))
        
        if (input$rest_region != "--") {
          
          # data <- left_join(data, region_lkup, by = c("fips" = "FIPS")) %>%
          #   mutate(
          #     focusOrg = focus,
          #     focus = ifelse(Region == input$region, 1, focus),
          #     focus = ifelse(is.na(focus), focusOrg, focus))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$rest_region) %>%
            mutate(label = paste0(Region, "  - ", input$rest_state),
                   label = ifelse(focus == 1, label, NA))
          
          data <- region_data
          
        } else {
          
          data <- data %>%
            filter(state == input$rest_state, county == input$rest_county)
          
        }
        
        value <- round(data$per_insufficient_sleep, 1)
        
        infoBox(
          HTML(paste("% of Population", br(), "with Insufficient Sleep")),
          paste0(value, " (", round(real_b * xchange, 1), ")"), 
          fill = TRUE,
          color = "navy"
        )
        
    })
    
    output$work_dv_echo <- renderInfoBox({
      
      # Get data
      data <- work_plot_data()
      
      data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
      
      if (input$work_region != "--") {
        data <- data %>% mutate(
          focusOrg = focus,
          focus = ifelse(Region == input$work_region, 1, focus),
          focus = ifelse(is.na(focus), focusOrg, focus))
      }
      
      # Get weight aka slider data
      slider_data <- work_slider_data()
      slider_med_data <- work_slider_med_data()
      
      if (nrow(slider_data) == 0) {
        slider_data <- data.frame(b = 0)
      }
      
      # Make focus generic
      # if (input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
      # data <- data %>%
      #     rename(
      #       "top5_iv" = input$rest_iv_top5)  %>%
      #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$rest_change_top5, top5_iv))
      # }
      
      # if (input$rest_iv != "Select a Measure") {
      #   
      # data <- data %>%
      #     rename(
      #       "focus_iv" = input$rest_iv) %>%
      #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$rest_change, focus_iv))
      # }
      
      # Change x axis
      if(input$work_iv_top5 != "Select a Measure" & input$work_iv == "Select a Measure") {
        xchange <- input$work_change_top5
      } else {
        xchange <- input$work_change
      }
      
      # Get real_b from slider
      work_slider_change_b.check <<- work_slider_change_b()
      real_b <- work_slider_change_b()
      
      # Modify the data
      data <- mutate(data,
                     label = paste0(county, "\n", state),
                     label = ifelse(focus == 1, label, NA),
                     per_w_a_disability = ifelse(focus == 1, (per_w_a_disability + (real_b * xchange)), per_w_a_disability))
      
      if (input$work_region != "--") {
        
        # data <- left_join(data, region_lkup, by = c("fips" = "FIPS")) %>%
        #   mutate(
        #     focusOrg = focus,
        #     focus = ifelse(Region == input$region, 1, focus),
        #     focus = ifelse(is.na(focus), focusOrg, focus))
        
        region_data <- data %>% 
          group_by(Region) %>% 
          summarise(across(where(is.numeric), mean, na.rm = T)) %>%
          filter(Region == input$work_region) %>%
          mutate(label = paste0(Region, "  - ", input$work_state),
                 label = ifelse(focus == 1, label, NA))
        
        data <- region_data
        
      } else {
        
        data <- data %>%
          filter(state == input$work_state, county == input$work_county)
        
      }
      
      value <- round(data$per_w_a_disability, 1)
      
      infoBox(
        HTML(paste("% of Population", br(), "with a cognitive or", br(), "physical impairment")),
        paste0(value, " (", round(real_b * xchange, 1), ")"), 
        fill = TRUE,
        color = "navy"
      )
      
    })
    
    output$dv_rank <- renderInfoBox({
      
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
        if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
          b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$iv) %>% 
                 dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b2 * bm
          # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
        } else {
          real_b <- slider_data$b
        }
        
        if (nrow(slider_data) < 1) {real_b <- 0}
        
        rank_df <-  ana_data_wgeo %>%
            dplyr::select(fips, state, county, per_physically_inactive) %>%
            mutate(
             focus = ifelse(state == input$state & county == input$county, 1, 0),
             per_physically_inactive = ifelse(focus == 1, (per_physically_inactive + (real_b * xchange)), per_physically_inactive),
              rank_value = rank(per_physically_inactive),
              per_rank_value = (1 - percent_rank(per_physically_inactive)) * 100
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
    
    output$rest_dv_rank <- renderInfoBox({
      
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xchange <- input$rest_change_top5
        } else {
          xchange <- input$rest_change
        }
        
        if (input$rest_iv_top5 == "avg_no_of_mentally_unhealthy_days" & input$rest_iv != "Select a Measure") {
          b2 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>% 
            dplyr::select(b) %>% unlist() %>% as.numeric()
          bm <- filter(slider_med_data, var_name == input$rest_iv) %>% 
            dplyr::select(b) %>% unlist() %>% as.numeric()
          real_b <- b2 * bm
          # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
        } else {
          real_b <- slider_data$b
        }
        
        if (nrow(slider_data) < 1) {real_b <- 0}
        
        rank_df <-  ana_data_wgeo %>%
            dplyr::select(fips, state, county, per_insufficient_sleep) %>%
            mutate(
             focus = ifelse(state == input$rest_state & county == input$rest_county, 1, 0),
             per_insufficient_sleep = ifelse(focus == 1, (per_insufficient_sleep + (real_b * xchange)), per_insufficient_sleep),
              rank_value = rank(per_insufficient_sleep),
              per_rank_value = (1 - percent_rank(per_insufficient_sleep)) * 100
            ) %>%
            dplyr::filter(state == input$rest_state, county == input$rest_county)
        
        rank_df.check <<- rank_df
        
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
    
    output$work_dv_rank <- renderInfoBox({
      
      # Get weight aka slider data
      slider_data <- work_slider_data()
      slider_med_data <- work_slider_med_data()
      
      
      # Change x axis
      if(input$work_iv_top5 != "Select a Measure" & input$work_iv == "Select a Measure") {
        xchange <- input$work_change_top5
      } else {
        xchange <- input$work_change
      }
      
      if (input$work_iv_top5 == "teen_birth_rate" & input$work_iv != "Select a Measure") {
        b2 <- filter(slider_data, var_name == "teen_birth_rate") %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        bm <- filter(slider_med_data, var_name == input$work_iv) %>% 
          dplyr::select(b) %>% unlist() %>% as.numeric()
        real_b <- b2 * bm
        # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
      } else {
        real_b <- slider_data$b
      }
      
      if (nrow(slider_data) < 1) {real_b <- 0}
      
      rank_df <-  ana_data_wgeo %>%
        dplyr::select(fips, state, county, per_w_a_disability) %>%
        mutate(
          focus = ifelse(state == input$work_state & county == input$work_county, 1, 0),
          per_w_a_disability = ifelse(focus == 1, (per_w_a_disability + (real_b * xchange)), per_w_a_disability),
          rank_value = rank(per_w_a_disability),
          per_rank_value = (1 - percent_rank(per_w_a_disability)) * 100
        ) %>%
        dplyr::filter(state == input$work_state, county == input$work_county)
      
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
        
        value_region <- play_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(score, mean, na.rm = T)) %>%
           dplyr::select(score) %>%
            unlist() %>%
            as.numeric()
        
        value_county <- play_select_geo_df() %>%
            dplyr::select(score) %>%
            unlist() %>%
            as.numeric()
      
        
        if (input$region != "--") {
          value <- value_region
        } else {
          value <- value_county
        }
        
        # Add original to calculate change
        value_fixed <- play_fixed_z_data_wgeo %>%
          filter(state == input$state, county == input$county) %>%
          pull(score)
        
        change <- round(value - value_fixed, 1)
        
        value_fixed <- round(value_fixed, 1)
        
        value <- round(value, 1)
        
        infoBox(
          "Play Index", 
          paste0(value, " (", change, ")"), 
          fill = TRUE,
          color = "blue"
        )
        
     })
      
      output$rest_index_echo <- renderInfoBox({
        
        value_region <- rest_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(score, mean, na.rm = T)) %>%
          dplyr::select(score) %>%
          unlist() %>%
          as.numeric()
        
        value_county <- rest_select_geo_df() %>%
          dplyr::select(score) %>%
          unlist() %>%
          as.numeric()
        
        if (input$rest_region != "--") {
          value <- value_region
        } else {
          value <- value_county
        }
        
        # Add original to calculate change
        value_fixed <- rest_fixed_z_data_wgeo %>%
          filter(state == input$state, county == input$county) %>%
          pull(score)
        
        change <- round(value - value_fixed, 1)
        
        value_fixed <- round(value_fixed, 1)
        
        value <- round(value, 1)
        
        infoBox(
          "Rest Index", 
          paste0(value, " (", change, ")"), 
          fill = TRUE,
          color = "blue"
        )
        
     })
      
      output$work_index_echo <- renderInfoBox({
        
        value_region <- work_select_geo_region_df() %>%
          group_by(Region) %>% 
          summarise(across(score, mean, na.rm = T)) %>%
          dplyr::select(score) %>%
          unlist() %>%
          as.numeric()
        
        value_county <- work_select_geo_df() %>%
          dplyr::select(score) %>%
          unlist() %>%
          as.numeric()
        
        if (input$work_region != "--") {
          value <- value_region
        } else {
          value <- value_county
        }
        
        # Add original to calculate change
        value_fixed <- work_fixed_z_data_wgeo %>%
          filter(state == input$state, county == input$county) %>%
          pull(score)
        
        change <- round(value - value_fixed, 1)
        
        value_fixed <- round(value_fixed, 1)
        
        value <- round(value, 1)
        
        infoBox(
          "Work Index", 
          paste0(value, " (", change, ")"),
          fill = TRUE,
          color = "blue"
        )
        
      })
        
      output$index_rank <- renderInfoBox({
        
        rank_df <- play_z_geo_df() %>%
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
      
      output$rest_index_rank <- renderInfoBox({
        
        rank_df <- rest_z_geo_df() %>%
            dplyr::select(fips, state, county, score) %>%
            mutate(
              rank_value = rank(-score),
              per_rank_value = percent_rank(score) * 100
            ) %>%
            dplyr::filter(state == input$rest_state, county == input$rest_county)
        
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
      
      output$work_index_rank <- renderInfoBox({
        
        rank_df <- work_z_geo_df() %>%
          dplyr::select(fips, state, county, score) %>%
          mutate(
            rank_value = rank(-score),
            per_rank_value = percent_rank(score) * 100
          ) %>%
          dplyr::filter(state == input$work_state, county == input$work_county)
        
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
        
        value <- cor(data$score, data$avg_no_of_physically_unhealthy_days, 
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
        
        value <- cor(data$score, data$avg_no_of_mentally_unhealthy_days, 
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
     
    
    # --- Extra Row Card ---
     
    # Physical Inactivity change for card
    phy_inactive_change <- reactive({
      
        # Physical Inactivity
        # Get data
        data <- plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$region != "--") {
          data <- data %>% mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        # Make focus generic
       #  if (input$iv_top5 != "Select a Measure"  & input$rest_iv == "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "top5_iv" = input$iv_top5)  %>%
       #    mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
       #  }
       #  
       # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "focus_iv" = input$iv_top5) %>%
       #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
       #  }
        
        # 
        # if (input$iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
        # }
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   real_b <- b2 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
        # } else {
        #   real_b <- slider_data$b
        # }
        
       # Get real_b from slider
       real_b <- play_slider_change_b()
        
        change <- real_b * xchange
        
        return(change)
        
    })
    
    
    # Insufficient sleep change for card
    insufficient_sleep_change <- reactive({
      
      # Physical Inactivity
      # Get data
      data <- rest_plot_data()
      
      data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
      
      if (input$rest_region != "--") {
        data <- data %>% mutate(
          focusOrg = focus,
          focus = ifelse(Region == input$rest_region, 1, focus),
          focus = ifelse(is.na(focus), focusOrg, focus))
      }
      
      # Get weight aka slider data
      slider_data <- rest_slider_data()
      slider_med_data <- rest_slider_med_data()
      
      if (nrow(slider_data) == 0) {
        slider_data <- data.frame(b = 0)
      }
      
      # Make focus generic
      #  if (input$iv_top5 != "Select a Measure"  & input$rest_iv == "Select a Measure") {
      #  data <- data %>%
      #      rename(
      #        "top5_iv" = input$iv_top5)  %>%
      #    mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
      #  }
      #  
      # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
      #  data <- data %>%
      #      rename(
      #        "focus_iv" = input$iv_top5) %>%
      #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
      #  }
      
      # 
      # if (input$iv != "Select a Measure") {
      #   
      # data <- data %>%
      #     rename(
      #       "focus_iv" = input$iv) %>%
      #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
      # }
      
      # Change x axis
      if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
        xchange <- input$rest_change_top5
      } else {
        xchange <- input$rest_change
      }
      
      # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
      #   b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
      #          dplyr::select(b) %>% unlist() %>% as.numeric()
      #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
      #          dplyr::select(b) %>% unlist() %>% as.numeric()
      #   real_b <- b2 * bm
      #   # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
      # } else {
      #   real_b <- slider_data$b
      # }
      
      # Get real_b from slider
      real_b <- rest_slider_change_b()
      
      change <- real_b * xchange
      
      return(change)
      
    })
    
    
    # Per with Disability change for card
    per_w_disability_change <- reactive({
      
      # Physical Inactivity
      # Get data
      data <- work_plot_data()
      
      data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
      
      if (input$work_region != "--") {
        data <- data %>% mutate(
          focusOrg = focus,
          focus = ifelse(Region == input$work_region, 1, focus),
          focus = ifelse(is.na(focus), focusOrg, focus))
      }
      
      # Get weight aka slider data
      slider_data <- work_slider_data()
      slider_med_data <- work_slider_med_data()
      
      if (nrow(slider_data) == 0) {
        slider_data <- data.frame(b = 0)
      }
      
      # Make focus generic
      #  if (input$iv_top5 != "Select a Measure"  & input$rest_iv == "Select a Measure") {
      #  data <- data %>%
      #      rename(
      #        "top5_iv" = input$iv_top5)  %>%
      #    mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
      #  }
      #  
      # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
      #  data <- data %>%
      #      rename(
      #        "focus_iv" = input$iv_top5) %>%
      #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
      #  }
      
      # 
      # if (input$iv != "Select a Measure") {
      #   
      # data <- data %>%
      #     rename(
      #       "focus_iv" = input$iv) %>%
      #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
      # }
      
      # Change x axis
      if(input$work_iv_top5 != "Select a Measure" & input$work_iv == "Select a Measure") {
        xchange <- input$work_change_top5
      } else {
        xchange <- input$work_change
      }
      
      # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
      #   b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>% 
      #          dplyr::select(b) %>% unlist() %>% as.numeric()
      #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
      #          dplyr::select(b) %>% unlist() %>% as.numeric()
      #   real_b <- b2 * bm
      #   # print(paste0("Mod Route(", input$iv, "): ", real_b, " = ", bm, " * ", b2))
      # } else {
      #   real_b <- slider_data$b
      # }
      
      # Get real_b from slider
      real_b <- work_slider_change_b()
      
      change <- real_b * xchange
      
      return(change)
      
    })
    
    # Extra data grouping
    extra_data_for_card <- reactive({
        
        select_data <- play_z_geo_df()
        data <- left_join(select_data, ana_data_criterion, by = c("fips", "state", "county"))
        
        data <- data %>%
          left_join(data_labour_1, by = c("fips" = "FIPS"))
  
        if (input$region != "--") {
          
          data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
          
          data <- data %>% 
            filter(Region == input$region) %>%
            ungroup() %>%
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T))
           
        } else {
          
          data <- filter(data, state == input$state, county == input$county)
          
        } 
        
        return(data)
    })

     
    # New cards (4):  
     # Years of Potential Life Lost Rate, 
     # Average Number of Physically Unhealthy Days, 
     # Average Number of Mentally Unhealthy Days, 
     # Premature age-adjusted mortality
     
    output$play_impact_card_1 <- renderInfoBox({
      
        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
      
        # value <- cor(data$score, data$years_of_potential_life_lost_rate, 
        #     method = "pearson", use = "complete.obs")
        
        model <- lm(years_of_potential_life_lost_rate ~ score, data = data) %>%
        tidy()
        
        b <- model$estimate[2]
        
        data <- extra_data_for_card()
              
        # b.check <<- b
        # select_data.check <<- data
        # phy_inactive_change.check <<- phy_inactive_change
        
        final_value <- round(data$years_of_potential_life_lost_rate - (b * phy_inactive_change), 1)
        
        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
        
        infoBox(
          HTML(paste(" Years of Potential", br(), "Life Lost Rate")),
          paste0(final_value, " (", round(b * phy_inactive_change, 1), ")"),
          fill = TRUE,
          color = "fuchsia"
        )
      
    })
    
    output$play_impact_card_2 <- renderInfoBox({
      
        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
        
        model <- lm(avg_no_of_physically_unhealthy_days ~ score, data = data) %>%
        tidy()
        
        b <- model$estimate[2]
        
        data <- extra_data_for_card()
        
        final_value <- round(data$avg_no_of_physically_unhealthy_days - (b * phy_inactive_change), 2)
        
        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
        
        infoBox(
          HTML(paste("Average # of", br(), "Physically Unhealthy Days")),
          paste0(final_value, " (", round(b * phy_inactive_change, 2), ")"),
          fill = TRUE,
          color = "olive"
        )
      
    })
    
    output$play_impact_card_3 <- renderInfoBox({
      
        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
        
        model <- lm(avg_no_of_mentally_unhealthy_days ~ score, data = data) %>%
        tidy()
        
        b <- model$estimate[2]
        
        data <- extra_data_for_card()
        
        final_value <- round(data$avg_no_of_mentally_unhealthy_days - (b * phy_inactive_change), 2)
        
        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
        
        infoBox(
          HTML(paste("Average # of", br(), "Mentally Unhealthy Days")),
          paste0(final_value, " (", round(b * phy_inactive_change, 2), ")"),
          fill = TRUE,
          color = "olive"
        )
      
    })
    
    output$play_impact_card_4 <- renderInfoBox({

        # Physical inactive change
        phy_inactive_change <- phy_inactive_change()
        # Get impact model b
        data <- z_geo_w_criterion_df()
        
        model <- lm(age_adjusted_death_rate ~ score, data = data) %>%
        tidy()

        b <- model$estimate[2]

        data <- extra_data_for_card()
        
        final_value <- round(data$age_adjusted_death_rate - (b * phy_inactive_change), 1)

        # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))

        infoBox(
          HTML(paste("Premature Age Adjusted", br(), "Mortality")),
          paste0(final_value, " (", round(b * phy_inactive_change, 2), ")"),
          fill = TRUE,
          color = "fuchsia"
        )

    })

    output$play_impact_card_5 <- renderInfoBox({
      
      # Physical inactive change
      phy_inactive_change <- phy_inactive_change()
      # Get impact model b
      # data <- z_geo_w_criterion_df()
      # 
      # model <- lm(age_adjusted_death_rate ~ score, data = data) %>%
      #   tidy()
      # 
      # b <- model$estimate[2]
      
      # Hard code for now
      b <- -43.3992
      
      data <- extra_data_for_card()
      
      final_value <- round(0- (b * phy_inactive_change), 1)
      
      # final_value <- round(data$age_adjusted_death_rate - (b * phy_inactive_change), 1)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Est' Change Employer Total", br(), "Spend per Member")),
        paste0("$", round(final_value, 1)),
        fill = TRUE,
        color = "olive"
      )
      
    })
    
    
    # New cards (4):  
    # Years of Potential Life Lost Rate, 
    # Average Number of Physically Unhealthy Days, 
    # Average Number of Mentally Unhealthy Days, 
    # Premature age-adjusted mortality
    
    output$rest_impact_card_1 <- renderInfoBox({
      
      # Insufficient Sleep change
      insufficient_sleep_change <- insufficient_sleep_change()
      # Get impact model b
      data <- rest_z_geo_w_criterion_df()
      
      # value <- cor(data$score, data$years_of_potential_life_lost_rate, 
      #     method = "pearson", use = "complete.obs")
      
      model <- lm(years_of_potential_life_lost_rate ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      # b.check <<- b
      # select_data.check <<- data
      # phy_inactive_change.check <<- phy_inactive_change
      
      final_value <- round(data$years_of_potential_life_lost_rate - (b * insufficient_sleep_change), 1)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste(" Years of Potential", br(), "Life Lost Rate")),
        paste0(final_value, " (", round(b * insufficient_sleep_change, 1), ")"),
        fill = TRUE,
        color = "fuchsia"
      )
      
    })
    
    output$rest_impact_card_2 <- renderInfoBox({
      
      # Insufficient Sleep change
      insufficient_sleep_change <- insufficient_sleep_change()
      # Get impact model b
      data <- rest_z_geo_w_criterion_df()
      
      model <- lm(avg_no_of_physically_unhealthy_days ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      final_value <- round(data$avg_no_of_physically_unhealthy_days - (b * insufficient_sleep_change), 2)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Average # of", br(), "Physically Unhealthy Days")),
        paste0(final_value, " (", round(b * insufficient_sleep_change, 2), ")"),
        fill = TRUE,
        color = "olive"
      )
      
    })
    
    output$rest_impact_card_3 <- renderInfoBox({
      
      # Insufficient Sleep change
      insufficient_sleep_change <- insufficient_sleep_change()
      # Get impact model b
      data <- rest_z_geo_w_criterion_df()
      
      model <- lm(avg_no_of_mentally_unhealthy_days ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      final_value <- round(data$avg_no_of_mentally_unhealthy_days - (b * insufficient_sleep_change), 2)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Average # of", br(), "Mentally Unhealthy Days")),
        paste0(final_value, " (", round(b * insufficient_sleep_change, 2), ")"),
        fill = TRUE,
        color = "olive"
      )
      
    })
    
    output$rest_impact_card_4 <- renderInfoBox({
      
      # Insufficient Sleep change
      insufficient_sleep_change <- insufficient_sleep_change()
      # Get impact model b
      data <- rest_z_geo_w_criterion_df()
      
      # add age_adjusted_death_rate
      data <- left_join(data, dplyr::select(ana_data_criterion, age_adjusted_death_rate, fips),
                        by = "fips")
      
      model <- lm(age_adjusted_death_rate ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      final_value <- round(data$age_adjusted_death_rate - (b * insufficient_sleep_change), 1)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Premature Age Adjusted", br(), "Mortality")),
        paste0(final_value, " (", round(b * insufficient_sleep_change, 2), ")"),
        fill = TRUE,
        color = "fuchsia"
      )
      
    })
    
    
    output$rest_impact_card_5 <- renderInfoBox({
      
      # Insufficient Sleep change
      insufficient_sleep_change <-  insufficient_sleep_change()
      # Get impact model b
      # data <- z_geo_w_criterion_df()
      # 
      # model <- lm(age_adjusted_death_rate ~ score, data = data) %>%
      #   tidy()
      # 
      # b <- model$estimate[2]
      
      # Hard code for now
      b <- -64.94864 
      
      data <- extra_data_for_card()
      
      final_value <- round(0 - (b * insufficient_sleep_change), 1)
      
      # final_value <- round(data$age_adjusted_death_rate - (b * phy_inactive_change), 1)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Est' Change Employer Total", br(), "Spend per Member")),
        paste0("$", round(final_value, 1)),
        fill = TRUE,
        color = "olive"
      )
      
    })

    
    # New cards (4):  
    # Years of Potential Life Lost Rate, 
    # Average Number of Physically Unhealthy Days, 
    # Average Number of Mentally Unhealthy Days, 
    # Premature age-adjusted mortality
    
    output$work_impact_card_1 <- renderInfoBox({
      
      # Per with Disability change
      per_w_disability_change <-  per_w_disability_change()
      # Get impact model b
      data <- work_z_geo_w_criterion_df()
      
      # value <- cor(data$score, data$years_of_potential_life_lost_rate, 
      #     method = "pearson", use = "complete.obs")
      
      model <- lm(years_of_potential_life_lost_rate ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      # b.check <<- b
      # select_data.check <<- data
      # phy_inactive_change.check <<- phy_inactive_change
      
      final_value <- round(data$years_of_potential_life_lost_rate - (b * per_w_disability_change), 1)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste(" Years of Potential", br(), "Life Lost Rate")),
        paste0(final_value, " (", round(b * per_w_disability_change, 1), ")"),
        fill = TRUE,
        color = "fuchsia"
      )
      
    })
    
    output$work_impact_card_2 <- renderInfoBox({
      
      # Per with Disability change
      per_w_disability_change <-  per_w_disability_change()
      # Get impact model b
      data <- work_z_geo_w_criterion_df()
      
      model <- lm(avg_no_of_physically_unhealthy_days ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      final_value <- round(data$avg_no_of_physically_unhealthy_days - (b * per_w_disability_change), 2)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Average # of", br(), "Physically Unhealthy Days")),
        paste0(final_value, " (", round(b * per_w_disability_change, 2), ")"),
        fill = TRUE,
        color = "olive"
      )
      
    })
    
    output$work_impact_card_3 <- renderInfoBox({
      
      # Per with Disability change
      per_w_disability_change <-  per_w_disability_change()
      # Get impact model b
      data <- work_z_geo_w_criterion_df()
      
      model <- lm(avg_no_of_mentally_unhealthy_days ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      final_value <- round(data$avg_no_of_mentally_unhealthy_days - (b * per_w_disability_change), 2)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Average # of", br(), "Mentally Unhealthy Days")),
        paste0(final_value, " (", round(b * per_w_disability_change, 2), ")"),
        fill = TRUE,
        color = "olive"
      )
      
    })
    
    output$work_impact_card_4 <- renderInfoBox({
      
      # Per with Disability change
      per_w_disability_change <-  per_w_disability_change()
      # Get impact model b
      data <- work_z_geo_w_criterion_df()
      
      # # add age_adjusted_death_rate
      data <- left_join(data, dplyr::select(ana_data_criterion, age_adjusted_death_rate, fips),
                        by = "fips")
      
      model <- lm(age_adjusted_death_rate ~ score, data = data) %>%
        tidy()
      
      b <- model$estimate[2]
      
      data <- extra_data_for_card()
      
      final_value <- round(data$age_adjusted_death_rate - (b * per_w_disability_change), 1)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Premature Age Adjusted", br(), "Mortality")),
        paste0(final_value, " (", round(b * per_w_disability_change, 2), ")"),
        fill = TRUE,
        color = "fuchsia"
      )
      
    })
    
    output$work_impact_card_5 <- renderInfoBox({
      
      # per_w_disability change
      per_w_disability_change <-  per_w_disability_change()
      # Get impact model b
      # data <- z_geo_w_criterion_df()
      # 
      # model <- lm(age_adjusted_death_rate ~ score, data = data) %>%
      #   tidy()
      # 
      # b <- model$estimate[2]
      
      # Hard code for now
      b <- -46.56329  
      
      data <- extra_data_for_card()
      
      final_value <- round(0- (b * per_w_disability_change), 1)
      
      # final_value <- round(data$age_adjusted_death_rate - (b * phy_inactive_change), 1)
      
      # print(paste0(final_value, ": ", b, ": ", phy_inactive_change))
      
      infoBox(
        HTML(paste("Est' Change Employer Total", br(), "Spend per Member")),
        paste0("$", round(final_value, 1)),
        fill = TRUE,
        color = "olive"
      )
      
    })
    
    # --- Extra Impact Card ---
    # Optional for some Clients
     
    output$play_extra_impact_card <- renderInfoBox({
      
      # annual_avg_emplvl * 166 (from literature per capita cost)
      
        # data <- play_select_geo_df()
        
        data <- extra_data_for_card()
        
        # Join with labour data
        # data <- data %>%
        #   left_join(data_labour_1, by = c("fips" = "FIPS"))
        
        # Pop change impact repeat here
        # TODO: move to reactive
        
        if (input$region == "--") {
          value <-  play_select_geo_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        } else {
          value <-  play_select_geo_region_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
          value <- sum(value)
        }
        
         # slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        # b <- filter(slider_data, var_name == input$iv) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b_top5 <- filter(slider_data, var_name == input$iv_top5) %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # 
        # # b1 <- filter(slider_data, var_name == "per_adults_with_obesity") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b2 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b3 <- filter(slider_data, var_name == "per_with_access_to_exercise_oppurtunities") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b4 <- filter(slider_data, var_name == "per_excessive_drinking") %>%
        # #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # # b5 <- filter(slider_data, var_name == "per_insufficient_sleep") %>%
        # #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # 
        # 
        # if (input$iv_top5 == "per_fair_or_poor_health" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b1 <- filter(slider_data, var_name == "per_fair_or_poor_health") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b1 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        # } else if (input$iv_top5 == "per_w_grad_or_prof_degree" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b2 <- filter(slider_data, var_name == "per_w_grad_or_prof_degree") %>%
        #   
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b2 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        # } else if (input$iv_top5 == "per_adults_with_diabetes" & input$iv != "Select a Measure") {
        #   
        # # Set The second layer mediator b
        #  b3 <- filter(slider_data, var_name == "per_adults_with_diabetes") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        #  
        #   bm <- filter(slider_med_data, var_name == input$iv) %>% 
        #          dplyr::select(b) %>% unlist() %>% as.numeric()
        #   b <- b3 * bm
        #   # print(paste0("Mod Route(", input$iv, "): ", b, " = ", bm, " * ", b2))
        #  
        # } else {
        #   print(paste("Normal Route: ", b))
        # }
        
        # Get real_b from slider
      play_slider_change_b.check <<- play_slider_change_b()
      real_b <- play_slider_change_b()
        
        value2_0 <- round(value * ((real_b * input$change)/100), 0)
        value2_top5 <- round(value * ((real_b * input$change_top5)/100), 0)
        
        # value2_0 <- round(value * ((b * input$change)/100), 0)
        # value2_top5 <- round(value * ((b_top5 * input$change_top5)/100), 0)
        # value2_1 <- round(value * ((b1 * input$change1)/100), 0)
        # value2_2 <- round(value * ((b2 * input$change2)/100), 0)
        # value2_3 <- round(value * ((b3 * input$change3)/100), 0)
        # value2_4 <- round(value * ((b4 * input$change4)/100), 0)
        # value2_5 <- round(value * ((b5 * input$change5)/100), 0)
        
        value2 <- sum(value2_0, value2_top5, na.rm = T)
        
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
        
        # print(data$annual_avg_emplvl / value)
         
        # estimate % of employee impacted based on proportion of employed population
        pop_change_value <- (data$annual_avg_emplvl / value) * value2
        
        final_value <- (data$annual_avg_emplvl + pop_change_value ) * 166
        
        final_value <- paste0("$", formatC(round(final_value, 1), format="f", big.mark=",", digits = 1),
                              
                              " ($", formatC(round(pop_change_value, 1), format="f", big.mark=",", digits = 0),
                              ")")
        
        infoBox(
          HTML(paste("Physical Inactivity", br(), " Absenteeism Cost to Employer")),
          final_value, 
          fill = TRUE,
          color = "olive"
        )
      
      
    })
    
    
    output$rest_extra_impact_card <- renderInfoBox({
      
      # Companies lose an estimated $2,280 per employee each year due to sleep deprivation
      
        data <- rest_select_geo_df()
        
        # Join with labour data
        data <- data %>%
          left_join(data_labour_1, by = c("fips" = "FIPS"))
        
        # Pop change impact repeat here
        # TODO: move to reactive
        value <-  rest_select_geo_df() %>%
            dplyr::select(population) %>%
            unlist() %>%
            as.numeric()
        
         # slider data
        slider_data <- rest_slider_data()
        
        b <- filter(slider_data, var_name == input$rest_iv) %>%
          dplyr::select(b) %>% unlist() %>% as.numeric()
        b_top5 <- filter(slider_data, var_name == input$rest_iv_top5) %>%
          dplyr::select(b) %>% unlist() %>% as.numeric()
        # b1 <- filter(slider_data, var_name == "avg_no_of_mentally_unhealthy_days") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b2 <- filter(slider_data, var_name == "per_smokers") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b3 <- filter(slider_data, var_name == "per_black") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        # b4 <- filter(slider_data, var_name == "average_daily_pm2_5") %>%
        #   dplyr::select(b) %>% unlist() %>% as.numeric()
        # b5 <- filter(slider_data, var_name == "per_non_hispanic_white") %>%
        #  dplyr::select(b) %>% unlist() %>% as.numeric()
        
        
        value2_0 <- round(value * ((b * input$rest_change)/100), 0)
        value2_top5 <- round(value * ((b_top5 * input$rest_change_top5)/100), 0)
        # value2_1 <- abs(round(value * ((b1 * input$rest_change1)/100), 0))
        # value2_2 <- abs(round(value * ((b2 * input$rest_change2)/100), 0))
        # value2_3 <- abs(round(value * ((b3 * input$rest_change3)/100), 0))
        # value2_4 <- abs(round(value * ((b4 * input$rest_change4)/100), 0))
        # value2_5 <- abs(round(value * ((b5 * input$rest_change5)/100), 0))
        
        value2 <- sum(value2_0, value2_top5, na.rm = T)
        # value2 <- sum(value2_0, value2_top5, value2_1, value2_2, value2_3, value2_4, value2_5, na.rm = T)
        
        # print(data$annual_avg_emplvl / value)
        
        # estimate % of employee impacted based on proportion of employed population
        pop_change_value <- (data$annual_avg_emplvl / value) * value2
        
        final_value <- (data$annual_avg_emplvl - pop_change_value ) * 2280 
        
        final_value <- paste0("$", formatC(round(final_value, 1), format="f", big.mark=",", digits = 1),
                              " ($", formatC(round(pop_change_value, 1), format="f", big.mark=",", digits = 0), ")")
        
        infoBox(
          HTML(paste("Insufficient Sleep", br(), "Cost")),
          final_value, 
          fill = TRUE,
          color = "olive"
        )
      
      
    })

        
    
    # --- Chart and Plot ----
     
    # Play Index Page Plots
     z_geo_w_criterion_df <- reactive({
      
       data <- play_z_geo_df()
       
       # Join with criterion variable
       data <- left_join(data, ana_data_criterion, by = c("fips", "state", "county"))
       
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
      
     value <- cor(data$score, data$avg_no_of_physically_unhealthy_days, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_physically_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
      labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_3 <- renderPlot({
      
     data <- z_geo_w_criterion_df()
    
     value <- cor(data$score, data$avg_no_of_mentally_unhealthy_days, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_mentally_unhealthy_days)) +
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
    
    # Play index correlation with additional measure
    # % Uninsured (rank), 
    # Primary Care Physicians Rate (rank),
    # % Unemployed (rank), 
    # 20th Percentile Income (rank), 
    # % Single-Parent Households (rank), 
    # Severe Housing Cost Burden (rank), 
    # Violent Crime Rate (rank), and 
    # Social Association Rate (rank).  Please post the result to the 'Play Index' view
    
    output$test_criterion_plot_a1 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_uninsured, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_uninsured)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a2 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$primary_care_physicians_ratio, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, primary_care_physicians_ratio)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a3 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_unemployed, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_unemployed)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a4 <- renderPlot({
          
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$x20th_perile_income, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, x20th_perile_income)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a5 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_single_parent_households, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_single_parent_households)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a6 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$severe_housing_cost_burden, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, severe_housing_cost_burden)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a7 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$violent_crime_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, violent_crime_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a8 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$social_association_rate, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, social_association_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a9 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
     value <- cor(data$score, data$per_adults_with_diabetes, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_adults_with_diabetes)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a10 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
     value <- cor(data$score, data$medicare_spending_age_sex_race_adjusted_4, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_age_sex_race_adjusted_4)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a11 <- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
     value <- cor(data$score, data$medicare_spending_price_age_sex_race_adjusted_5, 
            method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_price_age_sex_race_adjusted_5)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a12<- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
      data <- left_join(data, income_chg_1, by = "fips")
      
      value <- cor(data$score, data$income_chg_at_26, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, income_chg_at_26)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$test_criterion_plot_a13<- renderPlot({
      
      data <- z_geo_w_criterion_df()
      
      data <- left_join(data, social_cx_index_1, by = "fips")
      
      value <- cor(data$score, data$social_cx_index, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, social_cx_index)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    # Rest Index Page Plots
    rest_z_geo_w_criterion_df <- reactive({
      
      data <- rest_z_geo_df()
      
      # keep only scores and fips
      data <- data %>% dplyr::select(fips, score)
      
      # Join with criterion variable
      data <- left_join(data, criterion_ana, by = c("fips"))
      
      return(data)
      
    })
    
    
    # Simple between the Play index and the four criterion
    
    output$rest_test_criterion_plot_1 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$years_of_potential_life_lost_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, years_of_potential_life_lost_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_2 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$avg_no_of_physically_unhealthy_days, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_physically_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_3 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$avg_no_of_mentally_unhealthy_days, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_mentally_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_4 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$preventable_hospitalization_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, preventable_hospitalization_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    # Play index correlation with additional measure
    # % Uninsured (rank), 
    # Primary Care Physicians Rate (rank),
    # % Unemployed (rank), 
    # 20th Percentile Income (rank), 
    # % Single-Parent Households (rank), 
    # Severe Housing Cost Burden (rank), 
    # Violent Crime Rate (rank), and 
    # Social Association Rate (rank).  Please post the result to the 'Play Index' view
    
    output$rest_test_criterion_plot_a1 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_uninsured, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_uninsured)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a2 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$primary_care_physicians_ratio, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, primary_care_physicians_ratio)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a3 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_unemployed, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_unemployed)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a4 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$x20th_perile_income, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, x20th_perile_income)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a5 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_single_parent_households, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_single_parent_households)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a6 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$severe_housing_cost_burden, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, severe_housing_cost_burden)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a7 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$violent_crime_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, violent_crime_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a8 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$social_association_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, social_association_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a9 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_adults_with_diabetes, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_adults_with_diabetes)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a10 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
      value <- cor(data$score, data$medicare_spending_age_sex_race_adjusted_4, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_age_sex_race_adjusted_4)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a11 <- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
      value <- cor(data$score, data$medicare_spending_price_age_sex_race_adjusted_5, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_price_age_sex_race_adjusted_5)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a12<- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      data <- left_join(data, income_chg_1, by = "fips")
      
      value <- cor(data$score, data$income_chg_at_26, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, income_chg_at_26)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$rest_test_criterion_plot_a13<- renderPlot({
      
      data <- rest_z_geo_w_criterion_df()
      
      data <- left_join(data, social_cx_index_1, by = "fips")
      
      value <- cor(data$score, data$social_cx_index, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, social_cx_index)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    
    # Work Index Page Plots
    work_z_geo_w_criterion_df <- reactive({
      
      data <- work_z_geo_df()
      
      # keep only scores and fips
      data <- data %>% dplyr::select(fips, score)
      
      # Join with criterion variable
      data <- left_join(data, criterion_ana, by = c("fips"))
      
      work_data.check <<- data
      
      return(data)
      
    })
    
    
    # Simple between the Play index and the four criterion
    
    output$work_test_criterion_plot_1 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$years_of_potential_life_lost_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, years_of_potential_life_lost_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_2 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$avg_no_of_physically_unhealthy_days, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_physically_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_3 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$avg_no_of_mentally_unhealthy_days, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, avg_no_of_mentally_unhealthy_days)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_4 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$preventable_hospitalization_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, preventable_hospitalization_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    # Play index correlation with additional measure
    # % Uninsured (rank), 
    # Primary Care Physicians Rate (rank),
    # % Unemployed (rank), 
    # 20th Percentile Income (rank), 
    # % Single-Parent Households (rank), 
    # Severe Housing Cost Burden (rank), 
    # Violent Crime Rate (rank), and 
    # Social Association Rate (rank).  Please post the result to the 'Play Index' view
    
    output$work_test_criterion_plot_a1 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_uninsured, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_uninsured)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a2 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$primary_care_physicians_ratio, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, primary_care_physicians_ratio)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a3 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_unemployed, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_unemployed)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a4 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$x20th_perile_income, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, x20th_perile_income)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a5 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_single_parent_households, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_single_parent_households)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a6 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$severe_housing_cost_burden, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, severe_housing_cost_burden)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a7 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$violent_crime_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, violent_crime_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a8 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$social_association_rate, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, social_association_rate)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a9 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      value <- cor(data$score, data$per_adults_with_diabetes, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, per_adults_with_diabetes)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a10 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
      value <- cor(data$score, data$medicare_spending_age_sex_race_adjusted_4, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_age_sex_race_adjusted_4)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a11 <- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      data <- left_join(data, data_medicare_1, by = "fips")
      
      value <- cor(data$score, data$medicare_spending_price_age_sex_race_adjusted_5, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, medicare_spending_price_age_sex_race_adjusted_5)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a12<- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      data <- left_join(data, income_chg_1, by = "fips")
      
      value <- cor(data$score, data$income_chg_at_26, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, income_chg_at_26)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    output$work_test_criterion_plot_a13<- renderPlot({
      
      data <- work_z_geo_w_criterion_df()
      
      data <- left_join(data, social_cx_index_1, by = "fips")
      
      value <- cor(data$score, data$social_cx_index, 
                   method = "pearson", use = "complete.obs")
      
      ggplot(data, aes(score, social_cx_index)) +
        geom_point() +
        geom_smooth() +
        labs(title = paste0("Correlation r = ", round(value, 2))) +
        theme_minimal()
      
    })
    
    # Play index scatter grid plot
    output$play_grid_plot <- renderPlot({
      
        # Get data
        data <- plot_data()
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$region != "--") {
          data <- data %>% mutate(
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus))
        }
        

        # Get weight aka slider data
        slider_data <- play_slider_data()
        slider_med_data <- play_slider_med_data()
        
        if (input$region != "--") {
          slider_data <- bind_rows(slider_data, slider_med_data)
        }
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        
        # Make focus generic
       #  if (input$iv_top5 != "Select a Measure"  & input$rest_iv == "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "top5_iv" = input$iv_top5)  %>%
       #    mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$change_top5, top5_iv))
       #  }
       #  
       #  
       # if (input$iv_top5 %in% measure_top3_lst_play & input$iv != "Select a Measure") {
       #  data <- data %>%
       #      rename(
       #        "focus_iv" = input$iv_top5) %>%
       #    mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
       #  }
        
        # 
        # if (input$iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$change, focus_iv))
        # }
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xchange <- input$change_top5
        } else {
          xchange <- input$change
        }
        
      # Get real_b from slider
      real_b <- play_slider_change_b()
      
      
        # Modify the data
        data <- mutate(data,
          label = paste0(county),
          label = ifelse(focus == 1, label, NA),
          per_physically_inactive = ifelse(focus == 1, (per_physically_inactive + (real_b * xchange)), per_physically_inactive))
        

        
        if (input$region != "--") {
          
          data <- mutate(data,
              focusOrg = focus,
              focus = ifelse(Region == input$region, 1, focus),
              focus = ifelse(is.na(focus), focusOrg, focus),
                 label = ifelse(focus == 1, label, NA))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$region) %>%
            mutate(label = paste0(Region),
                   label = ifelse(focus == 1, label, NA))
          
          
        } else {
           data <- data
        }
        
        
        # Change x axis
        if(input$iv_top5 != "Select a Measure" & input$iv == "Select a Measure") {
          xlabel <- input$iv_top5
        } else {
          xlabel <- input$iv
        }
        
        # Make plot
        if (input$region == "--") {
          ggplot(data, 
            aes(per_physically_inactive, score)) +
            geom_point(aes(fill = factor(quintile),
                            color = factor(focus)), size = 5, shape = 21) +
            geom_label_repel(aes(label = label),
                       color = "black") +
            labs(y = "Play Index (0 to 100)", x = " % of Population Physically Inactive") +
            theme_minimal() +
            scale_fill_manual(values = quintile_colour_pal) +
            scale_colour_manual(values = c("#ffffff00", "black")) +
            theme(legend.position = "none")   
        } else {
          ggplot(data, 
            aes(per_physically_inactive, score)) +
            geom_point(aes(fill = factor(quintile),
                           color = factor(focus)), size = 5, shape = 21) +
            geom_point(data = region_data, size = 7.5, color = "#6a51a3") +
            geom_label_repel(aes(label = label),
                       color = "darkgrey", size = 4.5, box.padding = .12, label.padding = .12, label.size = 0.2) +
            geom_label_repel(data = region_data, aes(label = label),
                       color = "black", size = 7) +
            labs(y = "Play Index (0 to 100)", x = " % of Population Physically Inactive") +
            theme_minimal() +
            scale_fill_manual(values = quintile_colour_pal) +
            scale_colour_manual(values = c("#ffffff00", "black")) +
            theme(legend.position = "none")     
        }

        
    })
    
    # Rest index scatter grid plot
    output$rest_grid_plot <- renderPlot({
      
        # Get data
        data <- rest_plot_data()
        
        
        data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
        
        if (input$rest_region != "--") {
          data <- data %>% mutate(
            focusOrg = focus,
            focus = ifelse(Region == input$rest_region, 1, focus),
            focus = ifelse(is.na(focus), focusOrg, focus))
        }
        
        
        # Get weight aka slider data
        slider_data <- rest_slider_data()
        slider_med_data <- rest_slider_med_data()
        
        if (input$rest_region != "--") {
          slider_data <- bind_rows(slider_data, slider_med_data)
        }
        
        if (nrow(slider_data) == 0) {
          slider_data <- data.frame(b = 0)
        }
        
        if (nrow(slider_med_data) == 0) {
          slider_med_data <- data.frame(b = 0)
        }
        
        
        # Make focus generic
        # if (input$rest_iv_top5 != "Select a Measure") {
        # data <- data %>%
        #     rename(
        #       "top5_iv" = input$rest_iv_top5)  %>%
        #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$rest_change_top5, top5_iv))
        # }
        # 
        # if (input$rest_iv != "Select a Measure") {
        #   
        # data <- data %>%
        #     rename(
        #       "focus_iv" = input$rest_iv) %>%
        #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$rest_change, focus_iv))
        # }
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xchange <- input$rest_change_top5
        } else {
          xchange <- input$rest_change
        }
        
        # Get real_b from slider
        real_b <- rest_slider_change_b()
        
        
        # Modify the data
        data <- mutate(data,
          label = paste0(county),
          label = ifelse(focus == 1, label, NA),
          per_insufficient_sleep = ifelse(focus == 1, (per_insufficient_sleep + (real_b * xchange)), per_insufficient_sleep))
        
        
        if (input$rest_region != "--") {
          
          data <- mutate(data,
                         focusOrg = focus,
                         focus = ifelse(Region == input$rest_region, 1, focus),
                         focus = ifelse(is.na(focus), focusOrg, focus),
                         label = ifelse(focus == 1, label, NA))
          
          region_data <- data %>% 
            group_by(Region) %>% 
            summarise(across(where(is.numeric), mean, na.rm = T)) %>%
            filter(Region == input$rest_region) %>%
            mutate(label = paste0(Region),
                   label = ifelse(focus == 1, label, NA))
          
          
        } else {
          data <- data
        }
        
        
        
        # Change x axis
        if(input$rest_iv_top5 != "Select a Measure" & input$rest_iv == "Select a Measure") {
          xlabel <- input$rest_iv_top5
        } else {
          xlabel <- input$rest_iv
        }
        
        # Make plot
        if (input$rest_region == "--") {
          
          ggplot(data, 
                 aes(per_insufficient_sleep, score)) +
            geom_point(aes(fill = factor(quintile),
                           color = factor(focus)), size = 5, shape = 21) +
            geom_label_repel(aes(label = label),
                             color = "black") +
            labs(y = "Rest Index (0 to 100)", x = "% of Population with Insufficient Sleep") +
            theme_minimal() +
            scale_fill_manual(values = quintile_colour_pal) +
            scale_colour_manual(values = c("#ffffff00", "black")) +
            theme(legend.position = "none")
          
        } else {
          
          ggplot(data, 
            aes(per_insufficient_sleep, score)) +
            geom_point(aes(fill = factor(quintile),
                     color = factor(focus)), size = 5, shape = 21) +
            geom_point(data = region_data, size = 7.5, color = "#6a51a3") +
            geom_label_repel(aes(label = label),
                             color = "darkgrey", size = 4.5, box.padding = .12, label.padding = .12, label.size = 0.2) +
            geom_label_repel(data = region_data, aes(label = label),
                             color = "black", size = 7) +
            labs(y = "Rest Index (0 to 100)", x = "% of Population with Insufficient Sleep") +
            theme_minimal() +
            scale_fill_manual(values = quintile_colour_pal) +
            scale_colour_manual(values = c("#ffffff00", "black")) +
            theme(legend.position = "none")     
        }
        

        
    })
    
    
    # Work index scatter grid plot
    output$work_grid_plot <- renderPlot({
      
      # Get data
      data <- work_plot_data()
      
      
      data <- left_join(data, region_lkup, by = c("fips" = "FIPS"))
      
      if (input$work_region != "--") {
        data <- data %>% mutate(
          focusOrg = focus,
          focus = ifelse(Region == input$work_region, 1, focus),
          focus = ifelse(is.na(focus), focusOrg, focus))
      }
      
      
      # Get weight aka slider data
      slider_data <- work_slider_data()
      slider_med_data <- work_slider_med_data()
      
      if (input$work_region != "--") {
        slider_data <- bind_rows(slider_data, slider_med_data)
      }
      
      if (nrow(slider_data) == 0) {
        slider_data <- data.frame(b = 0)
      }
      
      if (nrow(slider_med_data) == 0) {
        slider_med_data <- data.frame(b = 0)
      }
      
      
      # Make focus generic
      # if (input$work_iv_top5 != "Select a Measure") {
      # data <- data %>%
      #     rename(
      #       "top5_iv" = input$work_iv_top5)  %>%
      #   mutate(top5_iv =  ifelse(focus == 1, top5_iv + input$work_change_top5, top5_iv))
      # }
      # 
      # if (input$work_iv != "Select a Measure") {
      #   
      # data <- data %>%
      #     rename(
      #       "focus_iv" = input$work_iv) %>%
      #   mutate(focus_iv = ifelse(focus == 1, focus_iv + input$work_change, focus_iv))
      # }
      
      # Change x axis
      if(input$work_iv_top5 != "Select a Measure" & input$work_iv == "Select a Measure") {
        xchange <- input$work_change_top5
      } else {
        xchange <- input$work_change
      }
      
      # Get real_b from slider
      real_b <- work_slider_change_b()
      
      
      # Modify the data
      data <- mutate(data,
                     label = paste0(county),
                     label = ifelse(focus == 1, label, NA),
                     per_w_a_disability = ifelse(focus == 1, (per_w_a_disability + (real_b * xchange)), per_w_a_disability))
      
      
      if (input$work_region != "--") {
        
        data <- mutate(data,
                       focusOrg = focus,
                       focus = ifelse(Region == input$work_region, 1, focus),
                       focus = ifelse(is.na(focus), focusOrg, focus),
                       label = ifelse(focus == 1, label, NA))
        
        region_data <- data %>% 
          group_by(Region) %>% 
          summarise(across(where(is.numeric), mean, na.rm = T)) %>%
          filter(Region == input$work_region) %>%
          mutate(label = paste0(Region),
                 label = ifelse(focus == 1, label, NA))
        
        
      } else {
        data <- data
      }
      
      
      
      # Change x axis
      if(input$work_iv_top5 != "Select a Measure" & input$work_iv == "Select a Measure") {
        xlabel <- input$work_iv_top5
      } else {
        xlabel <- input$work_iv
      }
      
      # Make plot
      # Old version, just keep as copy
      # if (input$work_region == "--") {
      #   
      #   ggplot(data, 
      #          aes(per_w_a_disability, score, color = focus)) +
      #     geom_point(size = 5) +
      #     geom_label_repel(aes(label = label),
      #                      color = "black") +
      #     labs(y = "Work Index (0 to 100)", x = "% with a Disability") +
      #     theme_minimal() +
      #     theme(legend.position = "none")
      #   
      # } else {
      #   
      #   ggplot(data, 
      #          aes(per_w_a_disability, score, color = focus)) +
      #     geom_point(size = 5) +
      #     geom_point(data = region_data, size = 7.5, color = "#6a51a3") +
      #     geom_label_repel(aes(label = label),
      #                      color = "darkgrey", size = 4.5, box.padding = .12, label.padding = .12, label.size = 0.2) +
      #     geom_label_repel(data = region_data, aes(label = label),
      #                      color = "black", size = 7) +
      #     labs(y = "Work Index (0 to 100)", x = "% with a Disability") +
      #     theme_minimal() +
      #     theme(legend.position = "none")     
      # }

      # region.data.work.check <<- region_data
      # data.work.check <<- data
      
      # Make plot
      if (input$work_region == "--") {
        
        ggplot(data, 
               aes(per_w_a_disability, score, color = focus)) +
          geom_point(aes(fill = factor(quintile),
                         color = factor(focus)), size = 5, shape = 21) +
          geom_label_repel(aes(label = label),
                           color = "black") +
          labs(y = "Work Index (0 to 100)", x = "% of population with a cognitive or physical impairment") +
          theme_minimal() +
          scale_fill_manual(values = quintile_colour_pal) +
          scale_colour_manual(values = c("#ffffff00", "black")) +
          theme(legend.position = "none")
        
      } else {
        
        ggplot(data, 
               aes(per_w_a_disability, score, color = focus)) +
          geom_point(aes(fill = factor(quintile),
                         color = factor(focus)), size = 5, shape = 21) +
          geom_point(data = region_data, size = 7.5, color = "#6a51a3") +
          geom_label_repel(aes(label = label),
                           color = "darkgrey", size = 4.5, box.padding = .12, label.padding = .12, label.size = 0.2) +
          geom_label_repel(data = region_data, aes(label = label),
                           color = "black", size = 7) +
          labs(y = "Work Index (0 to 100)", x = "% of population with a cognitive or physical impairment") +
          theme_minimal() +
          scale_fill_manual(values = quintile_colour_pal) +
          scale_colour_manual(values = c("#ffffff00", "black")) +
          theme(legend.position = "none")     
      }
      
      
      
    })
    
    # --- Test Table ---
    
    output$play_model_table <- DT::renderDataTable({
       datatable(m_step_df_play,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_play)[-1], digits = 2)
    })
    
    output$fp_health_model_table <- DT::renderDataTable({
       datatable(m_step_df_fp_health,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_fp_health)[-1], digits = 2)
    })
    
    output$grad_model_table <- DT::renderDataTable({
       datatable(m_step_df_grad,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_grad)[-1], digits = 2)
    })
    
    output$grad_model_table_work <- DT::renderDataTable({
      datatable(m_step_df_grad,
                options = list(
                  order = list(3, 'desc'),
                  paging =TRUE,
                  pageLength = 50
                )         
      ) %>%
        formatRound(columns = names(m_step_df_grad)[-1], digits = 2)
    })
    
    output$diabetes_model_table <- DT::renderDataTable({
       datatable(m_step_df_diabetes,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_diabetes)[-1], digits = 2)
    })
    
    output$rest_model_table <- DT::renderDataTable({
       datatable(m_step_df_rest,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_rest)[-1], digits = 2)
    })
    
    output$obesity_model_table <- DT::renderDataTable({
       datatable(m_step_df_obesity,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_obesity)[-1], digits = 2)
    })
    
    output$avg_m_days_model_table <- DT::renderDataTable({
       datatable(m_step_df_avg_m_days,
         options = list(
             order = list(3, 'desc'),
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_avg_m_days)[-1], digits = 2)
    })
    
    output$avg_m_days_model_table_work <- DT::renderDataTable({
      datatable(m_step_df_avg_m_days,
                options = list(
                  order = list(3, 'desc'),
                  paging =TRUE,
                  pageLength = 50
                )         
      ) %>%
        formatRound(columns = names(m_step_df_avg_m_days)[-1], digits = 2)
    })
    
    output$phy_inactive_model_table <- DT::renderDataTable({
      datatable(m_step_df_phy_inactive,
                options = list(
                  order = list(3, 'desc'),
                  paging =TRUE,
                  pageLength = 50
                )         
      ) %>%
        formatRound(columns = names(m_step_df_phy_inactive)[-1], digits = 2)
    })
    
    
    output$work_model_table <- DT::renderDataTable({
      datatable(m_step_df_work,
                options = list(
                  order = list(3, 'desc'),
                  paging =TRUE,
                  pageLength = 50
                )         
      ) %>%
        formatRound(columns = names(m_step_df_work)[-1], digits = 2)
    })
    
    output$teen_brate_model_table <- DT::renderDataTable({
      datatable(m_step_df_teen_brate,
                options = list(
                  order = list(3, 'desc'),
                  paging =TRUE,
                  pageLength = 50
                )         
      ) %>%
        formatRound(columns = names(m_step_df_teen_brate)[-1], digits = 2)
    })
    
    
    # --- IV Contribution ---
    
    # Play index
    output$iv_contr_stack_plot <- renderPlot({
      
     data <- m_step_df_play %>% arrange(pratt) %>% 
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
      
      data <- m_step_df_play %>% arrange(pratt) %>% 
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
    

    # Rest Index
    output$rest_iv_contr_stack_plot <- renderPlot({
      
      data <- m_step_df_rest %>% arrange(pratt) %>% 
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
    
    output$rest_iv_contr_plot <- renderPlot({
      
      data <- m_step_df_rest %>% arrange(pratt) %>% 
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
    
    
    # Rest Index
    output$work_iv_contr_stack_plot <- renderPlot({
      
      data <- m_step_df_work %>% arrange(pratt) %>% 
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
    
    output$work_iv_contr_plot <- renderPlot({
      
      data <- m_step_df_work %>% arrange(pratt) %>% 
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

      data <- play_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_histogram() +
        theme_minimal()
      
    })
    
    output$play_index_boxplot <- renderPlot({

      data <- play_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_boxplot() +
        theme_minimal()
      
    })
    
    output$play_table <- DT::renderDataTable({
      
       datatable(play_z_geo_df,
         options = list(
             paging =TRUE,
             pageLength = 50
          )         
        ) %>%
        formatRound(columns = names(m_step_df_play)[-1], digits = 2)
    })
    
    # Map of Play Index Quintiles
    output$play_map <- renderPlot({
      # see https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
    map_data <- left_join(play_fixed_z_data_wgeo, countydata, by = c("fips" = "county_fips")) %>%
      left_join(urbnmapr::counties, by = c("fips" = "county_fips"))
  
    
    ggplot(map_data, aes(long, lat, group = fips, fill = factor(quintile))) +
      geom_polygon(color = "black") +
      coord_map() +
      labs(fill = "Play Index") +
      theme_minimal() +
      scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
      scale_fill_manual(values = quintile_colour_pal) +
      # guides(fill = guide_colourbar(nbbin = 100)) +
      theme(legend.position = "bottom",
            legend.key.width = unit(7, "cm"))
    
    })
    
    
    output$rest_index_hist <- renderPlot({
      
      data <- rest_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_histogram() +
        theme_minimal()
      
    })
    
    output$rest_index_boxplot <- renderPlot({
      
      data <- rest_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_boxplot() +
        theme_minimal()
      
    })
    
    # Map of Rest Index Quintiles
    output$rest_map <- renderPlot({
      # see https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
      map_data <- left_join(rest_fixed_z_data_wgeo, countydata, by = c("fips" = "county_fips")) %>%
        left_join(urbnmapr::counties, by = c("fips" = "county_fips"))
      
      ggplot(map_data, aes(long, lat, group = fips, fill = factor(quintile))) +
        geom_polygon(color = "black") +
        coord_map() +
        labs(fill = "Rest Index") +
        theme_minimal() +
        scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
        scale_fill_manual(values = quintile_colour_pal) +
        # guides(fill = guide_colourbar(nbbin = 100)) +
        theme(legend.position = "bottom",
              legend.key.width = unit(7, "cm"))
    })
    
    
    output$work_index_hist <- renderPlot({
      
      data <- work_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_histogram() +
        theme_minimal()
      
    })
    
    output$work_index_boxplot <- renderPlot({
      
      data <- work_z_geo_df()
      
      ggplot(data, aes(score)) +
        geom_boxplot() +
        theme_minimal()
      
    })
    
    # Map of Work Index Quintiles
    output$work_map <- renderPlot({
      # see https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
      map_data <- left_join(work_fixed_z_data_wgeo, countydata, by = c("fips" = "county_fips")) %>%
        left_join(urbnmapr::counties, by = c("fips" = "county_fips"))
      
      ggplot(map_data, aes(long, lat, group = fips, fill = factor(quintile))) +
        geom_polygon(color = "black") +
        coord_map() +
        labs(fill = "Work Index") +
        theme_minimal() +
        scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
        scale_fill_manual(values = quintile_colour_pal) +
        # guides(fill = guide_colourbar(nbbin = 100)) +
        theme(legend.position = "bottom",
              legend.key.width = unit(7, "cm"))
      
    })
    
    # Map of Cross Index Quintiles
    output$cross_index_map <- renderPlot({
      
      map_data <- new_cross_map %>%
        left_join(countydata, by = c("fips" = "county_fips")) %>%
        left_join(urbnmapr::counties, by = c("fips" = "county_fips"))

      ggplot(map_data, aes(long, lat, group = fips, fill = factor(`US All Quintile Map`))) +
        geom_polygon(color = "black") +
        coord_map() +
        labs(fill = "Cross Indices") +
        theme_minimal() +
        # scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
        # scale_fill_manual(values = quintile_colour_pal) +
        scale_fill_manual(
          values = quintile_colour_pal_lst, drop = FALSE,  na.value="transparent") +
        theme(legend.position = "bottom",
              legend.key.width = unit(3, "cm"))
      
      # old map
      # ggplot(map_data, aes(long, lat, group = fips, fill = factor(all_quintile))) +
      #   geom_polygon(color = "black") +
      #   coord_map() +
      #   labs(fill = "Cross Indices") +
      #   theme_minimal() +
      #   scale_fill_continuous(limits = c(1, 5), breaks = seq(1, 5, 1)) +
      #   scale_fill_manual(values = quintile_colour_pal) +
      #   # guides(fill = guide_colourbar(nbbin = 100)) +
      #   theme(legend.position = "bottom",
      #         legend.key.width = unit(3, "cm"))
      
    })
    
    # Play Quintile Matrix
    output$play_quintile_matrix <- renderPlot({
      
      
      req(input$county)
      data <- play_fixed_z_data_wgeo_long
      
      data <- data %>%
        left_join(quintile_colour_pal_df, by = "quintile")
       
      if (input$region != "--") {
        
        data <- data %>%
          ungroup() %>%
          group_by(var_name, pratt, State, Region) %>%
          summarize(
            score = mean(score, na.rm = T)
          ) %>%
          ungroup() %>%
          group_by(var_name) %>%
          mutate(
            rank_value = rank(-score),
            per_rank_value = percent_rank(score) * 100,
            quintile = ntile(score, 5)
          )
        
        subdata <- filter(data, 
                          var_name %in% measure_all_lst_play,
                          State == input$state, 
                          Region == input$region) %>%
          mutate(
            `Key Impact` = factor(ifelse(var_name %in% measure_top3_lst_play, 1, NA)),
            quintile = factor(quintile),
            label = str_wrap(str_replace_all(var_name, "_", " "), 16),
            order = rank(score))
        
      } else {
        # subset data to select county
        subdata <- filter(data, 
                          var_name %in% measure_all_lst_play,
                          State == input$state, 
                          County == input$county) %>%
          mutate(
            `Key Impact` = factor(ifelse(var_name %in% measure_top3_lst_play, 1, NA)),
            quintile = factor(quintile),
            label = str_wrap(str_replace_all(var_name, "_", " "), 16),
            order = rank(score)) 
      }
      
      use_colour <- sort(as.numeric(as.character(unique(subdata$quintile))))
      
      # filter by drop down
      if (input$iv_domain != "Key Impact") {
        subdata <- filter(subdata,
          Domain == input$iv_domain
          )
      }
      
      
      p <- ggplot(subdata, aes(label, factor(1))) +
        geom_tile(aes(colour = `Key Impact`, fill = quintile), size = 2.2) +
          geom_text(aes(label = label), size= 4.1, lineheight = .8, fontface = "bold") +
        scale_color_manual(values = c("black"),  na.value="transparent", guide = "none") +
        scale_fill_manual(
          values = quintile_colour_pal_lst[use_colour], drop = FALSE,  na.value="transparent") +
        theme_blank() +
        theme(
          legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.spacing = unit(0, "null"))
      
      if (input$play_matrix_sort == "Index Measures Quintile Assignment") {
        p + facet_wrap(~ quintile + -round(pratt, 3) + var_name, scales = "free")
      } else if (input$play_matrix_sort == "Order of Measure Impact on Index") {
        p + facet_wrap(~ -round(pratt, 3) + var_name, scales = "free")
      }
      
    })
    
    # Rest Quintile Matrix
    output$rest_quintile_matrix <- renderPlot({
      
      
      req(input$rest_county)
      data <- rest_fixed_z_data_wgeo_long
      
      data <- data %>%
        left_join(quintile_colour_pal_df, by = "quintile")
      
    
      if (input$rest_region != "--") {
        
        data <- data %>%
          ungroup() %>%
          group_by(var_name, pratt, State, Region) %>%
          summarize(
            score = mean(score, na.rm = T)
          ) %>%
          ungroup() %>%
          group_by(var_name) %>%
          mutate(
            rank_value = rank(-score),
            per_rank_value = percent_rank(score) * 100,
            quintile = ntile(score, 5)
          )
        
        subdata <- filter(data, 
                          var_name %in% measure_all_lst_rest,
                          State == input$rest_state, 
                          Region == input$rest_region) %>%
          mutate(
            `Key Impact` = factor(ifelse(var_name %in% measure_top3_lst_rest, 1, NA)),
            quintile = factor(quintile),
            label = str_wrap(str_replace_all(var_name, "_", " "), 16),
            order = rank(score))
        
      } else {
        
        # subset data to select county
        subdata <- filter(data, 
                          var_name %in% measure_all_lst_rest,
                          State == input$rest_state, 
                          County == input$rest_county) %>%
          mutate(
            `Key Impact` = factor(ifelse(var_name %in% measure_top3_lst_rest, 1, NA)),
            quintile = factor(quintile),
            label = str_wrap(str_replace_all(var_name, "_", " "), 16),
            order = rank(score))
      }

      use_colour <- sort(as.numeric(as.character(unique(subdata$quintile))))
      
      
      # filter by drop down
      if (input$rest_iv_domain != "Key Impact") {
        subdata <- filter(subdata,
                          Domain == input$rest_iv_domain
        )
      }
      
      
      p <- ggplot(subdata, aes(label, factor(1))) +
        geom_tile(aes(colour = `Key Impact`, fill = quintile), size = 2.2) +
        geom_text(aes(label = label), size= 4.1, lineheight = .8, fontface = "bold") +
        scale_color_manual(values = c("black"),  na.value="transparent", guide = "none") +
        scale_fill_manual(
          values = quintile_colour_pal_lst[use_colour], drop = FALSE,  na.value="transparent") +
        theme_blank() +
        theme(
          legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.spacing = unit(0, "null"))
      
      if (input$rest_matrix_sort == "Index Measures Quintile Assignment") {
        p + facet_wrap(~ quintile + -round(pratt, 3) + var_name, scales = "free")
      } else if (input$rest_matrix_sort == "Order of Measure Impact on Index") {
        p + facet_wrap(~ -round(pratt, 3) + var_name, scales = "free")
      }
      
    })
    
    # Work Quintile Matrix
    output$work_quintile_matrix <- renderPlot({
      
      
      req(input$work_county)
      data <- work_fixed_z_data_wgeo_long
      
      data <- data %>%
        left_join(quintile_colour_pal_df, by = "quintile")
      
      if (input$work_region != "--") {
        
        data <- data %>%
          ungroup() %>%
          group_by(var_name, pratt, State, Region) %>%
          summarize(
            score = mean(score, na.rm = T)
          ) %>%
          ungroup() %>%
          group_by(var_name) %>%
          mutate(
            rank_value = rank(-score),
            per_rank_value = percent_rank(score) * 100,
            quintile = ntile(score, 5)
          )
        
        subdata <- filter(data, 
                          var_name %in% measure_all_lst_work,
                          State == input$work_state, 
                          Region == input$work_region) %>%
          mutate(
            `Key Impact` = factor(ifelse(var_name %in% measure_top3_lst_work, 1, NA)),
            quintile = factor(quintile),
            label = str_wrap(str_replace_all(var_name, "_", " "), 16),
            order = rank(score))
        
      } else {
        
        # subset data to select county
        subdata <- filter(data, 
                          var_name %in% measure_all_lst_work,
                          State == input$work_state, 
                          County == input$work_county) %>%
          mutate(
            `Key Impact` = factor(ifelse(var_name %in% measure_top3_lst_work, 1, NA)),
            quintile = factor(quintile),
            label = str_wrap(str_replace_all(var_name, "_", " "), 16),
            order = rank(score))
      }
      
      use_colour <- sort(as.numeric(as.character(unique(subdata$quintile))))

      
      # filter by drop down
      if (input$work_iv_domain != "Key Impact") {
        subdata <- filter(subdata,
                          Domain == input$work_iv_domain
        )
      }
      
 
      p <- ggplot(subdata, aes(label, factor(1))) +
        geom_tile(aes(colour = `Key Impact`, fill = quintile), size = 2.2) +
        geom_text(aes(label = label), size= 4.1, lineheight = .8, fontface = "bold") +
        scale_color_manual(values = c("black"),  na.value="transparent", guide = "none") +
        scale_fill_manual(
          values = quintile_colour_pal_lst[use_colour], drop = FALSE,  na.value="transparent") +
        theme_blank() +
        theme(
          legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.spacing = unit(0, "null"))
      
      if (input$work_matrix_sort == "Index Measures Quintile Assignment") {
        p + facet_wrap(~ quintile + -round(pratt, 3) + var_name, scales = "free")
      } else if (input$work_matrix_sort == "Order of Measure Impact on Index") {
        p + facet_wrap(~ -round(pratt, 3) + var_name, scales = "free")
      }
      
    })
    
    #### Sankey Charts
    
    output$play_sankey <- renderPlot({
      
      data <- filter(m_step_df_play, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_play)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      

      data <- mutate(data, 
                         Index = "Play",
                         var = factor(var_name, levels = measure_all_lst_play),
                         Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("Play Index") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$rest_sankey <- renderPlot({
      
      data <- filter(m_step_df_rest, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_rest)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")
      
      data <- mutate(data, 
                     Index = "Rest",
                     var = factor(var_name, levels = measure_all_lst_rest),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("Rest Index") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$work_sankey <- renderPlot({
      
      data <- filter(m_step_df_work, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_work)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")
      
      data <- mutate(data, 
                     Index = "Work",
                     var = factor(var_name, levels = measure_all_lst_work),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("Work Index") +
        # scale_fill_brewer() +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    # Second LAyer
    output$fp_health_sankey <- renderPlot({
      
      data <- filter(m_step_df_fp_health, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_fp_health)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_fp_health),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("% Fair or Poor Health") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$grad_sankey <- renderPlot({
      
      data <- filter(m_step_df_grad, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_grad)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_grad),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("% with Grad or Prof Degree") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$grad2_sankey <- renderPlot({
      
      data <- filter(m_step_df_grad, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_grad)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_grad),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("% with Grad or Prof Degree") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$diabetes_sankey <- renderPlot({
      
      data <- filter(m_step_df_diabetes, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_diabetes)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_diabetes),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("% with Diabetes") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$avg_m_days_sankey <- renderPlot({
      
      data <- filter(m_step_df_avg_m_days, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_avg_m_days)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_avg_m_days),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("Avg # of Mentally UnHealthy Days") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$avg_m_days2_sankey <- renderPlot({
      
      data <- filter(m_step_df_avg_m_days, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_avg_m_days)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_avg_m_days),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("Avg # of Mentally UnHealthy Days") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$obesity_sankey <- renderPlot({
      
      data <- filter(m_step_df_obesity, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_obesity)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_obesity),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("% with Obesity") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$phy_inactive_sankey <- renderPlot({
      
      data <- filter(m_step_df_phy_inactive, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_phy_inactive)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_phy_inactive),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("% Physical Inactive") +
        theme_void() +
        theme(legend.position = "none")
      
    })
    
    output$teen_brate_sankey <- renderPlot({
      
      data <- filter(m_step_df_teen_brate, var_name != "(Intercept)") %>%
        arrange(desc(pratt)) %>%
        filter(var_name %in% measure_all_lst_teen_brate)
      
      data <- data %>% 
        left_join(domain_map, by = "var_name")
      
      data <- data %>%
        left_join(domain_color_df, by = "Domain")      
      
      data <- mutate(data, 
                     var = factor(var_name, levels = measure_all_lst_teen_brate),
                     Freq = pratt*15 + 1) %>%
        ungroup() %>%
        arrange(pratt)
      
      # Estimate Domain Pratt by summing them
      data <- data %>%
        group_by(Domain) %>% 
        mutate(pratt_domain = sum(pratt, na.rm = T)) %>%
        ungroup() %>%
        arrange(desc(pratt_domain))
      
      domain_sort_order <- unique(data$Domain)
      
      # Set domain order based on estimate sum Pratt of measures
      data <- data %>%
        mutate(domain_order = factor(Domain, levels = domain_sort_order))
      
      ggplot(data,
             aes(y = Freq, axis1 = NA, axis2 = domain_order, axis3 = var)) +
        geom_alluvium(aes(fill = I(domain_color)), width = 1/12) +
        geom_stratum(width = 1/20, aes(fill = domain_color), color = NA) +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), hjust = 1, nudge_x = -.05) +
        # scale_x_discrete(limits = c("Index", "Domain", "var_name"), expand = c(.05, .05, .05)) +
        # scale_fill_brewer(type = "qual", palette = "Set1") +
        ggtitle("Teen Birthrate") +
        theme_void() +
        theme(legend.position = "none")
      
    })
})
