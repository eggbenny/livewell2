# ui.R
# Benedito Chou
# July 13 2021


# --- ui --------------------------------------------------

# --- Header
header <- dashboardHeader(title = "LiveWell Prototype Demo")

sidebar <- dashboardSidebar(
    
  sidebarMenu(
    menuItem("Play Dashboard", tabName = "dashboard", 
             icon = icon("dashboard"),
             badgeLabel = "Demo", badgeColor = "green"),
    menuItem("Rest Dashboard", tabName = "dashboard_rest", 
             icon = icon("dashboard"),
             badgeLabel = "Demo", badgeColor = "green"),
    menuItem("Work Dashboard", tabName = "dashboard_work", 
             icon = icon("dashboard"),
             badgeLabel = "Demo", badgeColor = "green"),
    menuItem("Play Model Table", icon = icon("th"),
             tabName = "playModelTable"),
    menuItem("Rest Model Table", icon = icon("th"),
             tabName = "restModelTable"),
    menuItem("Work Model Table", icon = icon("th"),
             tabName = "workModelTable"),
    menuItem("Play Index", icon = icon("bar-chart-o"),
             tabName = "playIndex"),
    menuItem("Rest Index", icon = icon("bar-chart-o"),
             tabName = "restIndex"),
    menuItem("Work Index", icon = icon("bar-chart-o"),
             tabName = "workIndex"),
    menuItem("Map", icon = icon("map-marked"),
             tabName = "map"),
    br(),
    checkboxInput("extra", "Show Extra Card")
  ) # End of sidebarMenu
  
) # End of dashboardSidebar

# --- Body
body <- dashboardBody(
    
   useShinyjs(),  # Set up shinyjs
  
  tabItems(
      
    tabItem(tabName = "dashboard",
      
      h3("Dashboard"),
      
      fluidRow(
        
        tabBox(
          width = 3,
           title = "Impact",
           # The id lets us use input$tabset1 on the server to find the current tab
           id = "tabset1", 
           tabPanel("Key Measures",
              fluidRow(
                    infoBoxOutput("index_echo", width = 12),
                    infoBoxOutput("index_rank", width = 12),
                    infoBoxOutput("dv_echo", width = 12),
                    infoBoxOutput("dv_rank", width = 12),
                    infoBoxOutput("population", width = 12),
                    infoBoxOutput("pop_impact", width = 12),
                    conditionalPanel(
                        condition = "input.extra == true",
                         infoBoxOutput("play_extra_impact_card", width = 12)
                    ),
                    # infoBoxOutput("iv_echo", width = 12),
                    # infoBoxOutput("iv_rank", width = 12)  
              )
            )
           
          # tabPanel("Criteria Measures",
          #   fluidRow(
          #     column(width = 12,
          #     h4("Correlation with: "),
          #           infoBoxOutput("criterion1_card", width = 12),
          #           infoBoxOutput("criterion2_card", width = 12),
          #           infoBoxOutput("criterion3_card", width = 12),
          #           infoBoxOutput("criterion4_card", width = 12)
          #     )
          #   )
          #  )
        ),
        
      # column(width = 3,
      #   # infoBoxOutput("criterion1_card", width = 12),
      #   # infoBoxOutput("criterion2_card", width = 12),
      #   # infoBoxOutput("criterion3_card", width = 12),
      #   # infoBoxOutput("criterion4_card", width = 12),
      #   # infoBoxOutput("index_echo", width = 12),
      #   # infoBoxOutput("index_rank", width = 12),
      #   # infoBoxOutput("population", width = 12),
      #   # infoBoxOutput("pop_impact", width = 12),
      #   # infoBoxOutput("dv_echo", width = 12),
      #   # infoBoxOutput("dv_rank", width = 12),
      #   # infoBoxOutput("iv_echo", width = 12),
      #   # infoBoxOutput("iv_rank", width = 12)
      # 
      # ),  #End of column
      
      # box(width = 3,
      # 
      # 
      # ), # End of box
      
   conditionalPanel(
    condition = "input.extra == true",
    tabBox(width = 3,
      title = "Impact of Play on",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset2", 
     tabPanel("Key Criteria",
      fluidRow(
        infoBoxOutput("play_impact_card_1", width = 12),
        infoBoxOutput("play_impact_card_2", width = 12),
        infoBoxOutput("play_impact_card_3", width = 12),
        infoBoxOutput("play_impact_card_4", width = 12),
        infoBoxOutput("play_impact_card_5", width = 12)
      )  
     )
    ) #End of box
   ),
   
   conditionalPanel(
    condition = "input.extra == false",   
    column(width = 6,
           column(12,
                  actionButton("hide_play_matrix", "Show / Hide Matrix",
                               style = "color: darkgrey;
                           background-color: white"),
                  actionButton("hide_play_scatter", "Show / Hide Scatterplot",
                               style = "color: darkgrey;
                           background-color: white")
           ),
      box(width = 12,  
          column(id = "play_matrix_box", width = 12,
            selectizeInput("play_matrix_sort", "Sort Matrix By",
                           choices = c("Index Measures Quintile Assignment", "Order of Measure Impact on Index"),
                           selected = "Index Measures Quintile Assignment"),
            plotOutput("play_quintile_matrix")
          ),
          column(id = "play_scatter_box", width = 12,
            plotOutput("play_grid_plot")
          )
     ) # End of box
    )
    ),
      
      column(width = 3,
          
        box(width = 12,
          column(width = 6,
          selectizeInput("state", "Select a state",
            choices = sort(unique(ana_data_wgeo$state)),
            selected = "North Carolina")
          ),
          
          column(width = 6,
          selectizeInput("county", "Select a county",
            choices = sort(unique(ana_data_wgeo$county)),
            selected = "Richmond")
          ),
          
          column(width = 6,
          selectizeInput("region", "Group by Region",
            choices = c("--", sort(unique(ana_data_wgeo$RegionOrg))))
          ),
          
          # column(width = 12,
          #  sliderInput("change1",
          #   "If I change % fair or poor health by 1% ... ",
          #   min = -20,
          #   max = 20,
          #   value = 0),
          # sliderInput("change2",
          #   "If I change % of adults with obesity by 1% ... ",
          #   min = -20,
          #   max = 20,
          #   value = 0),
          # sliderInput("change3",
          #   "If I changee % of insufficient sleep by 1% ... ",
          #   min = -20,
          #   max = 20,
          #   value = 0),
          # sliderInput("change4",
          #   "If I change % of smokers by 1% ... ",
          #   min = -20,
          #   max = 20,
          #   value = 0),
          # sliderInput("change5",
          #   "If I change % of excessive drinking by 1% ... ",
          #   min = -20,
          #   max = 20,
          #   value = 0),
          # ),
          
         column(width = 12,
           actionButton("reset_play", "Reset Measures") 
         ),
          
         column(width = 12,
          selectizeInput("iv_top5", "Select a measure",
            choices = c("Select a Measure" = "Select a Measure", 
                        "% Fair or Poor Health" = "per_fair_or_poor_health",
                        "% with Grad or Prof Degree" = "per_w_grad_or_prof_degree",
                        "% of Adults with Diabetes" = "per_adults_with_diabetes"),
            selected = "% Fair or Poor Health"),
            
          sliderInput("change_top5",
            "If I change the selected top 5 measure by 1% ... ",
            min = -20,
            max = 20,
            value = 0)
          ),
          
          column(width = 12,
            selectizeInput("iv_domain", "Filter by Key Impact / Domain",
              choices = c("Key Impact", domain_lst),
              selected = "Key Impact"),
            selectizeInput("iv", "Select a measure",
              choices = c("Select a Measure", measure_lst_play),
              selected = "Select a Measure"),
            sliderInput("change",
              "If I change the selected measure by 1% ... ",
              min = -20,
              max = 20,
              value = 0)
            ) # End of column
        ) # End of box
      ) # End of column
      ) # End of fluidRow
      
     ), # End of tabItem
    

 tabItem(tabName = "dashboard_rest",
      
      h3("Dashboard"),
      
      fluidRow(
        
        tabBox(
          width = 3,
           title = "Impact",
           # The id lets us use input$tabset1 on the server to find the current tab
           id = "tabset1_rest", 
           tabPanel("Key Measures",
              fluidRow(
                    infoBoxOutput("rest_index_echo", width = 12),
                    infoBoxOutput("rest_index_rank", width = 12),
                    infoBoxOutput("rest_dv_echo", width = 12),
                    infoBoxOutput("rest_dv_rank", width = 12),
                    infoBoxOutput("rest_population", width = 12),
                    infoBoxOutput("rest_pop_impact", width = 12),
                    conditionalPanel(
                        condition = "input.extra == true",
                         infoBoxOutput("rest_extra_impact_card", width = 12)
                    )
                    # infoBoxOutput("iv_echo", width = 12),
                    # infoBoxOutput("iv_rank", width = 12)  
              )
            )
           
          # tabPanel("Criteria Measures",
          #   fluidRow(
          #     column(width = 12,
          #     h4("Correlation with: "),
          #           infoBoxOutput("criterion1_card", width = 12),
          #           infoBoxOutput("criterion2_card", width = 12),
          #           infoBoxOutput("criterion3_card", width = 12),
          #           infoBoxOutput("criterion4_card", width = 12)
          #     )
          #   )
          #  )
        ),
        
        conditionalPanel(
          condition = "input.extra == true",
          tabBox(width = 3,
                 title = "Impact of Rest on",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "rest_tabset2", 
                 tabPanel("Key Criteria",
                          fluidRow(
                            infoBoxOutput("rest_impact_card_1", width = 12)
                          )  
                 )
          ) #End of box
        ),
        
        conditionalPanel(
          condition = "input.extra == false",   
    column(width = 6,  
           column(12,
           actionButton("hide_rest_matrix", "Show / Hide Matrix",
                        style = "color: darkgrey;
                           background-color: white"),
           actionButton("hide_rest_scatter", "Show / Hide Scatterplot",
                        style = "color: darkgrey;
                           background-color: white")
           ),
           box(width = 12,  
               column(id = "rest_matrix_box", width = 12,
                   selectizeInput("rest_matrix_sort", "Sort Matrix By",
                                  choices = c("Index Measures Quintile Assignment", "Order of Measure Impact on Index"),
                                  selected = "Index Measures Quintile Assignment"),
                   plotOutput("rest_quintile_matrix")
               ),
               column(id = "rest_scatter_box", width = 12,
               plotOutput("rest_grid_plot")
               )
           ) # End of box
    )
    ),
      
      column(width = 3,
          
        box(width = 12,
          column(width = 6,
          selectizeInput("rest_state", "Select a state",
            choices = sort(unique(ana_data_wgeo$state)),
            selected = "North Carolina")
          ),
          
          column(width = 6,
          selectizeInput("rest_county", "Select a county",
            choices = sort(unique(ana_data_wgeo$county)),
            selected = "Richmond")
          ),
          
          column(width = 6,
                 selectizeInput("rest_region", "Group by Region",
                                choices = c("--", sort(unique(ana_data_wgeo$RegionOrg))))
          ),
          
          column(width = 12,
                 actionButton("reset_rest", "Reset Measures") 
          ),
          
         column(width = 12,
          selectizeInput("rest_iv_top5", "Select a measure",
            choices = c("Select a Measure",
                        "Avg No of Mentally Unhealthy Days" = "avg_no_of_mentally_unhealthy_days",
                        "% Physical Inactive" = "per_physically_inactive",          
                        "% with Obesity" = "per_adults_with_obesity"),
            selected = "Avg No of Mentally Unhealthy Days"),
            
          sliderInput("rest_change_top5",
            "If I change the selected top 5 measure by 1% ... ",
            min = -20,
            max = 20,
            value = 0)
          ),
          
          column(width = 12,
           selectizeInput("rest_iv_domain", "Filter by Key Impact / Domain",
                          choices = c("Key Impact", domain_lst),
                          selected = "Key Impact"),
            selectizeInput("rest_iv", "Select a measure",
              choices = c("Select a Measure", measure_lst_rest),
              selected = "Select a Measure"),
            sliderInput("rest_change",
              "If I change the selected measure by 1% ... ",
              min = -20,
              max = 20,
              value = 0)
            ) # End of column
        ) # End of box
      ) # End of column
      ) # End of fluidRow
      
     ), # End of tabItem    


     tabItem(tabName = "dashboard_work",
         
         h3("Dashboard"),
         
         fluidRow(
           
           tabBox(
             width = 3,
             title = "Impact",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1_work", 
             tabPanel("Key Measures",
                      fluidRow(
                        infoBoxOutput("work_index_echo", width = 12),
                        infoBoxOutput("work_index_rank", width = 12),
                        infoBoxOutput("work_dv_echo", width = 12),
                        infoBoxOutput("work_dv_rank", width = 12),
                        infoBoxOutput("work_population", width = 12),
                        infoBoxOutput("work_pop_impact", width = 12),
                        conditionalPanel(
                          condition = "input.extra == true",
                          infoBoxOutput("work_extra_impact_card", width = 12)
                        )
                        # infoBoxOutput("iv_echo", width = 12),
                        # infoBoxOutput("iv_rank", width = 12)  
                      )
             )
             
             # tabPanel("Criteria Measures",
             #   fluidRow(
             #     column(width = 12,
             #     h4("Correlation with: "),
             #           infoBoxOutput("criterion1_card", width = 12),
             #           infoBoxOutput("criterion2_card", width = 12),
             #           infoBoxOutput("criterion3_card", width = 12),
             #           infoBoxOutput("criterion4_card", width = 12)
             #     )
             #   )
             #  )
           ),
           
           column(width = 6,   
                  column(12,
                         actionButton("hide_work_matrix", "Show / Hide Matrix",
                                      style = "color: darkgrey;
                           background-color: white"),
                         actionButton("hide_work_scatter", "Show / Hide Scatterplot",
                                      style = "color: darkgrey;
                           background-color: white")
                  ),
                  box(width = 12,  
                      column(id = "work_matrix_box", width = 12,
                          selectizeInput("work_matrix_sort", "Sort Matrix By",
                                         choices = c("Index Measures Quintile Assignment", "Order of Measure Impact on Index"),
                                         selected = "Index Measures Quintile Assignment"),
                          plotOutput("work_quintile_matrix")
                      ),
                      column(id = "work_scatter_box", width = 12,
                        plotOutput("work_grid_plot")
                      )
                  ) # End of box
           ),
           
           column(width = 3,
                  
                  box(width = 12,
                      column(width = 6,
                             selectizeInput("work_state", "Select a state",
                                            choices = sort(unique(ana_data_wgeo$state)),
                                            selected = "North Carolina")
                      ),
                      
                      column(width = 6,
                             selectizeInput("work_county", "Select a county",
                                            choices = sort(unique(ana_data_wgeo$county)),
                                            selected = "Richmond")
                      ),
                      
                      column(width = 6,
                             selectizeInput("work_region", "Group by Region",
                                            choices = c("--", sort(unique(ana_data_wgeo$RegionOrg))))
                      ),
                      
                      column(width = 12,
                             actionButton("reset_work", "Reset Measures") 
                      ),
                      
                      column(width = 12,
                             selectizeInput("work_iv_top5", "Select a measure",
                                            choices = c("Select a Measure",
                                                        "Teen Birthrate" = "teen_birth_rate",
                                                        "% with Grad or Prof Degree" = "per_w_grad_or_prof_degree",
                                                        "Avg No of Mentally Unhealthy Days" = "avg_no_of_mentally_unhealthy_days"),
                                            selected = "Teen Birthrate"),
                             
                             sliderInput("work_change_top5",
                                         "If I change the selected top 5 measure by 1% ... ",
                                         min = -20,
                                         max = 20,
                                         value = 0)
                      ),
                      
                      column(width = 12,
                             selectizeInput("work_iv_domain", "Filter by Key Impact / Domain",
                                            choices = c("Key Impact", domain_lst),
                                            selected = "Key Impact"),
                             selectizeInput("work_iv", "Select a measure",
                                            choices = c("Select a Measure", measure_lst_work),
                                            selected = "Select a Measure"),
                             sliderInput("work_change",
                                         "If I change the selected measure by 1% ... ",
                                         min = -20,
                                         max = 20,
                                         value = 0)
                      ) # End of column
                  ) # End of box
           ) # End of column
         ) # End of fluidRow
         
     ), # End of tabItem    
 
    tabItem(tabName = "playModelTable",
            
        tabBox(
          side = "left", height = "900px", width = 12,
          selected = "Play",
          tabPanel("Play",
            h2("Play Index - Weight (b) table from Stepwise Regression"),
            box(width = 12,
                DT::dataTableOutput("play_model_table")
            ) # End of box)
          ), # End of tabPanel
          tabPanel("Fair or Poor Health",
            h2("Fair or Poor Health - Weight (b) table from Stepwise Regression"),
            box(width = 12,
                DT::dataTableOutput("fp_health_model_table")
            ) # End of box        
          ),  # End of tabPanel
          tabPanel("Grad or Prof Degree",
            h2("% Adult with Grad or Prof Degree - Weight (b) table from Stepwise Regression"),
            box(width = 12,
                DT::dataTableOutput("grad_model_table")
            ) # End of box        
          ), # End of tabPanel
          tabPanel("Adult with Diabetes",
            h2("% Adult with Diabetes - Weight (b) table from Stepwise Regression"),
            box(width = 12,
                DT::dataTableOutput("diabetes_model_table")
            ) # End of box        
          )  # End of tabPanel
        ) # End of tabBox
      
    ), # End of tabItem
 
    tabItem(tabName = "restModelTable",
          
          tabBox(
          side = "left", height = "900px", width = 12,
          selected = "Rest",
          tabPanel("Rest",
            h2("Rest Index - Weight (b) table from Stepwise Regression"),
            box(width = 12,
              DT::dataTableOutput("rest_model_table")
            ) # End of box
          ), # End of tabPanel
          tabPanel("Mentally Unhealthy Days",
                   h2("Avg No of Mentally Unhealthy Days - Weight (b) table from Stepwise Regression"),
                   box(width = 12,
                       DT::dataTableOutput("avg_m_days_model_table")
                   ) # End of box        
          ),  # End of tabPanel
          tabPanel("Physical Inactivity",
            h2("Physical Inactivity - Weight (b) table from Stepwise Regression"),
            box(width = 12,
                DT::dataTableOutput("phy_inactive_model_table")
            ) # End of box        
          ),  # End of tabPanel
          tabPanel("Obesity",
            h2("% with Obesity - Weight (b) table from Stepwise Regression"),
            box(width = 12,
                DT::dataTableOutput("obesity_model_table")
            ) # End of box        
          ) # End of tabPanel
        ) # End of tabBox
      
    ), # End of tabItem
 
    tabItem(tabName = "workModelTable",
         
         tabBox(
           side = "left", height = "900px", width = 12,
           selected = "Work",
           tabPanel("Work",
                    h2("Work Index - Weight (b) table from Stepwise Regression"),
                    box(width = 12,
                        DT::dataTableOutput("work_model_table")
                    ) # End of box
           ), # End of tabPanel
           tabPanel("Teen Birthrate",
                    h2("Teen Birthrate - Weight (b) table from Stepwise Regression"),
                    box(width = 12,
                        DT::dataTableOutput("teen_brate_model_table")
                    ) # End of box        
           ),  # End of tabPane
           tabPanel("Grad or Prof Degree",
                    h2("% Adult with Grad or Prof Degree - Weight (b) table from Stepwise Regression"),
                    box(width = 12,
                        DT::dataTableOutput("grad_model_table_work")
                    ) # End of box        
           ), # End of tabPanel
           tabPanel("Mentally Unhealthy Days",
                    h2("Avg No of Mentally Unhealthy Days - Weight (b) table from Stepwise Regression"),
                    box(width = 12,
                        DT::dataTableOutput("avg_m_days_model_table_work")
                    ) # End of box
           ) # End of tabPanel
         ) # End of tabBox
         
    ), # End of tabItem
    
    tabItem(tabName = "playIndex",
            
     fluidRow(
      column(width = 12,
      h2("Enablers of Play"),
            
      box(width = 12,
        column(width = 6,
          plotOutput("iv_contr_plot", height = 100)
        ),
        column(width = 6,
          plotOutput("iv_contr_stack_plot", height = 100)
        )
      ), # End of box
            
      h2("Play Index Distribution"),
      
      # box(width = 12,
      #   DT::dataTableOutput("play_table")
      # ),
      
      box(width = 6,
        plotOutput("play_index_hist")
      ), # End of box
      box(width = 6,
        plotOutput("play_index_boxplot")
      ), # End of box
      
      h2("Play Index Correlation With Criteria"),
      
      box(width = 6,
        plotOutput("test_criterion_plot_1")
      ), # End of box
      box(width = 6,
        plotOutput("test_criterion_plot_2")
      ), # End of box
      
      box(width = 6,
        plotOutput("test_criterion_plot_3")
      ), # End of box
      box(width = 6,
        plotOutput("test_criterion_plot_4")
      ), # End of box
      
      box(width = 6,
        plotOutput("test_criterion_plot_a1")
      ), # End of box
      box(width = 6,
        plotOutput("test_criterion_plot_a2")
      ), # End of box
      
      box(width = 6,
        plotOutput("test_criterion_plot_a3")
      ), # End of box
      box(width = 6,
        plotOutput("test_criterion_plot_a4")
      ), # End of box
    
      box(width = 6,
        plotOutput("test_criterion_plot_a5")
      ), # End of box
      box(width = 6,
        plotOutput("test_criterion_plot_a6")
      ), # End of box
      
      box(width = 6,
        plotOutput("test_criterion_plot_a7")
      ), # End of box
      box(width = 6,
        plotOutput("test_criterion_plot_a8")
      ), # End of box
      
      box(width = 6,
        plotOutput("test_criterion_plot_a9")
      ), # End of box
      
      box(width = 6,
        plotOutput("test_criterion_plot_a10")
      ), # End of box
      box(width = 6,
        plotOutput("test_criterion_plot_a11")
      ), # End of box
      
      box(width = 6,
          plotOutput("test_criterion_plot_a12")
      ), # End of box
      box(width = 6,
          plotOutput("test_criterion_plot_a13")
      ) # End of box
      
      
     ) # End of column
     ) # End of fluidRow
      
    ), # End of tabItem
 
     tabItem(tabName = "restIndex",
         
         fluidRow(
           column(width = 12,
                  h2("Enablers of Rest"),
                  
                  box(width = 12,
                      column(width = 6,
                             plotOutput("rest_iv_contr_plot", height = 100)
                      ),
                      column(width = 6,
                             plotOutput("rest_iv_contr_stack_plot", height = 100)
                      )
                  ), # End of box
                  
                  h2("Rest Index Distribution"),
                  
                  # box(width = 12,
                  #   DT::dataTableOutput("play_table")
                  # ),
                  
                  box(width = 6,
                      plotOutput("rest_index_hist")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_index_boxplot")
                  ), # End of box
                  
                  h2("Rest Index Correlation With Criteria"),
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_1")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_2")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_3")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_4")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a1")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a2")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a3")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a4")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a5")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a6")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a7")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a8")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a9")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a10")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a11")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a12")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_test_criterion_plot_a13")
                  ) # End of box
                  
                  
           ) # End of column
         ) # End of fluidRow
         
      ), # End of tabItem
 
    tabItem(tabName = "workIndex",
         
         fluidRow(
           column(width = 12,
                  h2("Enablers of Work"),
                  
                  box(width = 12,
                      column(width = 6,
                             plotOutput("work_iv_contr_plot", height = 100)
                      ),
                      column(width = 6,
                             plotOutput("work_iv_contr_stack_plot", height = 100)
                      )
                  ), # End of box
                  
                  h2("Work Index Distribution"),
                  
                  # box(width = 12,
                  #   DT::dataTableOutput("play_table")
                  # ),
                  
                  box(width = 6,
                      plotOutput("work_index_hist")
                  ), # End of box
                  box(width = 6,
                      plotOutput("work_index_boxplot")
                  ), # End of box
                  
                  h2("Work Index Correlation With Criteria"),
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_1")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_2")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_3")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_4")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a1")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a2")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a3")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a4")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a5")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a6")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a7")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a8")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a9")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a10")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a11")
                  ), # End of box
                  
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a12")
                  ), # End of box
                  box(width = 6,
                      plotOutput("rest_test_criterion_plot_a13")
                  ) # End of box
                  
                  
           ) # End of column
         ) # End of fluidRow
         
      ), # End of tabItem
    
    tabItem(tabName = "map",
            
      tabBox(
        side = "left", height = "900px", width = 12,
        selected = "Play",
        tabPanel("Play",
                 h2("Play Index County Map (Quintiles)"),
                 box(width = 12,
                     plotOutput("play_map", height = "725px")
                 ) # End of box
        ), # End of tabPanel
        tabPanel("Rest",
                 h2("Rest Index County Map (Quintiles)"),
                 box(width = 12,
                     plotOutput("rest_map", height = "725px")
                 ) # End of box        
        ),  # End of tabPanel
        tabPanel("Work",
                 h2("Work Index County Map (Quintiles)"),
                 box(width = 12,
                     plotOutput("work_map", height = "725px")
                 ) # End of box        
        ), # End of tabPanel
        tabPanel("Opportunity to Thrive Index",
                 h2("Opportunity to Thrive Index"),
                 box(width = 12,
                     plotOutput("cross_index_map", height = "725px")
                 ) # End of box        
        ) # End of tabPanel 
      ) # End of tabBox
      
    ) # End of tabItem
    
  ) # End of tabItems
) # End of dashboardBody


#  Call dashboardPage
dashboardPage(
  header,
  sidebar,
  body
)
