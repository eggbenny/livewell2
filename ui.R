# ui.R
# Benedito Chou
# Dec 5 2020


# --- ui --------------------------------------------------

# --- Header
header <- dashboardHeader(title = "LiveWell Prototype Demo")

sidebar <- dashboardSidebar(
    
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", 
             icon = icon("dashboard"),
             badgeLabel = "Demo", badgeColor = "green"),
    menuItem("Data Table", icon = icon("th"),
             tabName = "dataTable"),
    menuItem("Play Index", icon = icon("bar-chart-o"),
             tabName = "playIndex")
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
                    infoBoxOutput("pop_impact", width = 12)
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
    column(width = 6,   
      box(width = 12,  
          plotOutput("test_grid_plot")
     ) # End of box
    ),
      
      column(width = 3,
          
        box(width = 12,
          column(width = 6,
          selectizeInput("state", "Select a state",
            choices = sort(unique(ana_data_1_wgeo$state)),
            selected = "North Carolina")
          ),
          
          column(width = 6,
          selectizeInput("county", "Select a county",
            choices = sort(unique(ana_data_1_wgeo$county)),
            selected = "Richmond")
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
          selectizeInput("iv_top5", "Select a measure",
            choices = c("Select a Measure", measure_top5_lst),
            selected = measure_top5_lst[1]),
            
          sliderInput("change_top5",
            "If I change the selected top 5 measure by 1% ... ",
            min = -20,
            max = 20,
            value = 0)
          ),
          
          column(width = 12,
            selectizeInput("iv", "Select a measure",
              choices = c("Select a Measure", measure_lst),
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

    tabItem(tabName = "dataTable",
            
      h2("Weight (b) table from Stepwise Regression"),
      
      box(width = 12,
        DT::dataTableOutput("test_table")
      ) # End of box
      
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
      ) # End of box
     ) # End of column
     ) # End of fluidRow
      
    ) # End of tabItem
    
  ) # End of tabItems
) # End of dashboardBody


#  Call dashboardPage
dashboardPage(
  header,
  sidebar,
  body
)
