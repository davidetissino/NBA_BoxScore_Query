library(shiny)
library(dplyr)
library(DT)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(bslib)

# APP ####

# read in the csvs
rs_tot <- read.csv('./datasets/Regular_Season_Logs.csv')
p_offs <- read.csv('./datasets/Playoff_Logs.csv')

# customize loader 
options(spinner.color = "firebrick", spinner.color.background = "floralwhite", spinner.size = 1)

# UI ####

ui <- page_fluid(
  
  ## Custom Card Headers ----
  ### Title Header ====
  tags$style(
    ".custom-bg-title {
       background-color: #112446;
       color: #fff; /* text color */
       /* Can add any other styling */
       # padding: 20px; /* Optional: Adjust padding as needed */
       # border-radius: 10px; 
     }"
  ),
  ### Conditions1 Header ====
  tags$style(
    ".custom-bg-RS {
       background-color: #C1CDCD;
       color: #000; /* text color */
       /* Can add any other styling */
       # padding: 20px; /* Optional: Adjust padding as needed */
       # border-radius: 10px; 
     }"
  ),
  ### Conditions Header ====
  tags$style(
    ".custom-bg-PO {
       background-color: #C1CDCD;
       color: #000; /* text color */
       /* Can add any other styling */
       # padding: 20px; /* Optional: Adjust padding as needed */
       # border-radius: 10px; 
     }"
  ),
  
  title = 'NBA Box Scores Query',
  br(),
  card(
    card_header(
      class = 'custom-bg-title',
      strong('NBA Box Scores Query', style = 'font-family:Helvetica;font-size:35px')
    ),
    
    # Intro Panel ####
    card_body(
      
      p(
        'The tables contain NBA ', 
        strong('Regular Season & Playoffs'),
        ' individual box scores. Data available from ', 
        strong('1946-47.'),
        br(), 
        style = 'font-family:Helvetica; font-size:22px'
      ),
      
      p(
        strong("By: "),
        a("@davidetissino", href = "https://twitter.com/dvdtssn", target = "_blank"),
        ' | ', 
        a('davidetissino.com', href = 'https://davidetissino.com', target = '_blank'),
        style = "font-size:16px;font-family:Helvetica; color:black;"
        
        ), 
    ), 
    
    ## How To Use ####
    card(
      card_body(
        p(
          strong('How to use:', style = 'font-family:Helvetica; font-size:20px; color:#104E8B'),
          br(),
          'In the', 
          strong('Filters', style = 'font-family:Helvetica;font-size:17px'), 
          'box write down the stat, the operator (>, >=, ==, <=, <<) and the value. You can use "," to include multiple conditions.  
        Hit the ', strong('Go!', style = 'font-family:Helvetica;font-size:17px'), 'button to update the table',
          br(),
          strong('Available stats:', style = 'font-size:17px'), 
          'SEASON, PTS, OREB, DREB, REB, AST, STL, BLK, TOV, PF, pm (+/-), FGM, FGA, FG_PCT, FG3M, FG3A, FG3_PCT, FTM, FTA, FT_PCT', 
          br(), 
          '→', 
          strong(' Example:', style = 'font-size:17px; color:#EE5C42'),
          'writing the query "PTS >= 70, REB > 5, TOV < 4, FG_PCT > 60" in the ',
          'Filters box results in',
          strong("Kobe's", style = 'font-size:17px'), 
          'legendary', 
          strong('81', style = 'font-size:17px'),
          'points game. Pay attention to spacing',
          style = 'font-family: Helvetica; font-size:18px', 
        )
      )
    )
  ),
  
  # Tabset Panel ####
  navset_card_tab(
    ## RS PANEL #####
    nav_panel(
      strong('Regular Season', style = 'font-family:Helvetica;color:#000080; font-size:17px'),
      card(
        card_header(
          strong('Conditions:', style = 'font-family:Helvetica; font-size:17px'),
          class = 'custom-bg-RS'),
        
        ### Selectors ----
        fluidRow(
          #### Player ====
          column(
            4, 
            uiOutput('sel_rs_player')
          ), 
          #### Text ====
          column(
            6, 
            uiOutput('rs_filter_conditions')
          ), 
          #### Button ====
          column(
            2, 
            br(),
            actionButton("rs_search_button", strong("Go!", style = 'font-size:25px;font-family:Avenir Next'),
                         style="color: #fff; background-color: firebrick; border-color: black; width: 100%;")
          )
        )
      ), 
      
      ### Table ----
      fluidRow(
        column(
          12,
          strong(paste0('Latest update: April 14, 2024')),
          style = "font-size:13px;font-family:Helvetica; color:#EE0000"),
        
        div(
          
          withSpinner(dataTableOutput('rs_boxscores'), type = 1), 
          style = 'font-size:14px'
          
        )
      )
    ),
    
    nav_panel(
      ## PO PANEL ####
      strong('Playoffs', style = 'font-family:Helvetica;color:#000080; font-size:17px'),
      
      card(
        card_header(
          strong('Conditions:', style = 'font-family:Helvetica; font-size:17px'), 
          class = 'custom-bg-PO'),
        ### Selectors ----
        fluidRow(
          #### Player ====
          column(
            4, 
            uiOutput('sel_po_player'),
            
          ), 
          #### Text ====
          column(
            6, 
            uiOutput('po_filter_conditions'),
            
          ), 
          #### Button ====
          column(
            2, 
            br(),
            actionButton("po_search_button", strong("Go!", style = 'font-size:25px;font-family:Avenir Next'),
                         style="color: #fff; background-color: firebrick; border-color: black; width: 100%;")
          )
        )
      ),
      
      ### Table ----
      fluidRow(
        column(
          12,
          strong('Latest update: ', format.Date(Sys.Date(), '%B %d, %Y'),
                 style = "font-size:13px;font-family:Helvetica; color:#EE0000"),
          
          div(
            
            withSpinner(dataTableOutput('po_boxscores'), type = 1), 
            
            style = 'font-size:14px'
            
          )
        )
      )
    )
  )
)




# SERVER ####


server <- function(input, output, session) {
  
  # Initialize reactive values with unfiltered data
  filtered_rs_data <- reactiveVal(rs_tot)
  filtered_po_data <- reactiveVal(p_offs)
  
  #### RS PANEL ####
  
  ##### Player Selector ----
  output$sel_rs_player <- renderUI({
    selectInput(
      "rs_player",
      strong("Player(s)", style = 'font-size:16px;font-family:Helvetica'),
      choices = c('All', unique(sort(rs_tot$PLAYER))),
      multiple = TRUE,
      selected = 'All', 
      width = '130%'
    )
  })
  
  ##### Text & Button ----
  output$rs_filter_conditions <- renderUI({
    fluidRow(
      column(
        12,
        textInput("rs_filter_conditions", 
                  strong("Filters", style = 'font-size:16px;font-family:Helvetica'), width = '200%')
      )
    )
  })
  
  ##### Text Filter ----
  observeEvent(input$rs_search_button, {
    filter_conditions <- strsplit(as.character(input$rs_filter_conditions), ", ")[[1]]
    filter_conditions <- lapply(strsplit(filter_conditions, " "), trimws)
    
    rs_data <- rs_tot
    
    if (!is.null(filter_conditions) && length(filter_conditions) > 0) {
      for (condition in filter_conditions) {
        if (length(condition) != 3) {
          return()
        }
        
        var <- condition[1]
        operator <- condition[2]
        value <- condition[3]
        rs_data <- subset(rs_data, eval(parse(text = paste(var, operator, value))))
      }
    }
    
    # Filter data based on selected player(s)
    if (!is.null(input$rs_player) && input$rs_player != 'All') {
      rs_data <- rs_data[rs_data$PLAYER %in% input$rs_player, ]
    }
    
    filtered_rs_data(rs_data)
  })
  
  ##### RS Table ----
  output$rs_boxscores <- renderDataTable({
    datatable(
      filtered_rs_data(), 
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#191970', 'color': '#fff'});",
          "}"),
        paging = TRUE, 
        pageLength = 10,  
        scrollX = TRUE,   
        scrollY = TRUE,  
        autoWidth = TRUE, 
        server = FALSE,
        columnDefs = list(
          list(targets = '_all', className = 'dt-center'), 
          list(targets = c(0), visible = FALSE)
        ),
        dom = 'Bfrtip', 
        searching = FALSE),
      
      filter = 'none', 
      rownames = FALSE, 
      selection = 'none')
  }) 
  
  
  #### PO PANEL ####
  
  ##### Player Selector ----
  output$sel_po_player <- renderUI({
    selectInput(
      "po_player",
      strong("Player(s)", style = 'font-size:16px;font-family:Helvetica'),
      choices = c('All', unique(sort(p_offs$PLAYER))),
      multiple = TRUE,
      selected = 'All',
      width = '130%'
    )
  })
  
  ##### Text & Button ----
  output$po_filter_conditions <- renderUI({
    fluidRow(
      column(
        12,
        textInput("po_filter_conditions", strong("Filters", style = 'font-size:16px;font-family:Helvetica'), width = '200%')
      )
    )
  })
  
  ##### Text Filter ----
  observeEvent(input$po_search_button, {
    filter_conditions <- strsplit(as.character(input$po_filter_conditions), ", ")[[1]]
    filter_conditions <- lapply(strsplit(filter_conditions, " "), trimws)
    
    data <- p_offs
    
    if (!is.null(filter_conditions) && length(filter_conditions) > 0) {
      for (condition in filter_conditions) {
        if (length(condition) != 3) {
          return()
        }
        
        var <- condition[1]
        operator <- condition[2]
        value <- condition[3]
        data <- subset(data, eval(parse(text = paste(var, operator, value))))
      }
    }
    
    # Filter data based on selected player(s)
    if (!is.null(input$po_player) && input$po_player != 'All') {
      data <- data[data$PLAYER %in% input$po_player, ]
    }
    
    filtered_po_data(data)
  })
  
  ##### PO Table ----
  output$po_boxscores <- renderDataTable({
    datatable(
      filtered_po_data(),
      options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#191970', 'color': '#fff'});",
          "}"),
        paging = TRUE,
        pageLength = 10,
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = TRUE,
        server = FALSE,
        columnDefs = list(
          list(targets = '_all', className = 'dt-center'),
          list(targets = c(0), visible = FALSE)
        ),
        dom = 'Bfrtip',
        searching = FALSE),
      
      filter = 'none',
      rownames = FALSE,
      selection = 'none')
  })
  
}




shinyApp(ui, server)
