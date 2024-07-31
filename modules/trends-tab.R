# Display UI for trends tab

trends_tab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    banner_ui('trendsBanner', 
              photo_filename = psrc_photos[sample.int(length(psrc_photos), 1)], 
              banner_title = "REMI Explorer", 
              banner_subtitle = "Compare REMI scenarios"),
    
    div(style = 'margin: 3rem 5rem;',
        fluidRow(
          column(width = 3,
                 trends_widgets_ui(ns('trends')),
                 
                 # if visual tab is clicked, display radio button selection
                 #conditionalPanel(paste0("input['", ns("tabset"), "'] == 'v'"),
                                  div(style = 'margin: 3rem 0',
                                      radioButtons(ns('visopt'),
                                                   label = 'Visual Options',
                                                   choices = dtype.choice.stab.vis
                                      ))
          ), # end column
          column(width = 9,
                 tabsetPanel(id = ns('tabset'),
                             type = 'pills',
                             #tabPanel('Table',
                             #         value = 't',
                             #         trends_table_ui(ns('table'))
                             #),
                             tabPanel('Visual',
                                      value = 'v',
                                      trends_plot_ui(ns('plot'))
                                      
                             )
                             
                 ) # end tabsetPanel
                 
          ) # end column
        ) # end fluidRow
    ) # end div
    
  )
  
}

trends_tab_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    vals <- reactiveValues(var = NULL)
    
    observeEvent(input$`trends-go`, {
      vals$var <- input$`trends-variable`
    })
    
    trends_widgets_server('trends')
    
    d <- eventReactive(input$`trends-go`, {
      trends_data_server('trendsData', 
                         go = input$`trends-go`, 
                         trend_var = input$`trends-variable`,
                         geography = input$`trends-geography`,
                         #subgeography = input$`trends-subgeography`
                         visoption = reactive(input$visopt)
                         )
    })
    
    # observeEvent(input$`trends-go`, {
    #   trends_table_server('table', 
    #                       go = input$`trends-go`, 
    #                       trendtable = d()$table, 
    #                       alias = d()$alias, 
    #                       geography = input$`trends-geography`,
    #                       subgeography = input$`trends-subgeography`
    #                       )
    # })
    
    trends_plot_server('plot',
                       go = input$`trends-go`,
                       trendtable= reactive(d()$tablevis),
                       trend_var = reactive(input$`trends-variable`),
                       alias = reactive(d()$alias),
                       geography = reactive(input$`trends-geography`),
                       #subgeography = reactive(input$`trends-subgeography`),
                       visoption = reactive(input$visopt),
                       valsvar = reactive(vals$var))
    
  }) # end moduleServer
  
}