# Display trends as a plot

trends_plot_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    # render static psrcplot
    uiOutput(ns('plotui'))
  )
  
}

trends_plot_server <- function(id, go, trendtable, trend_var, alias, geography, subgeography = NULL, visoption, valsvar) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$plotui <- renderUI({
      
      div(
        withSpinner(
          plotlyOutput(ns('plot')),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
    })
    
    clean_table <- reactive({
      
      trendtable()
    })

    text <- reactive({
      desc <- switch(visoption(),
                     'total' = 'Total',
                     'delta' = 'Delta',
                     "percent_delta" = 'Percent Delta',
                     "moving_average" = '5y moving average')
      alias <- variables.lu[variable %in% trend_var(), .(variable_name)]
      title <- paste(desc, 'of', unique(alias$variable_name))

      #x <- isolate(subgeography())
      x <- NULL
      y <- isolate(geography())
      
      #if(y %in% c('Region', 'Kitsap', 'Snohomish')) x <- NULL

        if(y == 'Region') g <- 'Regional'
        else {
            if(y == 'Nation') g <- 'National'
            else g <- paste(y, 'County')
        }
      
      subtitle <- paste(g, 'results')
      
      return(list(title = title, subtitle = subtitle))
    })
    
    output$plot <- renderPlotly({
        dat <- clean_table()
        #dat[, year := as.character(year)]
        #browser()
        #echart_line_chart(dat, x = "year", y = "value",
        #                  est = "number", fill = "Source", title = text()$title)
        interactive_line_chart(dat, x = "year", y = "value",
                          est = "number", fill = "Source", title = text()$title)
        
      # static_column_chart(t = clean_table(),
      #                     x = valsvar(),
      #                     #y = settings()$p,
      #                     #moe = settings()$m,
      #                     #est = settings()$e,
      #                     #fill = 'survey',
      #                     title = text()$title,
      #                     subtitle = text()$subtitle,
      #                     source = 'REMI')
    })
    
    
  }) # end moduleServer
  
}