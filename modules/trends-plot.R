# Display trends as a plot

trends_plot_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    # render static psrcplot
    uiOutput(ns('plotui'))
  )
  
}


trends_plot_server <- function(id, trendtable, trend_var, alias, geography, subgeography = NULL, visoption, valsvar
                               ) {
  
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
    
    get_table <- reactive({
      #trendtable()
      dat <- alldata.long[variable %in% trend_var()][order(year)]
      data <- dat[Region == if(is.null(geography())) "Region" else geography(), 
                  .(value = sum(value)), by = .(Source, category, variable, year)]
      if(nrow(data) == 0) browser()
      data <- switch(visoption(),
                     'total' = data,
                     'delta' = get_delta(data),
                     "percent_delta" = get_percent_delta(data),
                     "moving_average" = get_moving_average(data)
      )
      data
    })

    text <- reactive({
      desc <- switch(visoption(),
                     'total' = 'Counts',
                     'delta' = 'Annual Change',
                     "percent_delta" = 'Percent Annual Change',
                     "moving_average" = '5y moving average')
      alias <- variables.lu[variable %in% trend_var(), .(variable_name)]
      title <- paste(desc, 'of', unique(alias$variable_name))
      
      x <- NULL
      y <- geography()
      if(is.null(y)) y <- "Region"

        if(y == 'Region') g <- 'Regional'
        else {
            if(y == 'Nation') g <- 'National'
            else g <- paste(y, 'County')
        }
      
      subtitle <- paste(g, 'results')
      
      return(list(title = title, subtitle = subtitle))
    })
    
    
    output$plot <- renderPlotly({
        dat <- get_table()
        interactive_line_chart(dat, x = "year", y = "value",
                          est = "number", fill = "Source", title = text()$title)
    })
    
    
  }) # end moduleServer
  
}