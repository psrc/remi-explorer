function(input, output, session) {
  
  get_delta <- function(dat){
    dat[, value := c(NA, diff(value)), by = c("Source", "category", "variable")]
    dat[, difyear := c(NA, diff(year)), by = c("Source", "category", "variable")]
    dat[, value := value/difyear][, difyear := NULL]
  }
  
  get_percent_delta <- function(dat){
    dat[, value := c(NA, 100*(exp(diff(log(value)))-1)), by = c("Source", "category", "variable")]
    dat[, difyear := c(NA, diff(year)), by = c("Source", "category", "variable")]
    dat[, value := value/difyear][, difyear := NULL]
  }
  
  get_moving_average <- function(dat){
    dat[, value := (value - shift(value, n = 5))/5, by = c("Source", "category", "variable")]
    #dat[!startsWith(Source, "LUV")]
  }
  
  
  variables <- reactive({
    # variable and alias list
    # vars.subset is read in global.R
    t <- variables.lu[category %in% input$category, ]
    v.raw <- as.list(unique(t$variable))
    v.list <- setNames(v.raw, as.list(unique(t$variable_name)))
  })
  
  geography <- reactive({input$tabset})
  
  output$var <- renderUI({
    div(style = "width: 90%; float:left;",
        selectInput('variable',
                    label = 'Variable',
                    choices = variables(),
                    selected = variables()[1])
    )
  })

  get_data <- function(cat, var, geo, measure, source){
    dat <- alldata.long[category %in% cat & variable %in% var & Source %in% source][order(year)]
    data <- dat[Region == if(is.null(geo)) "Region" else geo, 
                .(value = sum(value)), by = .(Source, category, variable, year)]
    if(nrow(data) == 0) return(NULL)
    data <- switch(measure,
                   'total' = data,
                   'delta' = get_delta(data),
                   "percent_delta" = get_percent_delta(data),
                   "moving_average" = get_moving_average(data)
                  )
    data
  }
    get_table <- reactive({
      get_data(input$category, input$variable, geography(), input$visopt, input$datasource)
    })
    
    text <- reactive({
      desc <- switch(input$visopt,
                     'total' = 'Counts',
                     'delta' = 'Annual Change',
                     "percent_delta" = 'Percent Annual Change',
                     "moving_average" = '5y moving average')
      alias <- variables.lu[variable == input$variable, .(variable_name)]
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
    
    plot_trends <- function(data, title, line_width = 1, point_size = 1.5){
      if(is.null(data)) return(NULL)
      #interactive_line_chart(data, x = "year", y = "value", est = "number", fill = "Source", title = title)
      g <- static_line_chart(data, x = "year", y = "value", est = "number", fill = "Source", lwidth = line_width) + 
        scale_colour_discrete(drop=TRUE, limits = levels(data$Source)) + geom_point(size = point_size, aes(color = Source))
      make_interactive(g, title = title)
    }
    output$plot_region <- renderPlotly({
      plot_trends(get_table(), text()$title)
    })
    
    output$plot_king <- renderPlotly({
      plot_trends(get_table(), text()$title)
    })
    
    output$plot_snohomish <- renderPlotly({
      plot_trends(get_table(), text()$title)
    })
    
    output$plot_pierce <- renderPlotly({
      plot_trends(get_table(), text()$title)
    })
    
    output$plot_kitsap <- renderPlotly({
      plot_trends(get_table(), text()$title)
    })
    
    output$plot_nation <- renderPlotly({
      plot_trends(get_table(), text()$title)
    })
    
    output$plot_4counties <- renderPlotly({
      f1 <- plot_trends(get_data(input$category, input$variable, "King", input$visopt, input$datasource), 
                        "King", line_width = 0.3, point_size = 0.5)
      f2 <- plot_trends(get_data(input$category, input$variable, "Kitsap", input$visopt, input$datasource), 
                        "Kitsap", line_width = 0.3, point_size = 0.5)
      f3 <- plot_trends(get_data(input$category, input$variable, "Pierce", input$visopt, input$datasource), 
                        "Pierce", line_width = 0.3, point_size = 0.5)
      f4 <- plot_trends(get_data(input$category, input$variable, "Snohomish", input$visopt, input$datasource), 
                        "Snohomish", line_width = 0.3, point_size = 0.5)
      subplot(f1, 
              style(f2, showlegend = FALSE), 
              style(f3, showlegend = FALSE),
              style(f4, showlegend = FALSE),
              nrows = 2, shareX = TRUE, margin = 0.07)
    })
    
    output$afooter <- renderUI({
      mission <- "PSRC’s mission is to ensure a thriving central Puget Sound, now and into the future, 
      through planning for regional transportation, growth management and economic development."
      
      bs4Jumbotron(
        title = strong(div(class="footer_title", "About PSRC")),
        lead = div(class="footer_mission",  mission),
        
        a(class = "footer_url", href="https://www.facebook.com/PugetSoundRegionalCouncil", icon("facebook"), target="_blank"),
        a(class = "footer_url", href="https://twitter.com/SoundRegion", icon("twitter"), target="_blank"),
        a(class = "footer_url", href="https://www.instagram.com/soundregion/", icon("instagram"), target="_blank"),
        a(class = "footer_url", href="https://www.linkedin.com/company/soundregion", icon("linkedin"), target="_blank"),
        
        status = "info",
        btnName = strong(div(class="footer_title", "Connect with PSRC", icon("envelope"))),
        href = "mailto:info@psrc.org?"
      )
      
    })
    

  
}
