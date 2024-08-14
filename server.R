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
  geography_pyr <- reactive({input$tabset_pyr})
  
  output$var <- renderUI({
    div(style = "width: 100%; float:left;",
        selectInput('variable',
                    label = 'Variable',
                    choices = variables(),
                    selected = variables()[1])
    )
  })

  get_data <- function(cat, var, geo, measure, source){
    dat <- alldata.trends[category %in% cat & variable %in% var & Source %in% source][order(year)]
    data <- dat[Region == if(is.null(geo)) "Region" else geo, 
                .(value = sum(value)), by = .(Source, category, variable, year)]
    if(nrow(data) == 0) return(NULL)
    data <- switch(measure,
                   'total' = data,
                   'delta' = get_delta(data),
                   "percent_delta" = get_percent_delta(data),
                   "moving_average" = get_moving_average(data)
                  )
    # add tooltip
    data$tooltip <- paste0(data$Source, ": ", 
                           prettyNum(formattable::digits(round(data$value, 0), digits=0), 
                                     big.mark = ","), " (", data$year, ")")
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
      
      return(list(title = title))
    })
    
    plot_trends <- function(data, title, line_width = 1, point_size = 1.5, breaks.by = 5){
      if(is.null(data)) return(NULL)
      minx <- all.xvalues[cut(min(data$year), all.xvalues, labels = FALSE, include.lowest = TRUE, right = FALSE)]
      #interactive_line_chart(data, x = "year", y = "value", est = "number", fill = "Source", title = title)
      g <- static_line_chart(data, x = "year", y = "value", est = "number", fill = "Source", lwidth = line_width,
                             text = "tooltip", breaks = seq(minx, max(data$year), by = breaks.by)
                             ) + 
        scale_colour_discrete(drop=TRUE, limits = levels(data$Source)) + 
        geom_point(size = point_size, aes(color = Source)) 
        
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
                        "King", line_width = 0.3, point_size = 0.5, breaks.by = 10)
      f2 <- plot_trends(get_data(input$category, input$variable, "Kitsap", input$visopt, input$datasource), 
                        "Kitsap", line_width = 0.3, point_size = 0.5, breaks.by = 10)
      f3 <- plot_trends(get_data(input$category, input$variable, "Pierce", input$visopt, input$datasource), 
                        "Pierce", line_width = 0.3, point_size = 0.5, breaks.by = 10)
      f4 <- plot_trends(get_data(input$category, input$variable, "Snohomish", input$visopt, input$datasource), 
                        "Snohomish", line_width = 0.3, point_size = 0.5, breaks.by = 10)
      subplot(f1, 
              style(f2, showlegend = FALSE), 
              style(f3, showlegend = FALSE),
              style(f4, showlegend = FALSE),
              nrows = 2, shareX = TRUE, margin = 0.07)
    })
    
    output$afooter <- renderUI({
      mission <- "PSRC’s mission is to ensure a thriving central Puget Sound, now and into the future, 
      through planning for regional transportation, growth management and economic development."
      
      #bs4Jumbotron(
      jumbotron(
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
    
    # Functions for plotting pyramids
    
    output$yearUI <- renderUI({
      sliderInput('year', 'Year:', sep="",
                  #animate=animationOptions(interval = 1000),
                  min=alldata.pyramid[, min(year)], 
                  max=max(all.xvalues), value = 2023, step=1)
    })
    
    pyramid_text <- reactive({
      title <- paste("Population in ", input$year)
      return(list(title = title))
    })
    
    pyramid_geo_text <- function(geo, title){
      return(paste(geo, title))
    }
    
    get_pyramid_data <- function(yr, geo, source){
      if(is.null(yr)) yr <- 2023
      if(is.null(geo)) geo <- "Region"
      data <- alldata.pyramid[year %in% yr & 
                             category %in% "Population" & 
                             Region %in% geo &
                             Source %in% source][order(lower.age.limit)]
      if(nrow(data) == 0) return(NULL)
      #dat <- rbind(copy(data)[, Gender := "Male"], copy(data)[, Gender := "Female"])
      data[Gender == "Male", value := -value]
      return(data)
    }
    
    get_pyramid_table <- reactive({
      get_pyramid_data(input$year, geography_pyr(), input$datasource_pyr)
    })
    
    plot_pyramid <- function(data, title, line_width = 1){
      if(is.null(data)) return(NULL)
      data[, text := paste0(Source, ": ", round(abs(value)), " (", Age, ")")]
      data.range <- range(abs(data$value), na.rm=TRUE)
      grps <- levels(data$Source)
      num.grps <- length(grps)
      l.colors <- unlist(psrcplot::psrc_colors["gnbopgy_5"])
      l.colors <- l.colors[1:num.grps]
      cols <- stats::setNames(l.colors, grps)
      #browser()
      g <- ggplot(data, aes(y=value, x=reorder(Age, lower.age.limit), group=Source, colour=Source, text = text)) + 
        geom_line(data=subset(data, Gender=='Female'), linewidth=line_width) + geom_line(data=subset(data, Gender=='Male'), linewidth=line_width) + 
        geom_point(data=subset(data, Gender=='Female'), size = 0.1) + geom_point(data=subset(data, Gender=='Male'), size = 0.1) + 
        scale_x_discrete(name="") + scale_y_continuous(labels=function(x)abs(x), name="") + 
        coord_flip() + theme(legend.title=element_blank()) 
      g <- g + geom_text(data=NULL, y=-data.range[2]/2, x=length(unique(data$mid.age))-1, label="Male", colour='black')
      g <- g + geom_text(data=NULL, y=data.range[2]/2, x=length(unique(data$mid.age))-1, label="Female", colour='black')
      #browser()
      g <- g + geom_hline(yintercept = 0) + scale_color_manual(values=cols) + 
        scale_colour_discrete(drop=TRUE, limits = levels(data$Source)) + psrc_style_modified()
      make_interactive(g, title = title) 
    }
    output$plot_pyramid_region <- renderPlotly({
        plot_pyramid(get_pyramid_table(), pyramid_text()$title)
    })
    output$plot_pyramid_king <- renderPlotly({
      plot_pyramid(get_pyramid_table(), pyramid_text()$title)
    })
    output$plot_pyramid_pierce <- renderPlotly({
      plot_pyramid(get_pyramid_table(), pyramid_text()$title)
    })
    output$plot_pyramid_snohomish <- renderPlotly({
      plot_pyramid(get_pyramid_table(), pyramid_text()$title)
    })
    output$plot_pyramid_kitsap <- renderPlotly({
      plot_pyramid(get_pyramid_table(), pyramid_text()$title)
    })
    
    output$plot_pyramid_nation <- renderPlotly({
      plot_pyramid(get_pyramid_table(), pyramid_text()$title)
    })
    
    output$plot_pyramid_4counties <- renderPlotly({
      f1 <- plot_pyramid(get_pyramid_data(input$year, "King", input$datasource_pyr), 
                         pyramid_geo_text("King", pyramid_text()$title), line_width = 0.5)
      f2 <- plot_pyramid(get_pyramid_data(input$year, "Kitsap", input$datasource_pyr), 
                         pyramid_geo_text("Kitsap", pyramid_text()$title), line_width = 0.5)
      f3 <- plot_pyramid(get_pyramid_data(input$year, "Pierce", input$datasource_pyr), 
                         pyramid_geo_text("Pierce", pyramid_text()$title), line_width = 0.5)
      f4 <- plot_pyramid(get_pyramid_data(input$year, "Snohomish", input$datasource_pyr), 
                         pyramid_geo_text("Snohomish", pyramid_text()$title), line_width = 0.5)
      if(is.null(f1)) return(NULL)
      subplot(f1, 
              style(f2, showlegend = FALSE), 
              style(f3, showlegend = FALSE),
              style(f4, showlegend = FALSE),
              nrows = 2, shareX = FALSE, margin = 0.07)
    })
    
}
