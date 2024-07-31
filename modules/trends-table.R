# Display trends table as DT

trends_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('tableui'))
  )
  
}

trends_table_server <- function(id, go, trendtable, alias, geography, subgeography = NULL) {
 
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$tableui <- renderUI({
      go
      
      div(
        withSpinner(
          DTOutput(ns('table')),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )

    })
    
    clean_table <- reactive({
      # clean Margin of Error columns and column/row reorder for DT
      
      dt <- trendtable
      
      col <- names(dtype.choice[dtype.choice %in% "MOE"])
      col2 <- names(dtype.choice[dtype.choice %in% "estMOE"])

      # round columns
      dt[, (col) := lapply(.SD, function(x) round(x*100, 1)), .SDcols = col
      ][, (col2) := lapply(.SD, function(x) prettyNum(round(x, 0), big.mark = ",", preserve.width = "none")), .SDcols = col2]
      
      # add symbols
      dt[, (col) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = col
      ][, (col2) := lapply(.SD, function(x) paste0("+/-", as.character(x))), .SDcols = col2]
      
      # format survey year column, reorder rows
      dt[, Survey := str_replace_all(Survey, "_", "/")]
      dt <- dt[order(Survey)]
      
      new.colorder <- c('Survey',
                        alias,
                        names(dtype.choice[dtype.choice %in% c("share")]),
                        col,
                        names(dtype.choice[dtype.choice %in% c("estimate")]),
                        col2,
                        names(dtype.choice[dtype.choice %in% c("sample_count")]))
      
      setcolorder(dt,  new.colorder)
      return(dt)
    })
    
    description <- reactive({
      if(geography %in% c('Region', 'Kitsap', 'Snohomish')) subgeography <- NULL
      
      if(is.null(subgeography)) {
        if(geography == 'Region') g <- 'Regional Results'
        if(geography == 'Kitsap' | geography == 'Snohomish') g <- paste(geography, 'County Results')
      } else {
        if(geography != 'Region' & (subgeography != 'Region' && !is.null(subgeography))) g <- paste(geography, 'County:', subgeography, 'Results')
        if(geography != 'Region' & subgeography == 'Region') g <- paste(geography, 'County Results')
      }

      return(g)
    })
    
    output$table <- renderDT({
      # render DT with some additional column formatting
   
      colors <- list(ltgrey = '#bdbdc3', dkgrey = '#343439')
      
      dt <- clean_table()
      
      fmt.per <- names(dtype.choice[dtype.choice %in% c('share')])
      fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])

      DT::datatable(dt,
                    caption = description(),
                    options = list(autoWidth = FALSE,
                                   columnDefs = list(list(className = "dt-center", width = '100px', targets = c(2:ncol(dt))))
                    )
      ) %>%
        formatPercentage(fmt.per, 1) %>%
        formatRound(fmt.num, 0) %>%
        formatStyle(columns = 2:ncol(dt),
                    valueColumns = ncol(dt),
                    color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey)))
    })
    
  }) # end moduleServer
  
}