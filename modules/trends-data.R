# Returns trend data (to be used with Trends plot and table)

trends_data_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList()
  
}

get_delta <- function(dat){
  dat[, value := c(NA, diff(value)), by = c("Source", "category", "variable")]  
}

get_percent_delta <- function(dat){
  dat[, value := c(NA, 100*(exp(diff(log(value)))-1)), by = c("Source", "category", "variable")]  
}

get_moving_average <- function(dat){
  dat[, value := (value - shift(value, n = 5))/5, by = c("Source", "category", "variable")]  
}



trends_data_server <- function(id, go, trend_var, geography, visoption) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    alias <- eventReactive(go, {
      # return the variable's alias (all available years)
      alias <- variables.lu[variable %in% trend_var, .(variable_name)]
      unique(alias$variable_name)
    })
    
    tabledata <- eventReactive(go, {
      # identify table type (t, p, h), table name

      #v <- variables.lu[variable %in% trend_var, ]
      # t <- unique(v$table_name)
      # table_name <- table_names[[t]]
      # dtypes <- as.vector(unique(v$dtype))
      # 
      # ifelse('fact' %in% dtypes,  type <- 'fact', type <- 'dimension')
      # return(list(Type=type, Table_Name=table_name))
      dat <- alldata.long[variable %in% trend_var][order(year)]
      return(dat)
    } )
    
    trendtable <- eventReactive(go, {
      # return list of tables subsetted by value types
      
        data <- tabledata()
      # wt_field <- tabletype()$Weight_Name
      # table_name <- tabletype()$Table_Name
      # type <- tabletype()$Type
      # 
      # # collect data for available years
      # survey_years <- varyears()
      # data <- map(survey_years, ~get_hhts(survey = .x, level = table_name, vars = c("sample_county", "final_home_jurisdiction", "seattle_home", trend_var)))
      # walk(data, ~setDT(.x))
      
      # filter for home county when county is selected
        data <- data[Region == geography, .(value = sum(value)), by = .(Source, category, variable, year)]
        data <- switch(visoption(),
                       'total' = data,
                       'delta' = get_delta(data),
                       "percent_delta" = get_percent_delta(data),
                       "moving_average" = get_moving_average(data)
                      )
      return(data)
    })
    
    trendtable_dt <- eventReactive(go, {
      # create a version with prepped column headers for DT
      
      t <- copy(trendtable())
      # a <- alias()
      # 
      # dtypes <- dtype.choice.stab
      # selcols <- c(a, names(dtypes))
      # 
      # setnames(t, c('survey', trend_var, dtypes), c('Survey', selcols))
      # setcolorder(t, c('Survey', a, selcols[which(selcols != a)]))
      # 
      # dt <- t[!(base::get(eval(a)) %in% "")][, !('value_order')]
      return(t)
    })
    return(list(table = trendtable_dt(), tablevis = trendtable()))
    #return(list(table = trendtable_dt(), tablevis = trendtable(), tabletype = tabletype(), val = values(), alias = alias()))

  }) # end moduleServer
  
}