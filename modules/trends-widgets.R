# Display widgets for the trends tab. Display categories and render the appropriate variables for chosen category.
# !categories can differ across surveys!

trends_widgets_ui <- function(id) {
  ns <- NS(id)
  #vars.cat <- variables.lu
  
  tagList(
    selectInput(ns('category'),
                label = 'Category',
                choices = vars.cat, # list all categories available from variables table (regardless of survey)
                ),
    uiOutput(ns('var'))#, 
    #uiOutput(ns('icon')),
    #selectInput(ns('geography'),
    #            label = 'Geography',
    #            choices = c('Region', 'Nation', 'King', 'Kitsap', 'Pierce', 'Snohomish')),
    #uiOutput(ns('subgeog')),
    
    #actionButton(ns('go'),
    #             label = 'Enter')
  )
  
}

trends_widgets_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    variables <- reactive({
      # variable and alias list
      # vars.subset is read in global.R
      t <- variables.lu[category %in% input$category, ]#[variable %in% vars.subset$variable]
      #t<-t%>%filter(survey_year==max(t$survey_year))
      v.raw <- as.list(unique(t$variable))
      v.list <- setNames(v.raw, as.list(unique(t$variable_name)))
    })
    
    output$var <- renderUI({
     div(style = "width: 90%; float:left;",
      selectInput(ns('variable'),
                  label = 'Variable',
                  choices = variables(),
                  selected = variables()[1])
     )
    })
    
    # output$icon <- renderUI({
    #   
    #   tooltip(icon('circle-info', style="padding: 0px; font-size: 15px"),
    #           title = variable_desc())
    #   
    # })
    
    variable_desc <- eventReactive(input$variable, {
        return(NULL)
      if(is.null(input$variable)) return(NULL)
      unique(variables.lu[variable == input$variable, .(detail)])
    })
    
    
    
  }) # end moduleServer
  
}