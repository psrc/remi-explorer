navbarPage(
  id = "my_id",
  tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
  
  # logo
  title = a(div(tags$img(src='psrc-logo.png',
                         style="margin-top: -30px; padding-left: 40px;",
                         height = "80")), 
            href="https://www.psrc.org", target="_blank"),
  
  # navbar height
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:25px !important; 
                            padding-bottom:0 !important;
                            height: 75px;
                            }
                           .navbar {min-height25px !important;}'))
  ),
  
  windowTitle = "PSRC REMI Explorer", 
  theme = "styles.css",
  position = "fixed-top",
  
  # tabs ----
 
  tabPanel(title = "Time Series", 
           fluidRow(column(3, style = 'padding-right:0px;', 
                           div(
                             img(src = psrc_photos[sample.int(length(psrc_photos), 1)], 
                                 width = "100%", 
                                 style = "padding-top: 0px; border-radius:0 0 30px 0;height:150px;")
                           )
           ),
           column(9, style = 'padding-left:0px;',
                  jumbotron(
                    title = strong(div(class="mainpage_title", "REMI Explorer")),
                    status = "success",
                    btnName = strong(div(class="mainpage_subtitle", "Compare REMI and other scenarios"))
                  )
           )
           ),
           div(style = 'margin: 3rem 5rem;',
               fluidRow(
                 column(width = 3,
                        selectInput('category',
                                    label = 'Category',
                                    choices = vars.cat, # list all categories available from variables table (regardless of survey)
                        ),
                        uiOutput('var'),
                        
                        div(style = 'margin: 3rem 0',
                            radioButtons('visopt',
                                         label = 'Measure',
                                         choices = dtype.choice
                            )),
                        div(style = "width: 90%; float:left;",
                            checkboxGroupInput('datasource',
                                         label = 'Source',
                                         choices = ordered.sources,
                                         selected = ordered.sources
                                         )
                            )
                 ), # end column
                 column(width = 9,
                        tabsetPanel(id = 'tabset',
                                    type = 'pills',
                                    selected = "Region",
                                    tabPanel('Region',
                                             value = 'Region',
                                             div(
                                               withSpinner(
                                                 plotlyOutput('plot'),
                                                 type = 5,
                                                 color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
                                               ),
                                               style = 'margin-top: 1rem'
                                             )
                                    ),
                                    tabPanel('All Counties',
                                             value = 'four_counties',
                                             div(
                                               withSpinner(
                                                 plotlyOutput('plot_4counties'),
                                                 type = 5,
                                                 color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
                                               ),
                                               style = 'margin-top: 1rem'
                                             )       
                                    ),
                                    tabPanel('King',
                                             value = 'King',
                                             div(
                                               withSpinner(
                                                 plotlyOutput('plot_king'),
                                                 type = 5,
                                                 color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
                                               ),
                                               style = 'margin-top: 1rem'
                                             )       
                                            ),
                                    tabPanel('Kitsap',
                                             value = 'Kitsap',
                                             div(
                                               withSpinner(
                                                 plotlyOutput('plot_kitsap'),
                                                 type = 5,
                                                 color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
                                               ),
                                               style = 'margin-top: 1rem'
                                             )       
                                    ),
                                    tabPanel('Pierce',
                                             value = 'Pierce',
                                             div(
                                               withSpinner(
                                                 plotlyOutput('plot_pierce'),
                                                 type = 5,
                                                 color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
                                               ),
                                               style = 'margin-top: 1rem'
                                             )       
                                    ),
                                    tabPanel('Snohomish',
                                             value = 'Snohomish',
                                             div(
                                               withSpinner(
                                                 plotlyOutput('plot_snohomish'),
                                                 type = 5,
                                                 color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
                                               ),
                                               style = 'margin-top: 1rem'
                                             )       
                                    ),
                                    tabPanel('Nation',
                                             value = 'Nation',
                                             div(
                                               withSpinner(
                                                 plotlyOutput('plot_nation'),
                                                 type = 5,
                                                 color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
                                               ),
                                               style = 'margin-top: 1rem'
                                             )       
                                    )
                                    
                        ) # end tabsetPanel
                        
                 ) # end column
               ) # end fluidRow
           ) # end div
  ),
  
  tags$footer(uiOutput('afooter'))
  
) # end navbarpage

