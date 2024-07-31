server <- function(input, output, session) {
  footer_server('myFooter')
  
  trends_tab_server('trendsTab')
  dummy_tab_server('dummy')
  
}
