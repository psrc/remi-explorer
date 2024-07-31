# Display UI for trends tab

dummy_tab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    banner_ui('dummyBanner', 
              photo_filename = psrc_photos[sample.int(length(psrc_photos), 1)], 
              banner_title = "Something Else", 
              banner_subtitle = "Something Something"),

    
  )
  
}

dummy_tab_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
 
  }) # end moduleServer
  
}