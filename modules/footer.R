# Display footer

footer_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('afooter'))
  )
  
}

footer_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns

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
    
  }) # end moduleServer
  
}