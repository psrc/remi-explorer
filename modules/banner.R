# Display banner (picture and jumbotron at the top of each tab)

banner_ui <- function(id, photo_filename, banner_title, banner_subtitle = NULL) {
  ns <- NS(id)
  
  tagList( 
    fluidRow(column(3, style = 'padding-right:0px;', #height:5px;
                    div(
                      img(src = photo_filename, 
                          width = "100%", 
                          style = "padding-top: 0px; border-radius:0 0 30px 0;")
                    )
    ),
    column(9, style = 'padding-left:0px;',
           jumbotron(
             title = strong(div(class="mainpage_title", banner_title)),
             status = "success",
             btnName = strong(div(class="mainpage_subtitle", banner_subtitle))
           )
    )
    )
  )
  
}