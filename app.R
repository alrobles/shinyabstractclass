#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(miniUI)
library(shiny)
library(httr)

prep_fun <-  function(x) {
  # make text lower case
  x = stringr::str_to_lower(x)
  # remove non-alphanumeric symbols
  x = stringr::str_replace_all(x, "[^[:alnum:]]", " ")
  # collapse multiple spaces
  x = stringr::str_replace_all(x, "\\s+", " ")
  # collapse multiple car returns
  stringr::str_replace_all(x, "\\n+", " ")
}


get_api_abstract_score <- function(abstract){
  if(!is.null(abstract) & nchar(abstract) != 0){
    
    abstract <- prep_fun(abstract)
    
    baseUrl <- "http://www.vidaanalytics.com:8082/scoreabstract?abstract="
    query <-  paste0(baseUrl, abstract)
    output <- httr::content(httr::GET(url = query), encoding = "ISO-8859-1" )
    return(output)
  } else if(nchar(abstract) == 0 ) {
    output <- "0"
    return(output)
  } else if(is.null(abstract) ) {
    output <- "0"
    return(output)
  }
}
## Only run examples in interactive R sessions
ui <- miniPage(
    gadgetTitleBar(title = "Try our API", left = NULL, right = NULL),
    textAreaInput("caption", "Input your abstract",
                  "Abstract",
                  height = "120px", width = "480px"),
    # verbatimTextOutput("value", placeholder = TRUE ),
    # 
    # tags$head(tags$style("#value{color:red; 
    #                      font-size:12px;
    #                      font-style:italic; 
    #                      overflow-y:scroll; 
    #                      max-height: 120px; 
    #                      max-width: 480px; 
    #                      background: ghostwhite;}")),
    
    verbatimTextOutput("abstractvalue", placeholder = TRUE ),
    tags$head(tags$style("#abstractvalue{color:red; 
                         font-size:12px;
                         font-style:italic; 
                         overflow-y:scroll; 
                         max-height: 120px; 
                         max-width: 480px; 
                         background: ghostwhite;}"))
  )
  server <- function(input, output) {
    #output$value <- renderText({ input$caption })
    
    output$abstractvalue <- renderText({
      get_api_abstract_score( input$caption) }
    )
  }
shinyApp(ui, server)
  

