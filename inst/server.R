library(shiny)
library(DriverTools)
shinyServer(function(input, output) {
  output$text_out = 
    reactive(paste("Output: ",Display(Transform(parser(input$text_in)))))
})