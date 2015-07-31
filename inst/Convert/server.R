library(shiny)
library(DriverTools)

shinyServer(function(input, output) {
  "getOutput" <- function() {
    if (input$auto)
      c("List of Strategies: ",gsub("*"," * ",SubstituterTherapies(isolate(input$input_eqns)),fixed = T))
    else
      c("List of Strategies: ")
  }
  output$text_out = renderUI(lapply(getOutput(),div))
})