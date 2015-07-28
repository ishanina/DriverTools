library(shiny)
library(DriverTools)

shinyServer(function(input, output) {
  "getOutput" <- function() {
    if (input$auto)
      c("Output",SubstituterTherapies(isolate(input$input_eqns)))
    else
      c("Output")
  }
  output$text_out = renderUI(lapply(getOutput(),div))
})