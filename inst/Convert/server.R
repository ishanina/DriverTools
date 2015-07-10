library(shiny)
library(DriverTools)

shinyServer(function(input, output) {
  output$text_out = reactive(getOutput(input,output))
})

"getOutput" <- function(input,output) {
  if (input$auto){
    switch(input$method,
    "1" = paste("Output:",Display(Transform(parser(input$text_in)))),
    "2" = paste("Output:",Display(Simplify(parser(ExpressionTransform(input$text_in))))),
    "3" = paste("Output:",Display(ExpressionTransform(input$text_in))))
  }
  else
    "Output:"
}