library(shiny)
library(DriverTools)

shinyServer(function(input, output) {
  output$text_out = reactive(getOutput(input,output))
})

"getOutput" <- function(input,output) {
  if (input$auto){
    if (input$mult)
      if (input$time) 
        switch(input$method,
               "1" = as.character(system.time(mDisplay(Transform(mparser(input$text_in))))[3]),
               "2" = as.character(system.time(mDisplay(Simplify(mparser(mExpressionTransform(input$text_in)))))[3]),
               "3" = as.character(system.time(mExpressionTransform(input$text_in))[3]),
               "4" = as.character(system.time(mDisplay(Simplify(mparser(mExpressionTransform(mFactor(mparser(input$text_in)))))))[3]))
      else
        switch(input$method,
               "1" = paste("Output:",mDisplay(Transform(mparser(input$text_in)))),
               "2" = paste("Output:",mDisplay(Simplify(mparser(mExpressionTransform(input$text_in))))),
               "3" = paste("Output:",mExpressionTransform(input$text_in)),
               "4" = paste("Output:",mDisplay(Simplify(mparser(mExpressionTransform(mFactor(mparser(input$text_in))))))))
    else
      if (input$time) 
        switch(input$method,
               "1" = as.character(system.time(Display(Transform(parser(input$text_in))))[3]),
               "2" = as.character(system.time(Display(Simplify(parser(ExpressionTransform(input$text_in)))))[3]),
               "3" = as.character(system.time(ExpressionTransform(input$text_in))[3]),
               "4" = as.character(system.time(Display(Simplify(parser(ExpressionTransform(Factor(parser(input$text_in)))))))[3]))
      else
        switch(input$method,
               "1" = paste("Output:",Display(Transform(parser(input$text_in)))),
               "2" = paste("Output:",Display(Simplify(parser(ExpressionTransform(input$text_in))))),
               "3" = paste("Output:",ExpressionTransform(input$text_in)),
               "4" = paste("Output:",Display(Simplify(parser(ExpressionTransform(Factor(parser(input$text_in))))))))
  }
  else
    "Output:"
}