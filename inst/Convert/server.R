library(shiny)
library(DriverTools)

shinyServer(function(input, output) {
  "ExpressionSimplify" <- expression({Display(Simplify(RemoveDefine(SatisfiableList(parser(InvertString(ExpressionTransform(Sub(res[[4]][i],length(res[[3]]),res[[3]],res[[4]]))))),res[[3]])))})
  "Factoring" <- expression({
    Display(Simplify(InvertList(SatisfiableList(parser(ExpressionTransform(Factor(parser(input$text_in))))))))
  })
  
  "getOutput" <- function() {
    if (input$auto)
      c("Output",SubstituterTherapies(isolate(input$input_eqns)))
    else
      c("Output")
  }
  output$text_out = renderUI(lapply(getOutput(),div))
})