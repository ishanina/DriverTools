library(shiny)
library(DriverTools)

shinyServer(function(input, output) {
  "getOutput" <- function() {
    if (input$auto){
      out <- gsub("&"," & ",SubstituterTherapies(isolate(input$input_eqns)),fixed = T)
      paste(1:length(out),out,sep =") ")
    }
    else
      NULL
  }
  output$outputinfo <- renderUI({
    if(input$show)
      HTML("<p style = color:red>The outputs are all possible minimal strategies to not satisfy the first equation </p>")
  })
  output$info <- renderUI({ 
    if(input$show) 
      HTML("<p style = color:red>This serves to convert between equations describing driver sets of cancer and its
           corresponding set of therapeutic sets.
           <br>To use, enter in equations describing 
           necessary conditions for cancer. Use the operator '&' to mean 'and',
           '+' to mean 'or', and '!' to mean 'not'. Lastly, parentheses can be used to signify distribution. For
           example, '(A + D)&F' is interpreted as 'A&F + D&F'.</p>")})
  output$text_out = renderUI({
      lapply(getOutput(),div)
    })
})