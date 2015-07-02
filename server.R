library(shiny)
source("Transformation.R")

"fun" <- function(input) {
  nospace <- gsub(" ","",input$text_in,fixed = T)
  a <- list(list())
  for (i in 1:nchar(nospace)) {
    if (substr(nospace,i,i) != "+")
      a[[length(a)]][[length(a[[length(a)]])+1]] <- substr(nospace,i,i)
    else
      a[[length(a)+1]] <- list()
  }
  paste("Output: ",Display(Transform(a)))
}

shinyServer(function(input, output) {
  output$text_out = reactive({fun(input)})
})