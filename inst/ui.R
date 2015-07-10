library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("",windowTitle = "Driver set - Therapeutic set conversion"),
  tagAppendAttributes(
    textInput("text_in","Enter sets (separated by pluses)",value = "AB + CD"),
    style = "width: 100%; "),
  textOutput("text_out")
))
