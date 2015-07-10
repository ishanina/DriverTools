library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("",windowTitle = "Driver set - Therapeutic set conversion"),
  fluidRow(
    column(6,selectInput("method", label = "Method", selected = 3,
                choices = list(
                  "Standard" = 1, 
                  "Expression Simplify" = 2, 
                  "Expression" = 3))),
    column(1,checkboxInput("auto","Apply", value = T))
  ),
  tagAppendAttributes(
    textInput("text_in","Enter sets (separated by pluses)",value = "AB + CD"),
    style = "width: 100%; "),
  textOutput("text_out")
))
