library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("",windowTitle = "Driver set - Therapeutic set conversion"),
  fluidRow(
    column(6,
      textInput("text_in","Enter sets (separated by pluses)",value = "AB + CD")
    ),
    column(6,
      textOutput("text_out")
    )
  )
))