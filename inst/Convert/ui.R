library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Find therapeutic strategies",windowTitle = "Find therapeutic strategies"),
  tagAppendAttributes(
    checkboxInput("show","Help",value = TRUE),
    style = "color:red"
  ),
  uiOutput("info"),
  h3("Equations for cancer:"),
  tagAppendAttributes(
    tags$textarea(id = "input_eqns",rows = 5,'Cancer = !Apoptosis&Proliferation&!DNARepair\nApoptosis = A&B + F & Proliferation\nProliferation = C&!D + DNARepair&E\nDNARepair = !Apoptosis&D&B + !C&F'),
    style = "max-width: 100%; min-width: 100% ;background-color:#FFFF80"),
  fluidRow(
    column(2,actionButton("auto",style="background-color:#CDFCE0; font-weight: bold","Derive therapeutic strategies"))
  ),
  uiOutput("outputinfo"),
  h3("List of therapeutic strategies:"),
  tagAppendAttributes(
  uiOutput("text_out"),
  style = "background-color:#CDFCE0; font-weight: bold"
  )
))