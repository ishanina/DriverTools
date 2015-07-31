library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("Driver/Therapeutic Set Conversion",windowTitle = "Driver/Therapeutic Set Conversion"),
  HTML("<p>This serves to convert between equations describing driver sets of cancer and its
           corresponding set of therapeutic sets.</p> <p>To use, enter in equations describing 
           necessary conditions for cancer. Use the operator '*' to mean 'and',
           '+' to mean 'or', and '!' to mean 'not'. Lastly, parentheses can be used to signify distribution. For
           example, '(A + D)*F' is interpreted as 'A*F + D*F'.</p>"),
  fluidRow(
    column(2,actionButton("auto","Apply"))
  ),
  tagAppendAttributes(
    tags$textarea(id = "input_eqns",rows = 5,'Cancer = !Apoptosis*Proliferation*!DNARepair\nApoptosis = A*B + F * Proliferation\nProliferation = C*!D + DNARepair*E\nDNARepair = !Apoptosis*D*B + !C*F'),
    style = "max-width: 100%; min-width: 100%"),
  HTML("<p>The outputs are all possible minimal strategies to not satisfy the first equation"),
  uiOutput("text_out")
))