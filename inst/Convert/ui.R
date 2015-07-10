library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("Driver/Therapeutic Set Conversion",windowTitle = "Driver/Therapeutic Set Conversion"),
  HTML("<p>This serves to convert between a set of driver sets and its
           corresponding set of therapeutic sets.</p> <p>To use, enter in the 
           driver sets as characters. A group of characters such as 'ABC'
           together will be interpreted as a driver set. the operation '+'
           will be interpreted as separating driver sets. For example, 
           'D + F' is interpreted to mean there are two driver sets, one 
           which contains the event D and another that contains F.
           Lastly, parentheses can be used to signify distribution. For
           example, '(A + D)F' is interpreted as 'AF + DF'.</p> <p>There 
           are currently three algorithms in place. The first, called 
           'Default' simplifies the expression, distributes each term,
           and simplifies. The second algorithm, called 'Expression 
           Simplify' modifies the original expression, then expands it
           out. This is much faster than 'Default' when there are 
           parentheses in the input expression, and is recommended for
           expanded results. The last algorithm, 'Expression' is the same
           as before, except it does not simplify. It is extremely fast, 
           and gives a factored answer.</p>"),
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
