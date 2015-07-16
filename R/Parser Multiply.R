#' Parses input to interpret it as driver/therapeutic sets
#' @param input
#' @return the driver/therapeutic sets denoted by the input string
#' @details this function explicitly requires the multiplication operator '*'
#' @examples
#' mparser("(A*(B + C)*D + C*(D+E*(F+C))*A*G)")
"mparser" <- function(input) {
  if (!Proper(input) || input == "" || input == "+")
    list(list("Incorrect Input"))
  else {
    no <- gsub(" ","",input,fixed = T)
    a <- list()
    b <- list()
    name <- ""
    while(nchar(no) > 0)
      switch(substr(no,1,1),
             "+" = {
               if (name != "")
                 b <- Multiply(b,list(list(name)))
               name = ""
               a <- union(a,b)
               b <- list()
               no <- substr(no, 2,nchar(no))
             },
             "(" = {
               b <- Multiply(b,mparser(substr(no,2,getlastparen(no)-1)))
               no <- substr(no, getlastparen(no)+1,nchar(no))
             },
             {
               name <- paste(name,substr(no,1,1),sep = "")
               no <- substr(no, 2,nchar(no))
             },
             "*" = {
               if (name != "")
                 b <- Multiply(b,list(list(name)))
               no <- substr(no, 2,nchar(no))
               name<-""
             }
      )
    if (name != "")
      b <- Multiply(b,list(list(name)))
    union(a,b)
  }
}