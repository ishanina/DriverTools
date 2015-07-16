#' Factors a set of driver/therapeutic sets
#' @param input_list a list of driver/therapeutic sets
#' @return String of factored expression
#' @examples 
#' @details it uses an explicit '*' operator, however
#' this function is as of yet not implimented
#' Factor(parser("A*B + A*C + A*D"))
#' Factor(parser("BD + AC + AD + BC"))
"mFactor" <- function(input_list) {
  a = list()
  for (i in input_list)
    a[[length(a)+1]] <- ListSort(i)
  c = c()
  for (i in 1:(length(input_list)-1))
    for (k in (i+1):length(input_list))
      if (length(intersect(input_list[[i]],input_list[[k]])) != 0)
        c <- c(c,Display(list(ListSort(intersect(input_list[[i]],input_list[[k]])))))
  if (is.null(c))
    Display(input_list)
  else
  {
    b = as.factor(c)
    final = paste(names(sort(summary(b),decreasing = T)[1]),"(",sep = "")
    fac = parser(names(sort(summary(b),decreasing = T)[1]))[[1]]
    fact = list()
    notfact = list()
    for (i in 1:(length(input_list)))
      if (!setequal(intersect(input_list[[i]],fac),fac))
        input_list[[i]] -> notfact[[length(notfact) + 1]]
      else
        setdiff(input_list[[i]], fac) -> fact[[length(fact) + 1]]
    switch (as.character(length(notfact)),
            final <- paste(final,Factor(fact),") + ",Factor(notfact),sep = ""),
            "0" = final <- paste(final,Factor(fact),")",sep = ""),
            "1" = final <- paste(final,Factor(fact),") + ",Display(notfact),sep = ""))
    final
  }
}
#' Displays drivers or therapeutics in a simple way
#' @param input_list sets of driver/therapeutic sets
#' @return string to diplay
#' @details it functions using an explicit '*' operator
#' @examples
#' print(Display(list(list("A",9),list("G",3.567))))
"mDisplay" <- function(input_list) {
  string <- ""
  for (i in input_list) {
    T -> first
    for (j in i)
      if (!first)
        string = paste(string,toString(j),sep = "*")
      else {
        string = paste(string,toString(j),sep = "")
        first <- F
      }
    string = paste(string,"+",sep = "")
    first = T
  }
  string <- substr(string,1,nchar(string) - 3)
}

#' Transforms a string to get an expression for the complement set.
#' @param string
#' @return a simplified string representing the complement set
#' @details This function switches the + operator into multiplication and vice versa.
#'  It does so explicitly using the '*' operator
#'  The function should run in linear time with the length of the input string.
#'  
#' For expressions that are factored like this one, this algorithm is much faster\cr
#' \code{system.time(print(mDisplay(Simplify(parser(}\cr
#' \code{mExpressionTransform("A*(B + C)*D + C*(D+E*(F+C))*A*G")))))) #0.01 seconds}\cr\cr
#' \code{system.time(print(mDisplay(}\cr
#' \code{Transform(mparser("A*(B + C)*D + C*(D+E*(F+C))*A*G"))))) #0.26 seconds}
#'  
#' For expressions that are expanded out like this one, the algorithm is somewhat slower
#' \code{system.time(Display(Simplify(parser(}\cr
#' \code{mExpressionTransform("A*B*C + D*E*F + G*H*I + J*K*L + M*N*O + P*Q*R"))))) #39.42 seconds}\cr\cr
#' \code{system.time(Display(}\cr
#' \code{Transform(mparser("A*B*C + D*E*F + G*H*I + J*K*L + M*N*O + P*Q*R")))) #38.02 seconds}
#' 
#' However it is slower because of the parser, which expands out the expression.
#' The factored expression is obtained much more quickly\cr
#' \code{system.time(print(}\cr
#' \code{mExpressionTransform("A*B*C + D*E*F + G*H*I + J*K*L + M*N*O + P*Q*R"))) #0 seconds}\cr
#' [1] "(A+B+C)*(D+E+F)*(G+H+I)*(J+K+L)*(M+N+O)*(P+Q+R)"
"mExpressionTransform" <- function(string) {
  parentheses <- ""
  string <- gsub(" ","",string,fixed = T)
  string <- paste("(",string,")",sep = "")
  string <- gsub("*","&",string,fixed = T)
  string <- gsub("+",")*(",string,fixed = T)
  string <- gsub("&","+",string,fixed = T)
  RemoveParen(string)
}
#' Removes some unnecessary parentheses from expressions
#' @param string
#' @return string without some parentheses that doesn't change the meaning of the expression
#' @examples RemoveParen("(AB)")
"RemoveParen" <- function(string) {
  i <- 1
  while (substr(string,1,1) == "(" && getlastparen(string) == nchar(string))
    string <- substr(string,2,nchar(string)-1)
  while (i <= nchar(string)) {
    if (substr(string,i,i) == "(" && !grepl("+",substr(string,i,getlastparen(substr(string,i,nchar(string)))+i-1),fixed = T)) {
      string <- paste(substr(string,1,i-1),substr(string,i+1,i+getlastparen(substr(string,i,nchar(string)))-2),substr(string,i+getlastparen(substr(string,i,nchar(string))),nchar(string)),sep = "")
    }
    else 
      i = i + 1
  }
  string
}
