#' Displays drivers or therapeutics in a simple way
#' @param input_list sets of driver/therapeutic sets
#' @return string to diplay
#' @details it functions using an explicit '&' operator
#' @examples
#' print(Display(list(list("A",9),list("G",3.567))))
"Display" <- function(input_list) {
  string <- ""
  for (i in input_list) {
    T -> first
    for (j in i)
      if (!first)
        string = paste(string,toString(j),sep = "&")
      else {
        string = paste(string,toString(j),sep = "")
        first <- F
      }
      string = paste(string,"+",sep = "")
      first = T
  }
  string <- substr(string,1,nchar(string) - 1)
}

#' Transforms a string to get an expression for the complement set.
#' @param string
#' @return a simplified string representing the complement set
#' @details This function switches the + operator into multiplication and vice versa.
#'  It does so explicitly using the '&' operator
#'  The function should run in linear time with the length of the input string.
#'  
#' For expressions that are factored like this one, this algorithm is much faster\cr
#' \code{system.time(print(mDisplay(Simplify(parser(}\cr
#' \code{ExpressionTransform("A&(B + C)&D + C&(D+E&(F+C))&A&G")))))) #0.01 seconds}\cr\cr
#' \code{system.time(print(mDisplay(}\cr
#' \code{Transform(parser("A&(B + C)&D + C&(D+E&(F+C))&A&G"))))) #0.26 seconds}
#'  
#' For expressions that are expanded out like this one, the algorithm is somewhat slower
#' \code{system.time(Display(Simplify(parser(}\cr
#' \code{ExpressionTransform("A&B&C + D&E&F + G&H&I + J&K&L + M&N&O + P&Q&R"))))) #39.42 seconds}\cr\cr
#' \code{system.time(Display(}\cr
#' \code{Transform(parser("A&B&C + D&E&F + G&H&I + J&K&L + M&N&O + P&Q&R")))) #38.02 seconds}
#' 
#' However it is slower because of the parser, which expands out the expression.
#' The factored expression is obtained much more quickly\cr
#' \code{system.time(print(}\cr
#' \code{ExpressionTransform("A&B&C + D&E&F + G&H&I + J&K&L + M&N&O + P&Q&R"))) #0 seconds}\cr
#' [1] "(A+B+C)&(D+E+F)&(G+H+I)&(J+K+L)&(M+N+O)&(P+Q+R)"
"ExpressionTransform" <- function(string) {
  parentheses <- ""
  string <- gsub(" ","",string,fixed = T)
  string <- paste("(",string,")",sep = "")
  string <- gsub("&","*plaCeholder*",string,fixed = T)
  string <- gsub("+",")&(",string,fixed = T)
  string <- gsub("*plaCeholder*","+",string,fixed = T)
  RemoveParen(string)
}
#' Removes some unnecessary parentheses from expressions
#' @param string
#' @return string without some parentheses that doesn't change the meaning of the expression
#' @examples RemoveParen("(AB)")
"RemoveParen" <- function(string) {
  i <- 1
  while (substr(string,1,1) == "(" && getlastparen(string) == nchar(string))
    # This removes parentheses enclosing an entire string
    string <- substr(string,2,nchar(string)-1)
  while (i <= nchar(string))
    # This scans through for parentheses which don't enclose a '+' and removes them
    if (substr(string,i,i) == "(" && !grepl("+",substr(string,i,getlastparen(substr(string,i,nchar(string)))+i-1),fixed = T))
      string <- paste(substr(string,1,i-1),substr(string,i+1,i+getlastparen(substr(string,i,nchar(string)))-2),substr(string,i+getlastparen(substr(string,i,nchar(string))),nchar(string)),sep = "")
    else 
      i = i + 1
  string
}


#' Find all the distinct ways one can take one element from each input set.
#' @param input_list a list of driver sets/therapeutic sets
#' @return a list containing every possible combination of one element 
#' @details The function removes identical combinations from each input set 
#'   and returns the unique ones such that they are lexicographically sorted
"Distribute" <- function(input_list) {
  a <- c(1) # used to alternate between all possibilities of elements
  for (i in input_list)
    a <- c(a,tail(a,n = 1) * length(i))
  b <- tail(a,n = 1) # number of ways to combine elements
  n <- list()
  i <- 1
  while (i <= b) {
    c <- list() # a combination of elements from each element of input_list
    for (j in 1:length(input_list))
      # The below expression is used to get an index of a set such that
      # a unique combination of elements is used every iteration of i
      c <- union(c,list(input_list[[j]][[1 + ((i - 1) %/% a[j]) %% length(input_list[[j]])]]))
    n <- union(n,list(ListSort(c))) # set of all combinations
    i <- i+1
  }
  n
}

#' Sorts a list
#' @param list
#' @return sorted list. If the input list has numbers, they will be
#'   numerically sorted. If strings are inputed, they will be lexicographically sorted
"ListSort" <- function(list) {
  if (typeof(list) != "closure")
    as.list(sort(unlist(list)))
  else
    list
}
#' Finds minimal sets. Given an input, this function returns all the distinct 
#' sets that are not proper subsets of other sets.
#' @param input_list a list of driver/therapeutic sets
#' @return minimal sets from input
"Simplify" <- function(input_list) {
  if (length(input_list) == 0)
    list()
  else {
    t = !is.na(input_list) # a vector used to choose only minimal sets
    for (k in 1:length(input_list))
      if (t[k])
        for (h in 1:length(input_list))
          if (t[h] && k != h && setequal(intersect(input_list[[k]],input_list[[h]]),input_list[[k]]))
            t[h] = FALSE
    input_list[t] # the minimal sets are returned
  }
}


#' Switches between driver sets and therapeutic sets.
#'   Does so by distributing the sets and simplifying
#' @param input_list a list of all driver/therapeutic sets
#' @return the corresponding complement set
"Transform" <- function(input_list) {
  Simplify(Distribute(input_list))
}