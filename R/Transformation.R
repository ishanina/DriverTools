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
  for (i in 1:b) {
    c <- list() # a combination of elements from each element of input_list
    for (j in 1:length(input_list))
      # The below expression is used to get an index of a set such that
      # a unique combination of elements is used every iteration of i
      c <- union(c,list(input_list[[j]][[1 + ((i - 1) %/% a[j]) %% length(input_list[[j]])]]))
    n <- union(n,list(ListSort(c))) # set of all combinations
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
  t = !is.na(input_list) # a vector used to choose only minimal sets
  for (k in 1:length(input_list))
    if (t[k])
      for (h in 1:length(input_list))
        if (t[h] && k != h && setequal(intersect(input_list[[k]],input_list[[h]]),input_list[[k]]))
          t[h] = FALSE
  input_list[t] # the minimal sets are returned
}


#' Switches between driver sets and therapeutic sets.
#'   Does so by distributing the sets and simplifying
#' @param input_list a list of all driver/therapeutic sets
#' @return the corresponding complement set
"Transform" <- function(input_list) {
  Simplify(Distribute(input_list))
}

#' Factors a set of driver/therapeutic sets
#' @param input_list a list of driver/therapeutic sets
#' @return String of factored expression
#' @examples 
#' Factor(parser("AB + AC + AD"))
#' Factor(parser("BD + AC + AD + BC"))
"Factor" <- function(input_list) {
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
#' @examples
#' print(Display(list(list("A",9),list("G",3.567))))
"Display" <- function(input_list) {
  string <- ""
  for (i in input_list) {
    for (j in i)
      string = paste(string,toString(j),sep = "")
    string = paste(string,"+",sep = "")
  }
  string <- substr(string,1,nchar(string) - 3)
}

#' Transforms a string to get an expression for the complement set.
#' @param string
#' @return a simplified string representing the complement set
#' @details This function switches the + operator into multiplication and vice versa.
#'  The function should run in linear time with the length of the input string.
#'  
#' For expressions that are factored like this one, this algorithm is much faster\cr
#' \code{system.time(print(Display(Simplify(parser(}\cr
#' \code{ExpressionTransform("A(B + C)D + C(D+E(F+C))AG")))))) #0.01 seconds}\cr\cr
#' \code{system.time(print(Display(}\cr
#' \code{Transform(parser("A(B + C)D + C(D+E(F+C))AG"))))) #0.26 seconds}
#' 
#' For expressions that are expanded out like this one, the algorithm is somewhat slower
#' \code{system.time(Display(Simplify(parser(}\cr
#' \code{ExpressionTransform("ABC + DEF + GHI + JKL + MNO + PQR"))))) #39.42 seconds}\cr\cr
#' \code{system.time(Display(}\cr
#' \code{Transform(parser("ABC + DEF + GHI + JKL + MNO + PQR")))) #38.02 seconds}
#' 
#' However it is slower because of the parser, which expands out the expression.
#' The factored expression is obtained much more quickly\cr
#' \code{system.time(print(}\cr
#' \code{ExpressionTransform("ABC + DEF + GHI + JKL + MNO + PQR"))) #0 seconds}\cr
#' [1] "(A+B+C)(D+E+F)(G+H+I)(J+K+L)(M+N+O)(P+Q+R)"
"ExpressionTransform" <- function(string) {
  parentheses <- ""
  string <- gsub(" ","",string,fixed = T)
  for (i in 1:nchar(string))
    switch(substr(string,i,i),
           parentheses <- paste(parentheses,"(",substr(string,i,i),")",sep = ""),
           "(" = ,
           ")" = ,
           "+" = parentheses <- paste(parentheses,substr(string,i,i),sep = "")
           )
  string <- gsub("+"," ",parentheses,fixed = T)
  string <- gsub(")(","+",string,fixed = T)
  while (substr(string,1,1) == "(" && getlastparen(string) == nchar(string))
    string <- substr(string,2,nchar(string)-1)
  string <- gsub(" ","",string,fixed = T)
  out <- ""
  for (i in 1:nchar(string))
    if (!((substr(string,i,i) == "(" && substr(string,i+2,i+2) == ")")||
          (substr(string,i-2,i-2) == "(" && substr(string,i,i) == ")")))
      out <- paste(out,substr(string,i,i),sep = "")
  out
}