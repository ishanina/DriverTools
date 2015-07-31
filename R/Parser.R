#' This gives important information about a string with equations.
#' @param input_string a string with equations delimited by '\n'
#' @return A list of 4 things are returned. The first is a boolean
#' which is true if nothing is defined multiple times. The second is
#' a vector of all the variables used in the equations. The third is
#' a list of the left hand sides of the equations, and the fourth is a
#' a list of the right hand sides of the equations.
#' @examples 
#' CheckEqns("A = B + C\nA = C + A")
#' CheckEqns("A = B*C\nD = E+A")
"CheckEqns" <- function(input_string) {
  AddName <- expression({ #adds a variable to the list of variables
    if (stri != "" && !is.element(stri,inputs))
      inputs <- c(inputs,stri)
    stri <- ""
  })
  inputs <- c() #list of variables involved
  stri <- "" #variable used to read a variable in an equation
  left <- list() #left hand sides of equations
  right <- list() #right hand sides of equations
  onedef <- T #whether or not everything is only defined once
  for (i in gsub(" ", "", unlist(strsplit(input_string, split="\n")))){
    for (j in 1:nchar(i)){
      switch(substr(i,j,j),
             "=" = {
               old <- left
               left <- union(left,stri)
               if (setequal(left,old))
                 onedef <- F
               right <- union(right,list(paste("(",substr(i,j+1,nchar(i)),")",sep = "")))
               eval(AddName)
             },
             "(" =,")" =,"*" =,"+" = eval(AddName),
             stri <- paste(stri,substr(i,j,j),sep = "")
      )
    }
    eval(AddName)
  }
  list(onedef,inputs,left,right)
}

#' Substitutes equations into eachother to find the therapeutic sets
#' @param eqns a string with equations delimited by '\n'
#' @return a vector of string outputs
#' @details substitutes in equations into eachother the number of times that there are equations.
#' Then, the terms involving defined terms are removed and the resulting minimal ones are returned.
#' @examples 
#' SubstituterTherapies("C = !A*B\nB = D*C+E*A\nA = F+C")
#' SubstituterTherapies("P = P*!C + A")
"SubstituterTherapies" <- function(eqns) {
  ExpressionSimplify <- expression({Display(Simplify(RemoveDefine(SatisfiableList(parser(InvertString(ExpressionTransform(Sub(res[[4]][i],length(res[[3]]),res[[3]],res[[4]]))))),res[[3]])))})
  res <- CheckEqns(eqns)
  if (res[[1]]) {
    i <- 1
    #for(i in 1:length(res[[4]])) #for each equation, it finds the therapeutic sets
      out <- strsplit(eval(ExpressionSimplify), split="+", fixed = T)
    out[[1]]
  }
  else
    "Error: Something was defined multiple times"
}

#' Removes lists in a list containing any defined terms
#' @param input_list a list of driver/therapeutic sets
#' defined a list of terms that are to be removed
#' @return driver/therapeutic sets without the defined terms
#' @examples RemoveDefine(parser("A*B+A*C+D*B*+E*F"),list("A"))
"RemoveDefine" <- function(input_list,defined) {
  correct = !is.na(input_list)
  for (i in 1:length(input_list))
    for (j in 1:length(input_list[[i]]))
      if (is.element(gsub("!","",input_list[[i]][[j]]),defined)) {
        correct[i] = FALSE
        break
      }
  input_list[correct]
}

#' Substitutes equations into expr n number of times.
#' @param 
#' expr expression to be substituted
#' n number of times to substitute
#' left left hand sides of equations
#' right right hand sides of equations
#' @return substituted equations
#' @examples Sub("A",3,"A","(A+B)")
"Sub" <- function(expr, n, left, right) {
  expr <- gsub(" ","",expr)
  addstri <- expression({
    if (stri != ""){
      if (substr(stri,1,1) != "!") {
        ind <- match(stri, left)
        if (!is.na(ind))
          out <- paste(out,right[ind],sep = "")
        else
          out <- paste(out,stri,sep = "")
      }
      else {
        ind <- match(substr(stri,2,nchar(stri)), left)
        if (!is.na(ind))
          out <- paste(out,"(",InvertString(ExpressionTransform(right[ind])),")",sep = "")
        else
          out <- paste(out,stri,sep = "")
      }
    }
  })
  stri <- ""
  out <- ""
  for (i in 1:nchar(expr)) {
    switch(substr(expr,i,i),
           "(" =,")" =,"*" =,"+" = {
             eval(addstri)
             stri <- ""
             out <- paste(out,substr(expr,i,i),sep = "")
           },
           stri <- paste(stri,substr(expr,i,i),sep = "")
    )
  }
  eval(addstri)
  if (n == 1)
    out
  else
    Sub(out, n-1, left, right)
}

#' Removes non-satisfiable elements of a list
#' @param input_list list of driver/therapeutic sets
#' @return satisfiable driver/therapeutic sets
#' @details any driver/therapeutic set with some string 
#' as well as the string with '!' is not satisfiable and is removed
#' @examples SatisfiableList(parser("A*!A+B"))
"SatisfiableList" <- function(input_list) {
  a = !is.na(input_list)
  for (i in 1:length(input_list))
    for (j in 1:length(input_list[[i]]))
      if (substr(input_list[[i]][[j]],1,1) == "!")
        for (k in 1:length(input_list[[i]]))
          if (substr(input_list[[i]][[j]],2,nchar(input_list[[i]][[j]])) == input_list[[i]][[k]])
            a[i] <- F
  input_list[a]
}

#' Applies '!' to each element of each driver/therapeutic set
#' @param input_list list of driver/therapeutic sets
#' @return driver/therapeutic sets with '!' applied to each element
#' @details if an element has '!', the '!' is removed
#' @examples InvertList(parser("A + !F"))
"InvertList" <- function(input_list) {
  for (i in 1:length(input_list))
    for (j in 1:length(input_list[[i]]))
      if (substr(input_list[[i]][[j]],1,1) == "!")
        input_list[[i]][[j]] <- substr(input_list[[i]][[j]],2,nchar(input_list[[i]][[j]]))
      else
        input_list[[i]][[j]] <- paste("!",input_list[[i]][[j]],sep = "")
      input_list
}
#' Inverts a string by applying '!' to each variable
#' @param string
#' @examples InvertString("A + B*(C+D)")
"InvertString" <- function(string) {
  string <- gsub(" ","",string)
  out <- ""
  add <- ""
  for (i in 1:nchar(string)){
    switch(substr(string,i,i),
           "(" = ,")" = ,"+" = ,"*" = {
             if (add != "") {
               if (substr(add,1,1) == "!")
                 out <- paste(out,substr(add,2,nchar(add)),sep = "")
               else
                 out <- paste(out,"!",add,sep = "")
             }
             out <- paste(out,substr(string,i,i),sep = "")
             add <- ""
           },
           add <- paste(add,substr(string,i,i),sep = ""))
  }
  if (add != "")
    if (substr(add,1,1) == "!")
      out <- paste(out,substr(add,2,nchar(add)),sep = "")
    else
      out <- paste(out,"!",add,sep = "")
  out
}

#' Parses input to interpret it as driver/therapeutic sets
#' @param input
#' @return the driver/therapeutic sets denoted by the input string
#' @details this function explicitly requires the multiplication operator '*'
#' @examples
#' parser("(A*(B + C)*D + C*(D+E*(F+C))*A*G)")
"parser" <- function(input) {
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
               b <- Multiply(b,parser(substr(no,2,getlastparen(no)-1)))
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
               name <- ""
             }
      )
    if (name != "")
      b <- Multiply(b,list(list(name)))
    union(a,b)
  }
}

#' Finds the index of a parenthesis corresponding to the first one.
#' @param s a string
#' @examples getlastparen("(()())")
getlastparen <- function(s){
  j = 1
  for (i in 2:nchar(s)) {
    switch(substr(s,i,i),
           "(" = j <- j + 1,
           ")" = j <- j - 1)
    if (j == 0)
      break
  }
  i
}

#' Distributes two lists of lists
#' @param lon a list of driver/therapeutic sets
#' lo a list of driver/therapeutic sets
#' @return a list of all the ways to combine elements from each list
#' @examples Multiply(parser("A*B"),parser("C+D"))
Multiply <- function(lon,lo) {
  if (length(lon) != 0) {
    out <- list()
    for (i in lon)
      for (j in lo)
        out[[length(out)+1]] <- union(i,j)
    out
  }
  else
    lo
}

#' Checks if a string has the same number of opening and closing parentheses.
#' @param string
#' @return boolean evaluating the condition
#' @examples Proper("(AB)")
#' Proper("((((((AB)))))")
Proper <- function(string) {
  value <- as.numeric(regexpr("\\([^\\(]*$", string)) <= as.numeric(regexpr("\\)[^\\)]*$", string))
  string <- gsub("A","",string,fixed = T)
  string <- gsub("B","",string,fixed = T)
  string <- gsub("(","A",string,fixed = T)
  string <- gsub(")","B",string,fixed = T)
  sapply(regmatches(string, gregexpr("B", string)), length) == sapply(regmatches(string, gregexpr("A", string)), length) && value
}

