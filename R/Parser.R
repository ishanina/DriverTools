"CheckEqns" <- function(input_string) {
  "AddName" <- expression({
    if (stri != "" && !is.element(stri,inputs))
      inputs <- c(inputs,stri)
    stri <- ""
  })
  inputs <- c()
  stri <- ""
  left <- list()
  right <- list()
  onedef <- T
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

"SubstituterTherapies" <- function(eqns) {
  ExpressionSimplify <- expression({Display(Simplify(RemoveDefine(SatisfiableList(parser(InvertString(ExpressionTransform(Sub(res[[4]][i],length(res[[3]]),res[[3]],res[[4]]))))),res[[3]])))})
  res <- CheckEqns(eqns)
  out <- NULL
  if (res[[1]]) {
    for(i in 1:length(res[[4]]))
      out <- c(out,paste("!",res[[3]][i],"=",eval(ExpressionSimplify),sep = ""))
    out
  }
  else
    "Error: Something was defined multiple times"
}

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

"Sub" <- function(expr, n, left, right) {
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

"InvertList" <- function(input_list) {
  for (i in 1:length(input_list))
    for (j in 1:length(input_list[[i]]))
      if (substr(input_list[[i]][[j]],1,1) == "!")
        input_list[[i]][[j]] <- substr(input_list[[i]][[j]],2,nchar(input_list[[i]][[j]]))
      else
        input_list[[i]][[j]] <- paste("!",input_list[[i]][[j]],sep = "")
      input_list
}

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

# gets the last instance of ")" in a string
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

# distributes two lists of lists
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

Proper <- function(string) {
  value <- as.numeric(regexpr("\\([^\\(]*$", string)) <= as.numeric(regexpr("\\)[^\\)]*$", string))
  string <- gsub("A","",string,fixed = T)
  string <- gsub("B","",string,fixed = T)
  string <- gsub("(","A",string,fixed = T)
  string <- gsub(")","B",string,fixed = T)
  sapply(regmatches(string, gregexpr("B", string)), length) == sapply(regmatches(string, gregexpr("A", string)), length) && value
}

