#' Parses input to interpret it as driver/therapeutic sets
#' @param input
#' @return the driver/therapeutic sets denoted by the input string
#' @examples
#' parser("(A(B + C)D + C(D+E(F+C))AG)")
"parser" <- function(input) {
  if (!Proper(input) || input == "" || input == "+")
    list(list("Incorrect Input"))
  else {
    no <- gsub(" ","",input,fixed = T)
    level <- 1
    a <- list()
    b <- list()
    while(nchar(no) > 0)
      switch(substr(no,1,1),
             "+" = {
               a <- union(a,b)
               b <- list()
               no <- substr(no, 2,nchar(no))
             },
             "(" = {
               b <- Multiply(b,parser(substr(no,2,getlastparen(no)-1)))
               no <- substr(no, getlastparen(no)+1,nchar(no))
             },
             {
               b <- Multiply(b,list(list(substr(no,1,1))))
               no <- substr(no, 2,nchar(no))
             }
      )
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

