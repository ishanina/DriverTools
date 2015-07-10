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
    n <- union(n,list(as.list(sort(unlist(c))))) # set of all combinations
  }
  n
}

#' Sorts a list
#' @param list
#' @return sorted list. If the input list has numbers, they will be
#'   numerically sorted. If strings are inputed, they will be lexicographically sorted
"Sort" <- function(list) {
  for (i in 1:length(list))
    a[[i]] <- as.list(sort(unlist(list[[i]])))
  a
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
#' @return list of lists of factors and lists of sets
"Factor" <- function(input_list) {
  input_list <- Sort(input_list)
  a = list()
  for (i in 1:(length(input_list)-1))
    for (k in (i+1):length(input_list)) {
      a[[length(a)+1]] <- intersect(input_list[[i]],input_list[[k]])
    }
  a
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
    string = paste(string," + ",sep = "")
  }
  string <- substr(string,1,nchar(string) - 3)
}


#a = list()

#a[[length(a) + 1]] = list("C","A")
#a[[length(a) + 1]] = list("B","A")
#a[[length(a) + 1]] = list("B","C")
#Factor(a)

#Display(a)
#Display(Transform(a))
#Display(Transform(Transform(a)))