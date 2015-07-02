# This function switches between driver sets and therapeutic sets
"Transform" <- function(input_list) {
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
    n <- union(n,list(c)) # set of all combinations
  }
  t = !is.na(n) # a vector used to choose only minimal sets
  for (k in 1:length(n))
    if (t[k])
      for (h in 1:length(n))
        if (t[h] && k != h && setequal(intersect(n[[k]],n[[h]]),n[[k]]))
          t[h] = FALSE
  n[t] # the minimal combinations is returned
}
# This function displays drivers or therapeutics in a simple way
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

#a[[length(a) + 1]] = list("A","C")
#a[[length(a) + 1]] = list("A","B")
#a[[length(a) + 1]] = list("B","C")

#Display(a)
#Display(Transform(a))
#Display(Transform(Transform(a)))