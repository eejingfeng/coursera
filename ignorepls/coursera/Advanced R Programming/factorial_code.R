library(purrr)
library(microbenchmark)

### Factorial loop

Factorial_loop <- function(x) {
  result <- 1
  if (x==0) {return(1)}
  for (n in 1:x) {
    result <- result * n
  }
  return(result)
}

### Factorial reduce

Factorial_reduce <- function(x) {
  if (x==0) {return(1)}
  result <- reduce(as.numeric(1:x),`*`)
  return(result)
}

### Factorial func

Factorial_func <- function(x) {
  if (x==0) {return(1)}
  result <- x* Factorial_func(x-1)
  return(result)
}

### Factorial mem 

Factorial_mem <- function(x) {
  if (x==0) {return(1)}
  fact_table[x] <<- x* Factorial_mem(x-1)
  return(fact_table[x])
}

### benchmark function
benchmark <- function(occurance) {
  microbenchmark(
    result <- Factorial_loop(occurance),
    result <- Factorial_reduce(occurance),
    result <- Factorial_func(occurance),
    result <- Factorial_mem(occurance)
  )
}


#### Comparison
test1 <- 10
fact_table <- c(rep(NA, test1))
benchmark(test1)

test2 <- 100
fact_table <- c(rep(NA, test2))
benchmark(test2)

test3 <- 1000
fact_table <- c(rep(NA, test3))
benchmark(test3)




