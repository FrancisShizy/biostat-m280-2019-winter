## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n <- length(x)
  ind <- sapply(1:n, isPrime)
  return (mean(x[ind]))
}

# simulate data
compare_methods <- function(dist, n, reps, seed) {
  set.seed(seed)
  msePrimeAvg <- 0.0
  mseSamplAvg <- 0.0
  for (r in 1:reps) {
    if (dist == "gaussian"){ 
   x <- rnorm(n) 
  } else if (dist == "t1"){
   x <- rt(n,df=1) 
  } else if (dist == "t5"){ 
   x <- rt(n, 5) 
  } else {
   stop(paste("unrecognized dist: ", dist))
  }
# compute MSEs for the primed-indexed average estimator 
# and the classical sample average estimator
   msePrimeAvg <- msePrimeAvg + estMeanPrimes(x)^2 
   mseSamplAvg <- mseSamplAvg + mean(x)^2
 }
   mseSamplAvg <- mseSamplAvg / reps
   msePrimeAvg <- msePrimeAvg / reps
   return(c(mseSamplAvg, msePrimeAvg))
}

compare_methods (dist, n, reps, seed)

# estimate mean
#estMeanPrimes(x)
