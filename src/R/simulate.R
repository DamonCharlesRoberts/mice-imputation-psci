#' @title simulate
#' 
#' @description
#' This is a function that simulates five columns and returns a
#' data.table object. The equations for the simulation of the
#' columns are the following.
#' - Variable A: gamma(shape=1)
#' - Variable B: binomial(0.6)
#' - Variable X: 0.2 * A + 0.5 * B + normal(0, 10)
#' - Variable Z: 0.9 * A * B + normal(0, 10)
#' - Variable Y: 0.6 * Z * Z + normal(0, 10)
#' 
#' @param N the size of the population data.frame
#' @returns df_population. 
#' A data.table object containing the population data.
#' @examples
#' 
#' df_population <- simulate(N = 100)
#' nrow(df_population) # returns 100
#' 
#' df_population <- simulate(N = 1000)
#' nrow(df_population) # returns 1000
#' @export
simulate <- function (N = 1000000) {
  # Create simulated data vectors
  A=stats::rgamma(
    n=N
    ,shape=1
  )
  B=stats::rbinom(
    n=N
    ,size=1
    ,prob=0.6
  )
  X=0.2*A+0.5*B+stats::rnorm(n=N,mean=0,sd=10)
  Z=0.9*A*B+stats::rnorm(n=N,mean=0,sd=10)
  Y=0.6*X*Z+stats::rnorm(n=N,mean=0,sd=10)
  id=seq.int(1,N)
  # Store it in a data.table object
  df_population <- data.table::data.table(id,A,B,X,Z,Y)
  # Return the data.table object
  return(df_population)
}