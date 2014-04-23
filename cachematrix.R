## Assignment 2 -- Prof. Roger Peng -- Coursera
#
## I basically modify the "cachevector.R" template 
## the mean calculation in "cachevector" was replaced with "solve" 
## which calculates the matrix inverse
## no error checking -- assume input matrix is a square invertible matrix

## this function creates a cached matrix
## to test this function, type, for example, the following R commands and you should see
## a randomly generated 6x6 square matrix

## a_matrix <- matrix(rnorm(36, 0, 1), nrow = 6, ncol = 6)
## a_cache_matrix <- makeCacheMatrix(a_matrix)
## a_cache_matrix$get()

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL 
  }
  get <- function() x
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## this function calculates the cached inverse of a square matrix
## to test this function, type the following

## cacheSolve(a_cache_matrix)

## to test that the inverse matrix is correct, type the following
## cacheSolve(a_cache_matrix) %*% a_cache_matrix$get()
## you should now see a 6x6 identity matrix printed in the R console

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if (!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$set_inverse(inverse)
  inverse
}
