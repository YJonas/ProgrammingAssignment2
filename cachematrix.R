## The two functions invert the input matrix, and cache the inverse.
## The inverse will only be recalculated, when the input matrix changes.

## "makeCacheMatrix" function creates a list of sub-functions that can
## cache the inverse of the input matrix and get the cached matrix from
## the memory

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(inversed) inv <<- inversed
  get_inv <- function() inv
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv) 
}


## "cacheSolve" function compute and return the inverse of the matrix, if it hasn't been done.
## The argument "x" is a function, with sub-functions.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
