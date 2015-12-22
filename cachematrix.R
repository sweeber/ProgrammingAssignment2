## This pair of functions provides an efficient method to
## store a matrix and its inverse. The inverse is only
## calculated at most once.

## Create a cached matrix object with four functions to
## set and get the matrix and set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y                                ## Store the matrix
    inverse <<- NULL  ## Clear any previously computed result
  }
  get <- function() x                      ## Return the matrix
  setinv <- function(inv) inverse <<- inv  ## Store the inverse
  getinv <- function() inverse             ## Return the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Checks to see if the inverse of the matrix cache object
## has been previously computed and cached, otherwise
## calls solve() to compute and store the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
