## Function creation for getting matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(c) {
    x <<- c
    inv <<- NULL
  }
  get <- function() {x}
  set_inv <- function(inverse) {inv <<- inverse}
  get_inv <- function() {inv}
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Estimating and cache matrix inverse
## Introduction of arguments

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("To get cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
