## Take a matrix as input, then calculate its inverse and store it, so that if the 
## functions are called again on the same input, they can return the stored inverse
## without recalculating it.

## Takes a matrix as input, and creates a list of 4 functions which return the matrix
## or its inverse, or set the matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) i <<- inv
  get_inv <- function() i
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Takes the result of the makeCacheMatrix function as input, and returns the inverse
## of the matrix. Then stores the inverse, so if it's called again on the same input, 
## it will immediately return the stored input without recalculating the inverse.

cacheSolve <- function(x, ...) {
  i <- x$get_inv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inv(i)
  i
}
