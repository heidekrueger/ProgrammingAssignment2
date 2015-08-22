## The functions in this file implement a datatype that allows the inverse of a matrix
## to be cached and saved along with the matrix itself.

## Creates an object that can hold a matrix  as well as its inverse,
## once this has been calculated.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of a matrix that is in the makeCacheMatrix format.
## If the object holds a cached inverse, return it, oterwise compute the
## inverse and cache it in the matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) #Assume data is nonsingular! Error handling ommitted
  x$setinv(inv)
  inv
}
