## Below are two functions that are used to create a special object 
## that stores a matrix and caches its Inverse.
## this can be useful to save time when using the inverse matrix several times 

## the first function makeCacheMatrix creates a special "matrix" that is really a list 
## containing 4 additional functions used to: 
## 1) set the matrix 2) get the matrix 3) set the inverse 4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverted) Inv <<- inverted
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of the special "Matrix" created with the above function. 
## it initially checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse  of the matrix and sets the value of the Inv variable in the cache 
## via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}
