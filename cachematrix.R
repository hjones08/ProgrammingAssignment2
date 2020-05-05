## The following functions serve to save time when calling the inverse of a matrix.
## By caching the inverse of the marix, we can use the second function to call back
## the inverse if it has already been calculated.

## The first function returns a special matrix as an object which can then chache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  # set inverse
  inv <- NULL
  # set Matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x 
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}

  ###################

  ##The second function computes the inverse of the matrix. It also checks for and retrieves 
  ##the inverse of the matrix if it has already been calculated  before.
  
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if( !is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}

