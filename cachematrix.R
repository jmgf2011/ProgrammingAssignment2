## 2 Functions to cache the inverse of a matrix


## Cration of a matrix object that caches its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    x
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## List the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix created by the method makeCacheMatrix
## If the inverse of the matrix has been calculated and the matrix has not been modified,
## the method cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return the inverse of x
  inversa <- x$getInverse()
  
  ## Return the inverse when it is already set
  if( !is.null(m) ) {
    message("Getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Computation of the inverse
  inversa <- solve(data) %*% data
  
  ## Set the inverse
  x$setInverse(m)
  
  ## Matrix returned
  inversa
}