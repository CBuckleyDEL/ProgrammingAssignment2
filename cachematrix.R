## A pair of functions that cache the inverse of a matrix


## Creates a cache of the inverse of a matrix
makeCacheMatrix <- function( mx = matrix() ) {
  
  ## Inverse property
  inv <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    mx <<- matrix
    inv <<- NULL
  }
  
  ## Call the matrix
  get <- function() {
    ## Return the matrix
    mx
  }
  
  ## Creates the inverse of a matrix
  sInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  gInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return a list of the methods
  list(s = set, g = get,
       sInverse = setInverse,
       gInverse = getInverse)
}


## Compute the inverse of a matrix. If already calculated, then the "cachesolve" will call the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  mx <- x$gInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(mx) ) {
    message("getting cached data")
    return(mx)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  mx <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(mx)
  
  ## Return the matrix
  mx
} 
