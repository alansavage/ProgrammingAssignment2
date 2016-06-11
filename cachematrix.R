## Functions to cache the inverse of a square invertible matrix.
##
## makeCacheMatrix
##
## This function creates defines several functions to maintain a simple cache
## instance for a given matrix. The functions include:
##   set - to set a new matrix into the cache. Sets any previously cached inverse
##         value to null.
##   get - to return the matrix currently held by the cache, or null if no matrix
##        has been set
##   setinverse - sets the computed inverse for the matrix previously cached by 
##       the set function
##   getinverse - to return the inverse that is currently cached, or null if the
##       inverse has not been set
##   
## cacheSolve
##
##   Returns the inverse of a matrix previously saved in a makeCacheMatrix instance.
##   If the inverse was previously computed the inverse value is returned from
##   the cache, otherwise the inverse is calculated, stored in the cache and then
##   returned.
##
## Example usage:
##  myMatrix <- matrix(c(9, 10, 20, 21), 2, 2)
##  cachedMatrix <- makeCacheMatrix(myMatrix)
##  cacheSolve(cachedMatrix)
##  cacheSolve(cachedMatrix)


## Defines the functions to maintain a cached instance of a given matrix
## and its computed inverse.
##
## Args:
##   x - must be a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    # x is the cached matrix
    x <<- y
    # cached inverse must be re-calculated since the matrix has changed
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
##
## Using a matrix saved in a makeCacheMatrix instance, returns the inverse of
## that matrix. The inverse is cached so that the inverse is computed only once.
##
## Args:
##   x - a makeCacheMatrix instance holding a previously cached matrix. 
##       This argument will be updated to cache the inverse of the matrix unless
##       the inverse was previously cached.
##  ... - any addition arguments that should be passed to the solve() function
##       used to compute the matrix inverse
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse) ) {
    message("getting cached matrix inverse")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}