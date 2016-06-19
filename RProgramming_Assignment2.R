## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
              x <<- y
              inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
     if(!is.null(inverse)) {
         message("getting cached data")
          return(inverse)
     }
     matrix_assignment <- x$get()
     inverse <- solve(matrix_assignment, ...)
     x$setInverse(inverse)
     inverse
}