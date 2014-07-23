## Put comments here that give an overall description of what your
## functions do

## Creates a special type of matrix object, called a CacheMatrix, 
## which can cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  inverseMatrix <- NULL
  
  set <- function(aMatrix) {
    cacheMatrix <<- aMatrix
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Computes the inverse of a CacheMatrix, which is a matrix returned by makeCacheMatrix.
## If the matrix has not changed since the last call to cacheSolve, a cached copy of
## the inverse matrix will be returned, otherwise the inverse is computed, cached
## and returned.
##
## PreCondition: The matrix, x, is invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # get the inverse
  data <- x$get()
  inv <- solve(data)
  
  x$setInverse(inv)  # cache inv
  
  inv  # returns inv
}
