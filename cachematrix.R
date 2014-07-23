## These functions work together to improve the performance of programs
## that need to perform matrix inversions of matrices whose inversions
## may have been previously computed.

## Creates a special type of matrix, called a CacheMatrix, which can 
## cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

  # initialize
  inverseMatrix <- NULL
  
  # set function which can be used as an alternate input for the matrix
  # to be inverted. It re-initializes the inverseMatrix to NULL so that
  # an older inverted matrix is not returned incorrectly.
  set <- function(aMatrix) {
    x <<- aMatrix
    inverseMatrix <<- NULL
  }
  
  # returns the matrix passed as an argument to makeCacheMatrix
  get <- function() x
  
  # assigns the passed-in matrix to be the inverseMatrix (of x)
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  # returns the inverse matrix
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

  # Retrieve the inverse matrix of x and if it is not NULL
  # return it.
  inv <- x$getInverse()
  if (!is.null(inv)) {
      return(inv)
  }
  
  # If we made it here, then the CacheMatrix, x, has not yet been inverted.
  # Retrieve the original matrix (pre-inverted) and invert it.
  data <- x$get()
  inv <- solve(data)
  
  # Assign the inverted matrix to be the cached one
  x$setInverse(inv)
  
  # returns the inverted matrix
  inv
}
