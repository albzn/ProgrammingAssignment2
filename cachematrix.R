## File:   cachematrix.R
## Author: albzn (albzn@coursera.org)
## Date:   2015/06/10

# Description
# -----------
# This objects encapsulates a matrix and implements a cache of its inverse.
# This way, the inverse of a matric is calcultated only once.
#

# Usage
# -----
# Let M1 and M2 be two matrices.
# > cachedM1 <- makeCacheMatrix(M1)
# > M        <- cacheSolve(cachedM1, ...)   # Inverse of M1 is computed there
# > ...
# > N        <- cacheSolve(cachedM1, ...)   # The matrix has not changed, so
#                                           # the value in cache is taken
# > ...
# > cachedM1$set(M2)    # The matrix is changed, so the cache is emptied there
# > O        <- cacheSolve(cachedM1, ...)   # Here the inserse of M1 is
#                                           # recomputed because matrix changed

###############################################################################
# Function:  makeCacheMatrix
# Usage:     makeCacheMatrix(M)
# Arguments; M - a square numeric or complex matrix
# Returns:   An object of type CachedMatrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)
}

###############################################################################
# Function:  cacheSolve
# Usage:     cacheSolve(cachedM, ...)
# Arguments; cachedM - an object of type CachedMatrix as returned by 
#                      makeCacheMatrix
#            ... - extra arguments to pass to function solve
# Returns:   a square matrix, inverse of the matrix inside cachedM
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()

  # If 'inv' is NULL, then we have to compute the inverse and cache it
  if(is.null(inv)) {
    m   <- x$get()
    inv <- solve(m, ...)
    x$setInverse(inv)
  }
  # Return the inverse
  inv
}
