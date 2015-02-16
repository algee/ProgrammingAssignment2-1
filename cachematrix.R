# general comments:
# -----------------
# These 2 funtions allow one to compute and cache the results of a square matrix inversion.
# The 1st function (makeCacheMatrix) creates the input structure required to support the caching
# The 2nd function (cacheSolve) uses the input structure to either:
#     1) compute the matrix inverse and update the cache if the input changes
#  or 2) fetch the matrix inverse from the cache if the input remains the same
# The superassignment (<<-) operator is used to create the permanent values of the cache


# makeCacheMatrix: 
# ----------------
# Creates a special cache structure.
#
# The special structure holds 2 permanent values:
#    "matrixIn": input matrix
#    "cacheOut": cached inverse matrix
#
# The special structure also contains a list of functions to:
#     1) set the value of the "matrixIn"
#     2) get the value of the "matrixIn"
#     3) set the value of the "cacheOut"
#     4) get the value of the "cacheOut"
#
# Note: the state of the cache is updated only on input changes.

makeCacheMatrix <- function(matrixIn = matrix()) {
  
  cacheOut <- NULL
  
  set <- function(y) {                    # function to write the input
    if (!identical(matrixIn,y)) {         # invalidate the cache only on input changes
      matrixIn <<- y
      cacheOut <<- NULL                  
    }
  }
  
  get <- function() {                     # function to read the input
    matrixIn
  }
  
  setCacheOut <- function(cacheIn) {      # function to write the cache
    cacheOut <<- cacheIn
  }
  
  getCacheOut <- function() {             # function to read the cache
    cacheOut
  }
  
  list(set = set, 
       get = get,
       setCacheOut = setCacheOut,
       getCacheOut = getCacheOut)
}

# cacheSolve: 
# -----------
# Calculates the matrix inverse of the input matrix.
#
# The input uses the structure created with makeCacheMatrix.
#
# The routine operates as follows:
#
#   If the inverse matrix has already been calculated and input has not changed
#     then: 
#       a) the inverse matrix is read from the cache (skipping the computation). 
#     otherwise:
#       a) the inverse matrix is calculated
#       b) the cache is updated

cacheSolve <- function(x, ...) {
  
  matrixInverse <- x$getCacheOut()       # read the cache
  
  if(!is.null(matrixInverse)) {          # if valid cache, then return cached value and exit
    message("getting cached data")
    return(matrixInverse) 
  }
  
  # invalid cache case: 
  data <- x$get()                        # read the current input
  matrixInverse <- solve(data, ...)      # calculate the inverse
  x$setCacheOut(matrixInverse)           # update the cache
  
  matrixInverse                          # return the inverse
}








