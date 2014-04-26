## Assignment: Caching the Inverse of a Matrix
## (Programming Assignment 2 for R Programming on Coursera)
##
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss 
## here). Your assignment is to write a pair of functions that cache the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setInv <- function (inv_mat) inv_x <<- inv_mat
  getInv <- function() inv_x
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInv()
  if(!is.null(inv_x)) {
    message('getting cached inverse matrix')
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setInv(inv_x)
  inv_x
}
