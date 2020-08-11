## Solution R Assignment Week 3
## Caching the Inverse of a Matrix
## The following function creates a special "matrix" object 
## It can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      invMtx <- NULL
      set <- function(y) {
            x <<- y
            invMtx <<- NULL
      }
      get <- function() x
      setInvMtx <- function(inv) invMtx <<- inv
      getInvMtx <- function() invMtx
      list(set = set,
           get = get,
           setInvMtx = setInvMtx,
           getInvMtx = getInvMtx)
}


## The following funtion computes the inverse of the special "matrix"  
## makeCacheMatrix is the function to create the matrix.
## If the inverse has already been calculated (and the matrix has not changed)
## then it is obtained the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invMtx <- x$getInvMtx()
      if (!is.null(invMtx)) {
            message("obtaining data from cache")
            return(invMtx)
      }
      mat <- x$get()
      invMtx <- solve(mat, ...)
      x$setInvMtx(invMtx)
      invMtx
}