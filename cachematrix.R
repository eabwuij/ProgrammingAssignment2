## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This following function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## This following function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      i <- x$getInverse()
      if (!is.null(i)) {
            message("getting cached matrix")
            return(i)
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setInverse(i)
      i
}

