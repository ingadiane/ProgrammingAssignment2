## Caching the Inverse of a Matrix
## Creating a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
cacheInv <- NULL
  set <- function(y) {
    x <<- y
    cacheInv<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cacheInv <<- inverse
  getinverse <- function() cacheInv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        cacheInv <- x$getinverse()
  if(!is.null(cacheInv)) {
    message("getting cached data")
    return(cacheInv)
  }
  data <- x$get()
  cacheInv <- solve(data, ...)
  x$setinverse(cacheInv)
cacheInv
}
