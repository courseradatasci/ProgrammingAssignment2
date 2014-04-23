## Functions for constructing a special 'matrix' object
## and for calculating and caching the inverse of it

## This function constructs a special "matrix"
## object and returns a list of functions for
## getting and setting the object's data

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## This function computes the inverse of the object
## returned by makeCacheMatrix. If the inverse has
## already been calculated (and the matrix unchanged),
## then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## Example
##
## c = rbind(c(1, -1/4), c(-1/4, 1))
## cacheMatrix <- makeCacheMatrix(c)
## inv <- cacheSolve(cacheMatrix)