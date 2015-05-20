## This file contains two functions. First: makeCacheMatrix which creates a matrix with a functionality to store cached inverse.
## Egzample usage:
## Given a matrix named 'matrix' we can create a matrix with casched inverse like this:
## cachedMatrix <- makeCacheMatrix(matrix)
##
## Second function cacheSolve for a given matrix returns its inverse. 
## If the matrix had a previously cached inverse, it is not calculated again
## and the cached value is returned. Egzample usage:
## inverseMatrix <- cacheSolve(cachedMatrix)

## A function to create a matrix with a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function calculates an inverse of a matrix passed as a parameter x.
## This function uses a cached value for an inverse if it is available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
