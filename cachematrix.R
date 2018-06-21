## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than 
## computing it reapeatedly. The following functions will store a 
## matrix and cache its inverse.


## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function will return the inverse of the matrix. It will first
## check if the inverse has already been computed and cached. If this
## is the case, it will skip the computation. If it has not already 
## been computed, then it will compute the inverse and cache the value.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
