## This file contains two functions. The function makeCacheMatrix() creates an object to get and set
## a matrix and its inverse in cache. The second function, cacheSolve(), takes an object, created by
## calling makeCacheMatrix(), and returns the inverted matrix from its cache. If the cache is empty,
## cacheSolve() inverts the matrix, saves the inverse in cache and then returns the result

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
          x <<- y
          im <<- NULL
        }
        get <- function() x
        setInverse <- function(theinverse) im <<- theinverse
        getInverse <- function() im
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrievea the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse 
        im <- x$getInverse()
        if(!is.null(im)) {
          message("getting cached data")
          return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setInverse(im)
        im
}
