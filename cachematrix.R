## I wrote a pair of functions that cache the inverse of a matrix.

## 1. Caching the Inverse of a Matrix: 
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) iv <<- inverse
        getInverse <- function() iv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## 2. Computing the Inverse of a Matrix:

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.


cacheInverse <- function(x, ...) {
        iv <- x$getInverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- inverse(data, ...)
        x$setInverse(iv)
        iv
}
