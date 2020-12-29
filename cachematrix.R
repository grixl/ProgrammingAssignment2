## This file contains 2 functions makeCacheMatrix() and cacheSolve().
## 
## 1. makeCacheMatrix() creates and returns an object of type makeCacheMatrix 
## 2. cacheSolve() computes and returns the inverse of the matrix specified in the 
## object. It also caches the inverse of the matrix in the object so that it
## can be returned from the cache next time the cacheSolve() is called.

## factory function to create object that will cache inverse of a square
## invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse=getinverse)
}


## The cacheSolve() function returns the cached inverse matrix if cache is
## not empty. Otherwise, computes the inverse of the matrix in the passed 
## object, caches it in the object and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}
