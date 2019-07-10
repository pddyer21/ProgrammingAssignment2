## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() takes an invertible matrix as its argument
## and creates a special matrix that can cache its inverse.
## cacheSolve() computes the inverse of the special matrix returned by makeCacheMatrix()
## checks if the inverse is already cached, and returns it.

## Write a short comment describing this function

## The makeCacheMatrix() function creates a special matrix object that can cache
## its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                  x <<- y
                  inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## The cacheSolve() function calculates the inverse of the matrix returned by
## the makeCacheMatrix() function.If the inverse was already caculated and stored
## in the cache, cacheSolve() will retrieve the inverse from the cache else 
## the inverse is computed and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
