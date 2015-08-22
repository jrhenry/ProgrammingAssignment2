## makeCacheMatrix will create the inverse of a matrix and ## store it in cache. 
## cacheSolve will check if the inverse matrix is in cache. If it is it will
## retrieve it, if not it will create it. Either way, it returns the inverse 
## matrix.

## The makeCacheMatrix function does four things: a) set the value of the matrix, b) get the value 
## of the matrix, c) set the value of the inverse matrix, and d) get the value 
## of the inverse matrix.This function assumes the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## The cacheSolve function will first look if the invere matrix exists in the 
## cache and retrieve it. If it does not, it will execute solve to find the 
## inverse and then return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
