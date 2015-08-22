## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function does four things: a) set the value of the matrix, b) get the value 
## of the matrix, c) set the value of the inverse matrix, and d) get the value 
## of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}


## The cacheSolve function will first look if the invere matrix exists in the 
## cache and retrieve it. If it does not, it will execute solve to find the 
## inverse and then return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("Getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
