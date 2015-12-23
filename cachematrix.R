## These two functions take a given matrix (that we are assuming is invertible)
## and caches both the original matrix and its solution. The additional functions
## that are stored within the parent functions allow for a user to save
## computing time by retrieving stored/cached data, rather than performing
## the computations again.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(matrix) m <<- matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The above function caches the given matrix and creates functions that are called
## within the "cacheSolve" function found below. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## The above function serves two purposes. The first is to retrieve and return
## a stored solution to the given matrix, if one exists, and provide a prompt
## stating as such. The second is to calculate a solution, if one has not already 
## been cached, and return said solution.