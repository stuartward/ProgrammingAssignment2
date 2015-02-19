## Programming Assignment 2

## Matrix inversion is usually a costly computation,
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. The pair of functions below
## cache the inverse of a matrix.

## The function below (makeCacheMatrix) creates a special "matrix" object,
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The function below (cacheSolve) computes the inverse of the special "matrix"
## returned by makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
