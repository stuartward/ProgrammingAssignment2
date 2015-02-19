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
##then the cachesolve should retrieve the inverse from the cache.

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


# # Code to test the functions
# 
# # Create test matrix
# b <- matrix(c(2, 4, 5, 7), nrow=2, ncol=2)
# 
# # View test matrix
# b
# 
# # Take the inverse of the text matrix using the solve function
# solve(b)
# 
# # Set x equal to results of makeCacheMatrix; passing in test matrix
# x <- makeCacheMatrix(matrix(c(2, 4, 5, 7), nrow=2, ncol=2))
# 
# # First time calling cacheSolve; returns the inverse
# cacheSolve(x)
# 
# # Second time calling cacheSolve; returns the inverse from cache
# cacheSolve(x)