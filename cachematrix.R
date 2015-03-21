## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## The lines below initializes the value of the cache to NULL
        ## and contructs the function "set" with the purpose of
        ## reset m to null and pass the cached x value to the
        ## passed in matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        ## The lines below lists out the values of the functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}  

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated 
## (and the matrix has not changed), then cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## The lines below evaluates if there is data in the cache 
        ## If TRUE, the data in the cache is returned
        m <- x$getsolve() 
        if(!is.null(m)) { 
                message("getting cached data")
                return(m) 
        }
        ## The lines below calculates the inverse of the matrix,
        ## sets the data to the cache and returns the inversed matrix
        data <- x$get() 
        m <- solve(data, ...) #
        x$setsolve(m)
        m
}        



