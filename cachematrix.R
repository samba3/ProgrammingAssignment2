## Functions for creating and caching inverse matrices and using cached inverse matrices
#
## The first function, makeCacheMatrix, creates a cacheable inverse matrix, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

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


## The function, cacheSolve, solves the inverse of the matrix 
#
## If the inverse has already been calculated by makeCacheMatrix() 
## then the cacheSolve() returns the cached inverse matrix 
## If not then the cacheSolve() solves the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        message("solving inverse matrix")
        m

}
 

