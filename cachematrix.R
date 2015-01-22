## this code allows us to compute a matrix inverse with cache

## makeCacheMatrix - This function creates a special "matrix" 
##  object that can cache its inverse.

## note: assumes matrix is always invertible
## note: code based on "makeVector" example code.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) x_inv <<- solve
        getinv <- function() x_inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}

## cacheSolve - This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

## note: based on "cachemean" code example

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        x_inv
}


