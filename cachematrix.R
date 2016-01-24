## Cache the inverse of a matrix 

## This function creates a special "matrix" object that can cache its inverse

## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" created by
## the function above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
        
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
