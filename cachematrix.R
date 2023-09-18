# "makeCacheMatrix" function is used to create a special matrix object with caching capability.
# "cacheSolve" function is used to calculate the inverse matrix of a matrix with caching functionality.

# Create a new special matrix object and initialize the cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Calculate the inverse matrix of a matrix and cache it

cacheSolve <- function(x, ...) {
        # Check if the inverse matrix is already cached, if so, return it directly
        if (!is.null(x$getinverse())) {
                message("Getting cached inverse matrix")
                return(x$getinverse())
        }
        # Otherwise, calculate the inverse matrix
        data <- x$get()
        inverse <- solve(data, ...)
        
        # Cache the inverse matrix
        x$setinverse(inverse)
        
        # Return the inverse matrix
        inverse
}
