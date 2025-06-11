## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse property as NULL
        set <- function(y) {
                x <<- y         # Assign the new matrix
                inv <<- NULL    # Reset the inverse when the matrix changes
        }
        get <- function() x     # Retrieve the matrix
        setinverse <- function(inverse) inv <<- inverse   # Cache the inverse
        getinverse <- function() inv                      # Retrieve the cached inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")  # Inform the user it's using cached data
                return(inv)                     # Return the cached inverse
        }
        data <- x$get()        # Get the matrix
        inv <- solve(data, ...)  # Compute the inverse
        x$setinverse(inv)      # Cache the inverse for future use
        inv                    # Return the inverse
}
