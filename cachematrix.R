## This function takes a matrix and calculates the inverse of the given matrix
makeCacheMatrix <- function(x = matrix()) {
        # Initialze matrix 
        m <- NULL
        set <- function(y) {
               x <<- y
               m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # Get matrix from env
        m <- x$getinv()
        # if exists/not NULL, fetch from cache (return matrix).
        if(!is.null(m)) {
                message("Fetching cached data")
                return(m)
        }
        # First time , assign given matrix 
        data <- x$get()
        # Compute inverse of matrix
        m <- solve(data, ...)
        # Store result set using setinv
        x$setinv(m)
        m
}
