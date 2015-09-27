## The first function, makeCacheMatrix creates a matrix, 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        # Initialze matrix 
        m <- NULL
        set <- function(y) {
	       # Set x and m to values from dfferent/calling environment. 
               x <<- y
               m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
	# Create a list containing the above functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function cacheSolve takes a matrix and calculates the 
## inverse of the given matrix and returns the inverse matrix.
cacheSolve <- function(x, ...) {
        # see if given matrix already exists in env by calling getinv
	# function in makeCacheMatrix. This will return a null first time.
        m <- x$getinv()
        # if exists/not NULL, fetch from cache (return matrix).
        if(!is.null(m)) {
                message("Fetching cached data")
                return(m)
        }
        # First time , assign given matrix to data 
        data <- x$get()
        # Compute inverse of data (matrix) and asign it to m
        m <- solve(data, ...)
        # Store result set/Cache the result
	# using setinv function in makeCacheMatrix to cache this matrix
        x$setinv(m)
	# Return the matrix
        m
}
