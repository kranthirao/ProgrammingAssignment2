## This function takes a matrix and calculates the inverse of the given matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set <- function(y) {
        #       x <<- y
        #       m <<- NULL
        #}
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(get = get,setinv = setinv,getinv = getinv)
}


## This function Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # print ("in cS")
        m <- x$getinv()
        if(!is.null(m)) {
                message("Fetching cached data")
                return(m)
        }
        data <- x$get()
        print ("first time in cacheSolve...")
        print (data)
        m <- solve(data, ...)
        print ("Storing result set using setinv...")
        x$setinv(m)
        m
}
