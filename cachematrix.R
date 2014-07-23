## These functions create inverse  of a  given matrix and then cache the inverted matrix
## such that when next time inverse of the same matrix is needed, the function gets value
## from cache instead of recaclulating 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatr <- function(solve) m <<- solve
        getinvmatr <- function() m
        list(set = set, get = get,
             setinvmatr = setinvmatr,
             getinvmatr = getinvmatr)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinvmatr()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)    ## Return a matrix that is the inverse of 'x'
        x$setinvmatr(m)
        m
}
