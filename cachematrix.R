## These functions create inverse of a given matrix and then cache the inverted matrix
## such that when next time inverse of the same matrix is needed, the function gets value
## from cache instead of recaclulating 

##  This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

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
