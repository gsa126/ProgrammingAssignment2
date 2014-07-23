makematrix <- function(x = matrix()) {
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

cachematr <- function(x, ...) {
        m <- x$getinvmatr()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatr(m)
        m
}