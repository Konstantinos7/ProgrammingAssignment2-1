## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv_mat <<- inverse
        getinv <- function() inv_mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        dat <- x$get()
        inv_mat <- solve(dat, ...)
        x$setinv(inv_mat)
        inv_mat
        ## Return a matrix that is the inverse of 'x'
}
