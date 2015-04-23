## Put comments here that give an overall description of what your
## functions do

## returns a "matrix" object which cached the inverse of a given matrix argument
## the returned object can be passed into the companion function cacheSolve, which
## returns the inverse from the cache, if it exists, otherwise it computes and caches it. 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getinv <- function() inv
    setinv <- function(i) inv <<- i
    list(get = get, set = set,
         getinv = getinv, setinv = setinv)
}


## returns the inverse of a "matrix" created with the companion function makeCacheMatrix.
## The returned value could come from the cache it it was requested before. Otherwise (i.e. 1st time),
## it computes the inverse of the matrix represented by this object, and caches it for subsequent retrievals.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    m <- x$get
    i <- solve(m)
    x$setinv(i)
    i
}
