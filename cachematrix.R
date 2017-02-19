## Function makeCacheMatrix creates a cacheMatrix object from a matrix that
## stores the matrix and provides a cache for the matrix' inverse.
## Function cacheSolve returns the content of a cacheMatrix object's cache. If
## the cache has not filled yet, the matrix' inverse is explicitly calculated,
## stored the cache and returned.
## Using these functions allows speeding up calculations when the inverse of 
## a matrix may be required several times, without the necessity to explicitly 
## store and pass around the inverse.
## Note: cacheSolve neither checks whether its argument is actually a
## cacheMatrix object nor whether the matrix is invertible.


## Creates a cacheMatrix object that can cache a matrix' inverse 
## in addition to the matrix itself.
## The 'object' is a list with getters and setters 
## for the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) innv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
## In case the inverse was already stored in cache 
## it is returned right from there.
## Otherwise the inverse is explicitly calculated 
## and stored in cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
