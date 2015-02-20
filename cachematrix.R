## These functions demonstrate caching of objects.
## The makeCacheMatrix creates a matrix object that allows subsequent functions to check if a cached version of the object is available.
## The cacheSolve function computes the inverse of the matrix, or returns the cached value.
## USAGE:  matrix1 <- makeCacheMatrix(matrix(rexp(400, rate=.1), ncol=20))
##         cacheSolve(matrix1)  #This will compute the inverse
##         cacheSolve(matrix1)  #This will return the cached value


## makeCacheMatrix takes a matrix as an argument.
## The set method will cache the matrix.
## The get method will return the matrix, pulling from the cache.
## The setinverse function will cache the inverse from the solve function.
## The getinverse function will return the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a function as an argument.  
## It executes that function's getinverse method. If the result is cached, it returns the cached value 
## along with a message stating that the result was cached.  Otherwise, it computes the inverse and caches 
## the result using the setinverse method of the function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        message("caching result")
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
