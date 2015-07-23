# R Programming Week 3 assignment.
# Submitted Thu, 23 JUl 2015 (trb)


# Function makeCacheMatrix creates a list of four functions:
#     'set' stores the input matrix value and nulls the cached inverse value
#     'get' returns the input matrix value
#     'setinverse' caches the matrix inverse calculated by cacheSolve function
#     'getinverse' retrieves the cached matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x

        setinverse <- function( inverse) i <<- inverse
        getinverse <- function() i

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Function cacheSolve returns the inverse of the matrix stored by the makeCacheMatrix 'set' function.
#     If the cached value for the matrix inverse is NULL, the matrix is solved for its inverse,
#         and the inverse is stored in the cache.
#     If cacheSolve is called again for the same matrix input, the inverse is returned from the cache.
#     Matrix inversion is not done again until the input changes.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
