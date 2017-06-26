
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        invse <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invse <<- inverse
        getInverse <- function() invse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invse <- x$getInverse()
        if (!is.null(invse)) {
                print("retrieve the inverse from the cache")
                return(invse)
        }
        matx <- x$get()
        invse <- solve(matx, ...)
        x$setInverse(invse)
        invse
}
