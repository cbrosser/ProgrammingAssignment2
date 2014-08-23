## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix creates a list of four functions used to cache the value of a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## inverse initialized as NULL
        i <- NULL
        
        ## set the value of the matrix and set the inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## display the matrix
        get <- function() x
        
        ## set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        
        ## display the inverse
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve funtion computes and displays the inverse of the matrix returned by makeCacheMatrix. If the inverse
## has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check to see if the inverse has been calculated
        i <- x$getinverse()
        if(!is.null(i)) {
                ## if so, get the inverse from the cache
                message("getting cached data")
                return(i)
        }
        
        ## otherwise, calculate the inverse of the matrix and set the value in the cache via the setinverse function
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        
        ## display the inverse
        i
}
