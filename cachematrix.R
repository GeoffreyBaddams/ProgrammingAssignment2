## These functions are for creating and storing a matrix and its 
## inverse so that the inverse need not be computed more than once.


## makeCacheMatrix creates a list with functions to set a matrix,
## return (get) the matrix, set the inverse of the matrix and
## return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## cacheSolve checks if an inverse exists and if so, returns it
## if not, it creates an inverse of the data and sets it as the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i #inverse of x
}