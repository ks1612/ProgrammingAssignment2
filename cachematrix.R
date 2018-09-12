## makeCachematrix function creates a special 'matrix' object that stores a matrix in it

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) a <<- inverse
    getinverse <- function() a
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function finds out the inverse of the matrix after verifying whether the inverse has already
## been calculated or not. If so, it gets the inverse from the cache and skips computatuion

cacheSolve <- function(x, ...) {
    a <- x$getinverse()
    if(!is.null(a)) {
        message("getting cached inverse of matrix")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinverse(a)
    a
}