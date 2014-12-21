## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse matrix

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The second function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see
## if the inverse matrix has already been calculated. If so, it gets 
## the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setinverse() function

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
