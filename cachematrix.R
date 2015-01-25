## Two functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # the inverse of the matrix
    inv <- NULL
    
    # set the matrix
    set <- function(inv) {
        x <<- inv
        inv <<- NULL
    }
    
    # get the matrix
    get <- function() {
        x
    } 
    
    # set the inverse
    setinverse <- function(matinverse) {
        inv <<- matinverse
    }
    
    # get the inverse
    getinverse <- function() {
        inv
    }
    
    # return special "matrix" object as a list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # get cached value of matrix inverse
    inv <- x$getinverse()
    
    # if it's not empty, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise calculate inverce matrix:
    # get initial matrix data from the object
    data <- x$get()
    
    # calculate inverse
    inv <- solve(data)
    
    # cache inverse
    x$setinverse(inv)
    
    # return the inverse matrix
    inv
}
