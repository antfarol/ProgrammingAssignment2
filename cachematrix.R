## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setM <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getM <- function() x
        setInv <- function(invertion) inv <<- invertion
        getInv <- function() inv
        list (setM = setM, getM = getM, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message ("getting cached data")
                return(inv)
        }
        data <- x$getM()
        inv <- solve(data, ...)
        x$setInv(inv)
        
        ## Return a matrix that is the inverse of 'x' my comments
        ## bla-bla-bla
}
