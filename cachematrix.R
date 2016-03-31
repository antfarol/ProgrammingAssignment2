## This pare of functions calculate and store in 
## cache inverted matrix for the given one.

## makeCacheMatrix actually stores the list of four inserted functions. 

## The first one – setM – assigns the given matrix (argument of our function 
## makeCacheMatrix) with the variable in cache. 
## Also it assigns NULL with inverted matrix stored in cache, 
## as inverted matrix for the new one is still unknown.

## The second function in the list – getM – returns the matrix given as argument.

## Next one – setInv – using a variable invertion assigns inverted matrix 
## to the value stored in cache inv. inversion is supposed to be calculated 
## in the upcoming function cacheSolve

## The last one function in the list – getInv – returns stored value of 
## inverted matrix
## The last line in function makeCacheMatrix arranges everything as a list.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setM <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getM <- function() x
        setInv <- function(invertion = matrix()) inv <<- invertion
        getInv <- function() inv
        list (setM = setM, getM = getM, setInv = setInv, getInv = getInv)
}


## This function calculates or extract from the cache inverted matrix. 

## In the first line we assign value stored in cache with variable inv. 
## If we store something and the condition under “if” is true we return 
## this value. 

## If there is no value for inverted matrix in cache 
## (see setM – “inv <<- NULL”) we get the matrix given as argument and 
## calculate inverted one. 

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message ("getting cached data")
                return(inv)
        }
        data <- x$getM()
        inv <- solve(data, ...)
        x$setInv(inv)
        
## Then we send result as argument for setInv 
## to store this inverted matrix in cache. 
}
