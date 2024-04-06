## Assignment # 2: Caching the Inverse of a Matrix
## Required: 
## I. makeCacheMatrix (function)
## II. cacheSolve (function)
## Assumptions: 
## a. The matrix supplied is always an invertible square matrix
## b. Program is based on the example provided in the instructions

## The "makeCacheMatrix" function creates a special matrix object that can also store its inverse --
## as cached data. The function returns a list containing multiple functions to access the data.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        getMatrix <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i

        list(getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## The "cacheSolve" function takes the special matrix object, returned by the "makeCacheMatrix" --
## function above, as input. The function checks first if the inverse matrix object exist as cached.
## If it exists, the inverse is retrieved. Otherwise, this function solves for the inverse and cached it afterwards. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverse(i)
        return(i)
}
