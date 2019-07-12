## Put comments here that give an overall description of what your
## functions do


## A function to cache the inverse of a Matrix 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {               ## matrix as an argument for the function 
        inv <- NULL                                       ## use inv as NULL for matrix inverse
        set <- function(y){                               ## set function used to assign new value to
                x <<- y                                   ## parent environment
                inv <<- NULL                              ## reset in to NULL if there is a new value
        }
        get <- function() x                               ## get fucntion to returns value of the matrix argument
        setInverse <- function(solveMatrix) inv <<- solveMatrix    ##  assigns value of inv in parent environment
        getInverse <- function() inv                               ## get in value when called
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed ,
## then cacheSolve will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}
