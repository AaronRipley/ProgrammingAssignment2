## These two functions combined allow for a data matrix to be provided, an inverse
## performed, and then the cache of the inverse result for future use.source

## 2 by 2 Test matrix; myMatrix <- matrix(1:4, ncol = 2)
## RUN SEQUENCE
## 1. create matrix (above if test)
## 2. run makeCacheMatrix and store in a variable
## 3. set test data
## 4. run cacheSolve with variable from step 2 as arguement

## This function creates a series of functions that will accept a data matrix,
## cache it, cache its inverse, and retrive them.

makeCacheMatrix <- function(x = matrix()) {
    ## initiate variables
    i <- NULL
    
    ## define set fucntion to recieve a matrix to inverse / cache
    set <- function (matrixData) {
        ## set variable in makeCacheMatrix with matrix supplied in set
        x <<- matrixData
        i <<- NULL
    }
    ## defines a function that  returns the stored matrix
    get <- function() x
    
    ## defines afunction that accepts a value and stores as inverse
    setInverse <- function(inverse) i <<- inverse
    
    ## defines a function that returns the stored inverse
    getInverse <- function() i
    
    ##package up functions in a list  to be returned
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function takes as an argument a list produced by the makeCacheMatrix function
## the function checks to see if the matrix inverse has been computed before and stored.
## if it has, then the stored value is returned.  Otherwise, the inverse is calculated,
## stored, and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## check to see if the inverse has already been completed
    ## if it has return the value
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## calculate the inverse of the matrix, store result, and return result
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
