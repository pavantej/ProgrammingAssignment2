## Put comments here that give an overall description of what your
## functions do
## These functions help in reducing the costly computation of inversing a matrix by way of caching it

## Write a short comment describing this function
## The first function below provides or stores 4 functions as below
## 1. setmat() - sets a value to required matrix, and changing the original value if needed
## 2. getmat() - gets a value of required matrix 
## 3. setinvmat() - sets the value to inverse matrix as required
## 4. getinvmat() - gets the value of inverse matrix as required
## In effect this function is an exact replica of the example function of makeVector in the assignment

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL 
    setmat <- function(y){ 
        x <<- y         
        invr <<- NULL    
    }
    getmat <- function()x  
    setinvmat <- function(inverse) invr <<- inverse 
    getinvmat <- function() invr 
    list(setmat = setmat, getmat = getmat, setinvmat = setinvmat, getinvmat = getinvmat) 
}


## Write a short comment describing this function
## Similar to the example of the cachemean, this sexcond function calculates an inverse of matrix if and only if 
## it is not already stored in the cache or memory
## We use the built-in solve function for calculating the inverse instead of reinventing the wheel


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invr <- x$getinvmat() 
    if(!is.null(invr)){
        message("getting cached data")
        return(invr)
    }
    tempdata <- x$getmat()
    invr <- solve(tempdata,...)
    x$setinvmat(invr)
    invr
}
