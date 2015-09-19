## This script is for R.Programming Assignment 2 caching the inverse of a Matrix.
##
## To use this script you will first need to create a square matrix using the 
## makeCacheMatrix function which returns a matix that gets cached
##
## Then that matrix can be inverted with the cacheSolve function.   The function returns the 
## inverted function is it exists, or solves for it using the solve function

## Example Usage
## myMatrix <- matrix(runif(16), 4, 4)             # create random square matrix
## cachedMyMatrix <- makeCacheMatrix(myMatrix)     # create cached verion
## invertedMyMatrix <- cacheSolve(cachedMyMatrix)  # invert it
## invertedMyMatrix2 <- cacheSolve(cachedMyMatrix) # invert it again making sure it prints cached message
## identical(invertedMyMatrix,invertedMyMatrix2)   # should return TRUE
## identical(MyMatrix,invertedMyMatrix2)           # should return FALSE

## Function: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL    # no inverse to start
  set <- function(newmatric)
  {
     x <<- newmatrix
     inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversed) inverse <<- inversed
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  

}

## Function: cacheSolve

## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  
## above. If the inverse has already been calculated (and the matrix has not changed), then  
## cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
