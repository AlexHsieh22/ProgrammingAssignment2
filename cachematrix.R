## The makeCacheMatrix and cacheSolve functions will
## work together to solve the inverse of matrices.
## Because calculating the inverse of a matrix is 
## time consuming, if it has already been calculated
## once, we will return the cached inverse instead
## of recalculating it every time.

## This function will take in an Invertible matrix
## and convert it into a "cacheable matrix" which
## can then be used with teh cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse variable
  inverse <- NULL
  
  set <- function(y) {
    x <<- y          #save the value of the matrix
    
    #set the value of inverse to null when set is called to reinitialize
    inverse <<- NULL
  }
  
  get <- function() x #return the matrix
  
  #save the inverseMatrix
  setInverse <- function(inversedMatrix) inverse <<- inversedMatrix
  
  getInverse <- function() inverse #return the inverse matrix
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Taking in a "CacheMatrix" object, we will check if
## the inverse is cached, if it is, we will return it
## if not, we will calculate it and cache the value
## in case we are asked for the inverse again
cacheSolve <- function(x, ...) {
  #check if the Inverse is cached
  m <- x$getInverse()
  
  if(!is.null(m)) { #We have a cached value for the inverse!
    message("returning cached inverse")
    return(m)
  }
  
  #since we haven't calculated the inverse before, calculate it
  m <- solve(x$get())
  
  #save the inverse in the cache
  x$setInverse(m)
  
  #return the inverse
  m
}
