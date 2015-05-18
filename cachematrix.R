## Put comments here that give an overall description of what your
## functions do

## This function returns a list of functions:
## set  set the main matrix
## get  get the main matrix
## setinverse set the inverted matrix
## getinverse get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## inicialize the inverted matrix as NULL
  inv <- NULL
  
  ## set the main matrix and resets the inverted one
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
    
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse matrix using the makeCacheMatrix list of functions.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
