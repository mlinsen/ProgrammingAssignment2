## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates a list containing functions to
  ## - get and set the value of a matrix
  ## - get and set the inverse of a matrix
  inv <- NULL
  set <- function(y) {
     ## assign values to the objects in the calling environment
      ## which is makeCacheMatrix()
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  ## setiverse sets an object in makeCacheMatrix(), so use the <<- operator
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check if a cached value exists first to speed things up.
  inv <- x$getinverse()
  ## check if a cached value is available
  if(!is.null(inv)) {
      ## available, so we can use the cached value
      message("use the cached data.")
      return(inv)
  }
  ## no cached value available, compute it...
  data <- x$get()
  ## compute the inverse of a matrix
  inv <- solve(data)
  ## set the cached value for future use
  x$setinverse(inv)
  return(inv)  
}
