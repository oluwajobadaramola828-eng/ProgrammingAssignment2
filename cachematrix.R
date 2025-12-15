## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the value of the inverse
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been computed, it retrieves
## the cached version instead of recalculating it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## Return cached inverse if it exists
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, calculate the inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
