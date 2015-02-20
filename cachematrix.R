## Catching the Inverse of a Matrix, when matrix is invertible

## makeCacheMatrix is a function that creates a special matrix that cache its inverse.
## The process involves setting the matrix, followed by using the "set", "get" and 
## "setinv", "getinv" methods to at the end return a list.

makeCacheMatrix <- function(mymtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mymtx <<- x
    inverse <<- NULL
  }
  get <- function() mymtx
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function that calculates the inverse of the special matrix returned by the 
## function makeCacheMatrix. In case the inverse has already been calculated 
## (and the matrix has not changed), cacheSolve should get the inverse from the cache.
## The process involves getting the inverse of the matrix "mymtx" (above), if not empty then return it
## otherwise proceed to calculate the inverse, cache the result and finally return the inverse.

cacheSolve <- function(mymtx, ...) {
  inverse <- mymtx$getinv()
  if(!is.null(inverse)) {
    message("getting cached data...")
    return(inverse)
  }
  data <- mymtx$get()
  inverse <- solve(data, ...)
  mymtx$setinv(inverse)
  return(inverse)
}
