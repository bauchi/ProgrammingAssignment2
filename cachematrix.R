## makeCacheMatrix creates a special matrix object, which is a list containing the
## functions: 
## set: set the value of the matrix, 
## get: get the value of the matrix, 
## setinverse: set the value of the inverse
## getinverse: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special matrix created in makeCacheMatrix
## it first ckecks if the inverse has already been calculated. If so, it skips the calculation 
## and returns the inverse, else it calculates the inverse and sets it in the cache with the 
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
