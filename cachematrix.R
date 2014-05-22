## The following two functions are used to cache an inverse of a matrix

## This function creates a matrix object that can be cached
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cached is retured.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
