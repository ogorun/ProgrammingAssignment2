## Systen of two functions
## providing possibility to inverse matrix in cachable way

## Function makeCacheMatrix creates object with possibility to store
## cache of inversed matrix and provide its value
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinversed <- function(inversed) inversed <<- inversed
  getinversed <- function() inversed
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)
}


## Function cacheSolve retrieves cached version of inversed matrix.
## It also calculates and stores value to cache in case it is not available
cacheSolve <- function(x, ...) {
  inversed <- x$getinversed()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setinversed(inversed)
  inversed
}
