## This group of functions is used to create, store and retrieve
## a precalculated inverse matrix to save time in future computions

## makeCacheMatrix function creates a list of four functions 
##                 to manage the setting and retreiving of the
##                 matrix and inverse matrix objects

makeCacheMatrix <- function(x = matrix()) {
  cxinv <- NULL
  set <- function(y) {
    x <<- y
    cxinv <<- NULL
  }
  get <- function() x
  setxinverse <- function(xinv) cxinv <<- xinv
  getxinverse <- function() cxinv
  list(set = set, get = get,
       setxinverse = setxinverse,
       getxinverse = getxinverse)
}


## casheSolve function is used to retrieve the cached Matrix inverse 
##            or calculate the inverse and set it, if it does not exist

cacheSolve <- function(x, ...) {
  xinv <- x$getxinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setxinverse(xinv)
  xinv
}
