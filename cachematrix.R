## These functions will create a a special object from a matrix to
## cache its matrix inverse once computed.

## makeCacheMatrix will create a special object from a matrix with setter 
## and getter features, storing the inverse once computed.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will access the object created from makeCacheMatrix() and will
## get the stored inverse, if null it will compute it, otherwise displays it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
