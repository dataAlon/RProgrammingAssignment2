#This program takes a matrix and caches it, and its inverse, to memory.


#This function creates a list of functions which allow a matrix to be set and retrieved, and allow the same for the inverse of the matrix. 
#The "set" and "get" functions allow these values to be changed on the fly by the user.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = selibrartinv,
       getinv = getinv)
}
    
#This function computes the inverse of the matrix created by the above function, but first checks if that inverse has already been calculated;
#if so, then the inverse is retrieved from cache and the calculation is skipped. This helps to avoid unnecessary computation. If the inverse
#has not yet been calculated, then the function calculates it and sets this value using the "setinverse" function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
