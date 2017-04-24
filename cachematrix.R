#This program takes a matrix and saves it, and its inverse, to the cache. We assume all matrices to be used are invertible.


#This function creates a list of functions which allow a matrix to be set and retrieved, and allow the same for the inverse of the matrix. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL#m is just an empty matrix/value at this stage.
  set <- function(y) {
    x <<- y#Use of the global assignment operator.
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)#The output of this function is really a list of functions.
}

#This function computes the inverse of the matrix created by the above function, but first checks if that inverse has already been calculated.
#If so, then the inverse is retrieved from cache and the calculation is skipped. This helps to avoid unnecessary computation. 
#If the inverse has not yet been calculated, then the function calculates it and sets this value using the "setinv" function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")#Function is checking to see if inverse has already been calculated.
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)#If inverse not calculated already, then function computes it with "solve()".
  x$setinv(m)#Using "setinv()", the function saves the calculated inverse to cache.
  m
}
