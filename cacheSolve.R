# makeCacheMatrix function has four functions to create a special "matrix" 
# object (actually a list is returned with the required functions)
# that caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(invers) inv <<- invers
  getinverse <- function() inv
  list(set = setmatrix, get = getmatrix,
       setinv = setinverse,
       getinv = getinverse)
}


# cacheSolve function returns the inverse of the special "matrix" object returned
# by the function above. If the inverse for matrix has been calculated beforehand
# the cashed data is returned else the matrix inverse is computed, stored for later
# use and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  data <- x$get()
        ##assuming that given matrix is always a square invertible matrix
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
