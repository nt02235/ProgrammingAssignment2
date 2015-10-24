# makeCacheMatrix will cache the value of a matrix and its inverse,
# ultimately creating a list of functions for cacheSolve to use
# cacheSolve will call on the functions created by makeCacheMatrix
# to determine whether an inverse is cached and if not, calculate
# the inverse and then cache it

# makeCacheMatrix creates a list of functions when passed a matrix
# The four functions will save (or "set") the matrix, return (or "get")
# the matrix, cache (or "setInv") the inverse of the matrix, and lastly
# return the inverse (or "getInv")
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
}


# cacheSolve will first read the cached inverse.  If NULL it will "get"
# the cached matrix and calculate the inverse, ultimately caching the calculation
# If an inverse is cached, it will return that value and the function ends
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)){
    message("returning cached matrix inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
