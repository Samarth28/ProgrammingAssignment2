## The two functions are used to create a special object that stores a matrix and cache's its inverse

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# x - invertible matrix
  i = NULL
  # "<<-" to assign a value to an object in an environment
  
  # set the matrix
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get the matrix
  get = function() x
  
  # set inverse
  seti = function(inverse) i <<- inverse 
  
  # get inverse
  geti = function() i
  
  # input to cachesolve
  list(set=set, get=get, seti=seti, geti=geti)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 # x is makeCacheMatrix output
  
  # inverse of input matrix
  # if inverse is already calculated
  i = x$geti()
  if (!is.null(i)){
    # return cached value
    message("getting cached inverse")
    return(i)
  }
  # calculate inverse otherwise
  m = x$get()
  i = solve(m, ...)
  
  # set value of inverse in cache
  x$seti(i)
  
  return(i)
}
