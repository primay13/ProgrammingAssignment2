## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Find inverse of matrix either from cache or if its a new matrix, calculate its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # if inverse has previously been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #otherwise, calculate inverse of matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
