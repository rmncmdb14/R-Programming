
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## get matrix x
  setInverse <- function(inverse) inv <<- inverse ## set the inverse matrix
  getInverse <- function() inv ## get the inverse matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) ## create list with functions for inverse matrix calculations
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ## check if matrix has an inverse calculated
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return (inv) ## Return inverse matrix if inv value is not NULL == inverse matrix has been calculated already
  }
  
  data <- x$get() ## get cached matrix x
  inv <- solve(data) ## solve the inverse of matrix x with the solve function
  x$setInverse(inv) ## set x as new inverse matrix
  return (inv) ## Return a matrix that is the inverse of 'x'
}
