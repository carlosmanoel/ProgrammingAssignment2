## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix is a function that creates a chached matrix that store 
# values for normal and inverse matrix.
# cacheSolve uses makeCacheMatrix for calculate and stores the matrix inverse  


## Write a short comment describing this function
# makeCacheMatrix is a function that creates a cached matrix. 
# This is a list with methods acessor for matrix normal and inverse.
# * set --> sets a matrix on cache
# * get --> retrieves a matrix on cache
# * setInverse --> sets a inverse matrix on cache
# * getInverse --> retrieves a inverse matrix on cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrixParam) {
    x <<- matrixParam
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    inverse <<- inverse
  }
  getInverse <- function() {
    inverse
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(is.null(inverse) == FALSE) {
    message("getting cached data")
    return(inverse)
  }
  matrixSquare <- x$get()
  inverse <- solve(matrixSquare, ...)
  x$setInverse(inverse)
  inverse
}
