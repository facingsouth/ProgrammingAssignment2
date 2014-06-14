## Put comments here that give an overall description of what your
## functions do

## The function "makeCacheMatrix" creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setreverse <- function(inverse.matrix) inv <<- inverse.matrix
  getreverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function "cacheSolve" calculates the inverse of a matrix. 
## It first checks to see if the inversion has already been calculated.
## If so, it gets the inversion from the cache and skips the computation. 
## Otherwise, it calculates the inversion of the matrix 
## and sets the inversion in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
