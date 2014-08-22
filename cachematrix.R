## Put comments here that give an overall description of what your
## functions do

## This function creates an object which is a list of 4 functions
## (set, get, setinverse, getinverse) dealing with a matrix and its
## inverse. The matrix and its inverse are 'cached'

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns the inverse of the matrix: if it has already
## been calculated and it is the same matrix, 
## it gets the inverse from the cache; otherwise it
## computes, with the function solve, the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
