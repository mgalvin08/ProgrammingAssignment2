## This file contains functions that will compute the inverse of a Matrix
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
##  set the value of the matrix
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
}
##  get the value of the matrix
    get <- function() x
##  set the value of the inverse matrix
    setInvMatrix <- function(inverse) m <<- inverse
##  get the value of the inverse matrix
    getInvMatrix <- function() m
    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInvMatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
      
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMatrix(m)
    return(m)
}
