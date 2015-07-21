## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      setMatrix <- function(z) {
              x <<- z
              inverse <<- NULL
      }
      getMatrix <- function() x
      setInverse <- function(inverseMat) {
              inverse <<- inverseMat
      }
      getInverse <- function() inverse
      list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data");
                return(inv);
        }
        inv <- x$setInverse(solve(x$getMatrix()), ...);
        inv
}
