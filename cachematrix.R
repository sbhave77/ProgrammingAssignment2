## Put comments here that give an overall description of what your
## functions do
# Both these functions used in conjunction with each other allow for storage of the inverse
# of a matrix so as to prevent recaclulating over and over again. 

## Write a short comment describing this function
## makeCacheMatrix is analogous to a class in an object oriented programming language like java.
# It takes in a matrix and stores that matrix in a setMatrix attribute which is also a function.
# It also sets the attribute inverse to null and stores the inverse when setInverse is called.
# It returns a list of functions with stored variables.

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
# cacheSolve takes in a special cacheMatrix "object" (list) that was made using the previous function
# It gets the inverse. if the inverse is already stored, then it uses that inverse, otherwise it
# calculates the inverse and sets the inverse using the function setInverse.

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
