## Put comments here that give an overall description of what your
## functions do

## The function that was first used is the makeCacheMatrix wherein it was 
## followed by four functions to cache its inverse and used the variable "dee" 
## as null.


makeCacheMatrix <- function(x = matrix()) {
  dee <- NULL
  set <- function(y){
    x <<- y
    dee <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {dee <<- inverse}
  getInverse <- function() {dee}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## To compute the inverse in the makeCacheMatrix function, the main function 
## that was used here is the cacheSolve function and it can be seen that I also 
## used "dee" as the variable but it still worked when ran in console.

cacheSolve <- function(x, ...) {
  dee <- x$getInverse()
  if(!is.null(dee)){
    message("getting cached data")
    return(dee)
  }
  mat <- x$get()
  dee <- solve(mat, ...)
  x$setInverse(dee)
  dee      ## Return a matrix that is the inverse of 'x'
}
