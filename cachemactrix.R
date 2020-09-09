## makeCacheMatrix will generate a matrix and cache its inverse, while cacheSolve checks to see if the inverse has already been calculated.
## If it has it will pull it from the cache, and if not it will calculate the inverse.

## This function generates the starting matrix, creates the cache, and defines the get, setinverse, and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function checks to see if the inverse has been calculated already. If it has, it will return the inverse from the cache with a message. If it hasn't 
## it will calculate the inverse and use the setinverse function from the makeCacheMatrix environment to load the inverse of the matrix to the environment.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
