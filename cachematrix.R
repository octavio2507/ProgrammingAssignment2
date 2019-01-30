## makeCacheMatrix creates a cache matrix (as the name implies) that can be recovered 
## depending on the conditions met on cacheSolve

## Set a Cache matrix to be used further with the cacheSolve function
## This function will not work by itself

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Use the function to solve for the inverse of an object created with the 
## previous function

cacheSolve <- function(x, ...) {
  inverse <- x$getsolve()
  #Use the cache matrix if there is no other inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  #Return the inverse matrix
  inverse
}

## Example: 
x <- matrix(c(1,5,9,2), 2, 2)

CM <- makeCacheMatrix(x)

InvMatrix <- cacheSolve(CM)




