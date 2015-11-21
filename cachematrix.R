## This function will take a Matrix as a variable and comupute the inverse
## if the inverse has been computed, the function will retrieve the value from cache rather than computing again

## makeCacheMatrix is a function that takes a square matrix as variable and creates a list of 4 items.
## the 4 items are each a different function that allow to manipulate the matrix

makeCacheMatrix <- function (x= matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve is a function that uses the list of 4 functions from above as variables 
## it will compute the inverse of the matrix or it caches that value if it was already computed

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)   ## return function stops acts a break function at this point
  }
  data <- x$get()           ## if the inverse has never been computed for this matrix, those 
  inv <- solve(data, ...)   ## three lines of code will compute and save the value in the global envir.
  x$setinverse(inv)
  inv
}